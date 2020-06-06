//! Parsing PLY files.
//!
//! # Random notes on parsing and the file format
//!
//! - The "specification" talks about "carriage-return terminated lines", but
//!   this is incorrect, as in: all files I could find end their lines with
//!   '\n' (0x0A) and not '\r' (0x0D). This includes the example file linked in
//!   the specification! So in this file we use `parse::whitespace` which just
//!   works with '\n'.
//!
//! # TODO
//!
//! - Maybe accept 'point' as vertex element
//! - Accept 'property list * * vertex_index' for edges (currently only
//!   `vertex0` and `vertex1` are accepted).
//! - Refactor the reading part
//!

use std::{
    cmp::min,
    convert::{TryFrom, TryInto},
    fs::File,
    io,
    path::Path,
};

use byteorder::{ByteOrder, NativeEndian, WriteBytesExt};
use cgmath::{Point3, Vector3};

use crate::{
    prelude::*,
    handle::hsize,
    io::{
        StreamSource, MemSink, Primitive, Error, ErrorKind,
        parse::{self, ParseError, Span, Buffer, ParseBuf, MAX_BUFFER_SIZE},
        util::{IndexHandleMap, debug_fmt_bytes},
    },
    prop::ColorLike,
    util::MeshSizeHint,
};
use super::{
    Encoding,
    raw::{
        ElementDef, PropertyDef, PropertyType, PropIndex, RawElement,
        ScalarType, RawStorage, RawSink, RawListInfo,
        ListLenType, RawOffset, RawPropertyInfo,
        ScalarLen,
    },
};


// ----------------------------------------------------------------------------

// ===========================================================================
// ===== Definition of `Reader`
// ===========================================================================

/// A reader able to read binary and ASCII PLY files. Implements [`StreamSource`].
///
/// You can create a reader with [`Reader::open`] or [`Reader::new`]. Both
/// methods will parse the header of the PLY file. There are a couple of
/// methods to get information about the parsed header, but you usually don't
/// need to use those.
///
/// To actually read body data, there are basically two possibilities: (a) via
/// the high level [`StreamSource`] API (you probably want that), or (b) via
/// the low level [`read_raw`][Reader::read_raw] API (you only need to do that
/// in very special situations).
#[derive(Debug, Clone)]
pub struct Reader<R: io::Read> {
    buf: Buffer<R>,
    comments: Vec<String>,
    encoding: Encoding,
    elements: Vec<ElementDef>,
}

impl Reader<File> {
    /// Tries to open the file specified by the given path and creates a new
    /// `Reader` from that file.
    pub fn open(path: impl AsRef<Path>) -> Result<Self, Error> {
        // We don't need a `BufReader` here, because we will use our internal
        // parse buffer anyway.
        Self::new(File::open(path)?)
    }
}

impl<R: io::Read> Reader<R> {
    /// Creates a new `Reader` from the given `io::Read` instance and parses
    /// the header of the given input.
    ///
    /// If you want to open a file, rather use [`Reader::open`].
    pub fn new(reader: R) -> Result<Self, Error> {
        /// Mini helper function to add the comment in the given line to the
        /// list of comments. Assumes that `buf` is currently at the start of
        /// 'comment'. Consumes the whole line including linebreak.
        fn parse_comment(
            buf: &mut impl ParseBuf,
            comments: &mut Vec<String>,
        ) -> Result<(), Error> {
            parse::line(buf, |buf| {
                buf.take_until(b'\n', |line| {
                    // With `[7..]` we skip the "comment" at the start.
                    comments.push(line.assert_ascii()?[7..].trim_start().to_string());
                    Ok(())
                })
            })
        }

        /// Parses a scalar type delimited by whitespace (whitespace is not
        /// read by this function).
        fn parse_scalar_type(buf: &mut impl ParseBuf) -> Result<ScalarType, Error> {
            buf.take_until(b' ', |word| {
                word.assert_ascii()?
                    .parse::<ScalarType>()
                    .map_err(|e| word.error(e.to_string()).into())
            })
        }

        /// Parses a single word delimited by whitespace or newline.
        fn parse_ident(buf: &mut impl ParseBuf) -> Result<String, Error> {
            buf.take_until(|b| b == b' ' || b == b'\n', |s| {
                s.assert_ascii().map(|s| s.to_string()).map_err(|e| e.into())
            })
        }

        // Wrap reader into parse buffer.
        let mut buf = Buffer::new(reader)?;

        let mut comments = Vec::new();


        // ===== Parse magic number and format line ===========================
        // PLY files always start with `ply\n`. This serves as magic number.
        buf.expect_tag(b"ply\n").map_err(|e| {
            match e.kind() {
                ErrorKind::Parse(_) => ParseError::Custom(
                    "not a valid PLY file (does not start with \"ply\\n\")".into(),
                    Span::new(0, 4),
                ).into(),
                _ => e,
            }
        })?;

        // Read any comment lines that might be here
        while buf.is_next(b"comment")? {
            parse_comment(&mut buf, &mut comments)?;
        }

        // Parse format line. This is required to be before everything else in
        // the header (except the magic number and potential comments).
        let encoding = parse::line(&mut buf, |buf| {
            buf.expect_tag(b"format")?;
            parse::whitespace(buf)?;

            let encoding = buf.take_until(b' ', |line| {
                match line.data {
                    b"ascii" => Ok(Encoding::Ascii),
                    b"binary_little_endian" => Ok(Encoding::BinaryLittleEndian),
                    b"binary_big_endian" => Ok(Encoding::BinaryBigEndian),
                    other => {
                        let len = min(other.len(), 50); // limit size of error string
                        let msg = format!(
                            "expected \"ascii\", \"binary_little_endian\" or \
                                \"binary_big_endian\", found '{}'",
                            debug_fmt_bytes(&other[..len]),
                        );
                        Err(line.error(msg).into())
                    }
                }
            })?;

            parse::whitespace(buf)?;
            buf.expect_tag(b"1.0")?;

            Ok(encoding)
        })?;


        // ===== Parse elements and their properties =========================
        let mut elements: Vec<ElementDef> = Vec::new();

        // Line by line until we reach the end of the header
        while !buf.is_next(b"end_header")? {
            match () {
                () if buf.is_next(b"comment ")? => parse_comment(&mut buf, &mut comments)?,

                // Element definition, e.g. `element vertex 8`
                () if buf.is_next(b"element ")? => {
                    let line_start = buf.offset();

                    buf.consume(b"element".len());
                    parse::whitespace(&mut buf)?;

                    let name = parse_ident(&mut buf)?;
                    parse::whitespace(&mut buf)?;

                    // Make sure there is no other element with the same name
                    if elements.iter().find(|e| e.name == name).is_some() {
                        return Err(ParseError::Custom(
                            format!("duplicate element definition for '{}'", name),
                            Span::new(line_start, line_start +  "element".len()),
                        ).into());
                    }

                    let count = buf.take_until(|b| b == b' ' || b == b'\n', |n| {
                        match n.assert_ascii()?.parse::<u64>() {
                            Ok(v) => Ok(v),
                            Err(e) => {
                                let msg = format!("invalid integer as element count ({})", e);
                                Err(n.error(msg).into())
                            }
                        }
                    })?;

                    elements.push(ElementDef {
                        name,
                        count,
                        property_defs: Vec::new().into(),
                    });

                    parse::line(&mut buf, |buf| parse::opt_whitespace(buf))?;
                }

                // Property definition, e.g. `property float x` or
                // `property list uchar int vertex_index`
                () if buf.is_next(b"property ")? => {
                    let line_start = buf.offset();

                    // Get last element or error if there wasn't a preceeding
                    // `element` line.
                    let elem = elements.last_mut().ok_or_else(|| {
                        buf.spanned_data(b"property".len())
                            .error("property definition without preceding element definition")
                    })?;

                    buf.consume(b"property".len());
                    parse::whitespace(&mut buf)?;

                    if buf.is_next(b"list")? {
                        buf.consume(b"list".len());
                        parse::whitespace(&mut buf)?;

                        let len_type = parse_scalar_type(&mut buf)?;
                        parse::whitespace(&mut buf)?;
                        let scalar_type = parse_scalar_type(&mut buf)?;
                        parse::whitespace(&mut buf)?;
                        let name = parse_ident(&mut buf)?;

                        // We don't allow floating point or signed integer
                        // types to specify the length as they don't make a lot
                        // of sense.
                        let len_type = ListLenType::from_scalar_type(len_type).ok_or_else(|| {
                            let span = Span::new(line_start, buf.offset());
                            let msg = format!(
                                "only unsigned integers can be used to store list lengths \
                                    (property '{}')",
                                name
                            );

                            Error::from(ParseError::Custom(msg, span))
                        })?;

                        let ty = PropertyType::List { len_type, scalar_type };
                        elem.property_defs.push(PropertyDef { name, ty });
                    } else {
                        let ty = PropertyType::Scalar(parse_scalar_type(&mut buf)?);
                        parse::whitespace(&mut buf)?;
                        let name = parse_ident(&mut buf)?;

                        elem.property_defs.push(PropertyDef { name, ty });
                    }

                    parse::line(&mut buf, |buf| parse::opt_whitespace(buf))?;
                }

                // Something else...
                () => {
                    let len = min(buf.len(), 10);
                    let start = buf.spanned_data(len);
                    let msg = format!(
                        "expected line starting with \"comment \", \"element \" or \
                            \"property \", found {}",
                        debug_fmt_bytes(start.data),
                    );

                    return Err(start.error(msg).into());
                }
            }
        }

        // Consume the remaining header
        buf.expect_tag(b"end_header")?;
        parse::opt_whitespace(&mut buf)?;
        parse::linebreak(&mut buf)?;


        Ok(Self { buf, comments, encoding, elements })
    }

    /// Returns the encoding of this PLY file.
    pub fn encoding(&self) -> Encoding {
        self.encoding
    }

    /// Returns all comments in the PLY file (in the order as they appear in
    /// the file header).
    pub fn comments(&self) -> &[String] {
        &self.comments
    }

    /// Returns the raw definitions of all elements in the file (in the order
    /// as they appear in the header).
    pub fn elements(&self) -> &[ElementDef] {
        &self.elements
    }

    /// Reads the whole file into a [`RawStorage`].
    ///
    /// In most cases, you want to use [`read_raw`][Reader::read_raw] instead
    /// of this function as `read_raw` is a streaming version of this function
    /// and requires no temporary storage. `into_raw_storage` is mostly
    /// intended for testing and debugging.
    pub fn into_raw_storage(self) -> Result<RawStorage, Error> {
        let mut out = RawStorage::empty();
        self.read_raw(&mut out)?;
        Ok(out)
    }

    /// Reads the whole file into the given raw sink.
    ///
    /// This is a low level building block that you usually don't want to use
    /// directly. This function is only exposed to give you full control in
    /// rare situation where you might need it. That way you never have to
    /// write your own parser, even if you want to do very exotic things. The
    /// `StreamSource` API is built on top of this function.
    ///
    /// **Note**: this function is *really hard* to use. It is purposefully
    /// very low level. This should be your last resort.
    ///
    ///
    /// # Example
    ///
    /// Given the following PLY file (ASCII for this example):
    ///
    /// ```text
    /// ply
    /// format ascii 1.0
    /// element vertex 3
    /// property float x
    /// property float y
    /// property float z
    /// element face 1
    /// property list uchar uint vertex_indices
    /// end_header
    /// 0 1 2
    /// 3 5 8
    /// 0.1 0.2 0.3
    /// 3 0 1 2
    /// ```
    ///
    /// This would lead to the following calls to the given `RawSink` (in this
    /// order):
    ///
    /// - `element_group_start`: for `vertex` element with three properties
    /// - `element`: first vertex (`0 1 2`)
    /// - `element`: second vertex (`3 5 8`)
    /// - `element`: third vertex (`0.1 0.2 0.3`)
    /// - `element_group_start`: for `face` element with one property
    /// - `element`: the face
    #[inline(never)]
    pub fn read_raw(mut self, sink: &mut impl RawSink) -> Result<(), Error> {
        let buf = &mut self.buf;

        // Iterate through each element group
        for element_def in &self.elements {
            sink.element_group_start(&element_def)?;

            // Create index and half-initialized `elem` from the element
            // definition.
            let index = get_type_lens(element_def);
            let mut elem = RawElement {
                data: Vec::new().into(),
                prop_infos: element_def.property_defs
                    .iter()
                    .map(|def| RawPropertyInfo {
                        offset: 0.into(),
                        ty: def.ty,
                        name: def.name.clone(),
                    })
                    .collect::<Vec<_>>()
                    .into(),
            };

            if self.encoding == Encoding::Ascii {
                // For ASCII, we don't really optimize a bunch. Knowing the
                // offsets of all properties wouldn't help here, because that
                // would still have a variable length in ASCII encoding. We use
                // `read_element_ascii` which parses the values and calculates
                // the offsets at the same time.
                for _ in 0..element_def.count {
                    elem.data.clear();
                    read_element_ascii(buf, &index, &mut elem)?;

                    // Send read properties to the sink.
                    sink.element(&elem)?;
                }
            } else {
                // Try to calculate all offsets already. This will only work if
                // there are no lists properties. If they aren't, we can
                // optimize some stuff.
                //
                // While we're doing this, we can also create a swap table in
                // case we need to swap bytes afterwards. We could do this
                // later, but doing it here is convenient and doesn't hurt a
                // lot.
                let mut len_known = true;
                let mut offset = RawOffset::from(0);
                let mut swaps = Vec::new();
                for (prop_info, type_len) in elem.prop_infos.iter_mut().zip(&index) {
                    prop_info.offset = offset;

                    match type_len {
                        TypeLen::Scalar(len) => {
                            // Populate swap table
                            let idx = offset.0;
                            match *len {
                                ScalarLen::One => {}
                                ScalarLen::Two => {
                                    swaps.push((idx + 0, idx + 1));
                                }
                                ScalarLen::Four => {
                                    swaps.push((idx + 0, idx + 3));
                                    swaps.push((idx + 1, idx + 2));
                                }
                                ScalarLen::Eight => {
                                    swaps.push((idx + 0, idx + 7));
                                    swaps.push((idx + 1, idx + 6));
                                    swaps.push((idx + 2, idx + 5));
                                    swaps.push((idx + 3, idx + 4));
                                }
                            }

                            // Bump offset
                            offset += *len;
                        }
                        TypeLen::List { .. } => {
                            len_known = false;
                            break;
                        }
                    }
                }

                if len_known {
                    // We know the length of one element and have calculated
                    // all offsets! Now we just need to read the actual data
                    // and, in case of swapped endian, swap the properties.
                    let elem_len = offset.as_usize();

                    // Since we know the length, we don't have to care about
                    // vector reallactions. That's why we will create a vector
                    // with correct length once and won't change its capacity
                    // or len afterwards. We just overwrite the data.
                    elem.data = vec![0; elem_len].into();

                    // If the encoding has native endianess, we don't need to
                    // swap anything.
                    if self.encoding == Encoding::binary_native() {
                        swaps.clear();
                    }

                    for _ in 0..element_def.count {
                        // Just read the raw data from the buffer.
                        buf.prepare(elem_len)?;
                        elem.data.copy_from_slice(&buf.raw_buf()[..elem_len]);
                        buf.consume(elem_len);

                        // Swap bytes (if necessary)
                        for &(a, b) in &swaps {
                            elem.data.swap(a as usize, b as usize);
                        }

                        // Send read properties to the sink.
                        sink.element(&elem)?;
                    }
                } else {
                    // Our element contains lists, so we have to calculate the
                    // offsets for every element.
                    //
                    // TODO: maybe skip the last element?
                    //
                    // We just store function pointer to the generic element
                    // reading function.
                    let read_element = match self.encoding {
                        Encoding::BinaryBigEndian => read_element_bbe::<Buffer<R>>,
                        Encoding::BinaryLittleEndian => read_element_ble::<Buffer<R>>,
                        Encoding::Ascii => unreachable!(),
                    };

                    for _ in 0..element_def.count {
                        elem.data.clear();
                        read_element(buf, &index, &mut elem)?;

                        // Send read properties to the sink.
                        sink.element(&elem)?;
                    }
                }
            }
        }

        Ok(())
    }
}

impl<R: io::Read> StreamSource for Reader<R> {
    #[inline(never)]
    fn transfer_to<S: MemSink>(self, sink: &mut S) -> Result<(), Error> {
        // This function is rather complicated. Why? Speed. Hopefully.
        //
        // The the basic idea is the following:
        // - First, do some sanity checks and extract useful information for
        //   properties we want to read (like the position of that property).
        //   This is done directly in this function further below.
        // - Define a `RawSink` that receives the data via `read_raw`
        //   (this is the `HelperSink`).
        // - Prepare everything that we can prepare beforehand: offsets into
        //   raw data buffer and function pointer. Function pointers are the
        //   important bit here: We can match over the type of a property and
        //   use the pointer to the function instantiated with that type. Then
        //   the function pointer just needs to be called and the type doesn't
        //   need to be inspected anymore. All these things are stored in the
        //   `HelperSink`.
        // - Finally, start reading data via `HelperSink`. We also have a
        //   function pointer (`elem_handler`) that points to the function
        //   handling the current element (which in turn will call other
        //   function pointers). This is updated on each `element_group_start`
        //   call.

        /// Some info created while sanity checking the header. This is passed
        /// to `HelperSink` and is further processed there.
        ///
        /// For colors: the bool denotes if an alpha channel present. If not,
        /// the last prop index is garbage.
        struct PropInfo {
            vertex_count: hsize,
            vertex_position: Option<([PropIndex; 3], ScalarType)>,
            vertex_normal: Option<([PropIndex; 3], ScalarType)>,
            vertex_color: Option<([PropIndex; 4], bool)>,

            face_count: Option<hsize>,
            vertex_indices: Option<(PropIndex, ScalarType)>,
            face_normal: Option<([PropIndex; 3], ScalarType)>,
            face_color: Option<([PropIndex; 4], bool)>,

            edge_count: Option<hsize>,
            edge_endpoints: Option<([PropIndex; 2], ScalarType)>,
            edge_color: Option<([PropIndex; 4], bool)>,
        }

        /// Helper type alias for property handler functions.
        type FnPropHandler<S> = fn(&mut S, &RawElement);

        struct HelperSink<'a, S: MemSink> {
            sink: &'a mut S,

            /// This function is called for incoming new raw elements. The
            /// function pointer is updated every time `element_group_start` is
            /// called.
            elem_handler: fn(&mut Self, &RawElement) -> Result<(), Error>,

            // ----- State for the property handlers -----
            // Not always valid! Reading these handles at the wrong time might
            // return useless values.
            curr_vertex_handle: VertexHandle,
            curr_face_handle: FaceHandle,
            curr_edge_handle: EdgeHandle,
            // TODO: we could put the current element here, but we don't want
            // to clone it and via reference brings lifetime problems...

            // ----- State for vertex handling -----
            vertex_handles: IndexHandleMap<VertexHandle>,
            vertex_count: usize,

            /// The position of the x, y and z property in the list of vertex
            /// properties (usually 0, 1, 2)
            vertex_position_idx: [PropIndex; 3],

            /// Function to read the x, y, z properties
            read_vertex_position: FnPropHandler<Self>,

            /// The position of the nx, ny and nz properties in the list of
            /// vertex properties
            vertex_normal_idx: [PropIndex; 3],

            /// Function to read the nx, ny, nz properties
            read_vertex_normal: FnPropHandler<Self>,

            /// The position of the red, green, blue and alpha properties in
            /// the list of vertex properties (alpha value potentially garbage)
            vertex_color_idx: [PropIndex; 4],

            /// Function to read the red, green, blue and alpha properties
            read_vertex_color: FnPropHandler<Self>,


            // ----- State for face handling -----
            face_handles: IndexHandleMap<FaceHandle>,
            face_count: usize,

            /// The position of the `vertex_indices` property in the list of
            /// face properties (usually 0)
            vertex_indices_idx: PropIndex,

            /// Function to read the `vertex_indices` property and add the
            /// face. Returns `Err` if an vertex index is not valid or if the
            /// sink returns an error from `add_face`.
            read_face_vertex_indices:
                fn(&mut Self, &RawElement, &RawListInfo) -> Result<FaceHandle, Error>,

            /// This is a temporary buffer for `read_face_vertex_indices` to
            /// store vertex handles in. It shouldn't be used except inside of
            /// `read_face_vertex_indices`.
            vertex_handles_buffer: Vec<VertexHandle>,


            /// The position of the nx, ny and nz properties in the list of
            /// face properties
            face_normal_idx: [PropIndex; 3],

            /// Function to read the nx, ny, nz properties
            read_face_normal: FnPropHandler<Self>,

            /// The position of the red, green, blue and alpha properties in
            /// the list of face properties (alpha value potentially garbage)
            face_color_idx: [PropIndex; 4],

            /// Function to read the red, green, blue and alpha properties
            read_face_color: FnPropHandler<Self>,


            // ----- State for edge handling -----
            edge_handles: IndexHandleMap<EdgeHandle>,
            edge_count: usize,

            /// The position of the `vertex1` and `vertex2` properties in the list
            /// of edge properties (usually 0 and 1).
            endpoint_indices_idx: [PropIndex; 2],

            /// Function to read the `vertex1` and `vertex2` properties. Returns
            /// the edge handle of the edge between the two vertices, or an
            /// `Err` if none such edge exists.
            read_edge_endpoint_indices: fn(&mut Self, &RawElement) -> Result<EdgeHandle, Error>,

            /// The position of the red, green, blue and alpha properties in
            /// the list of edge properties (alpha value potentially garbage)
            edge_color_idx: [PropIndex; 4],

            /// Function to read the red, green, blue and alpha properties.
            read_edge_color: FnPropHandler<Self>,
        }

        impl<'a, S: MemSink> HelperSink<'a, S> {
            fn new(sink: &'a mut S, info: PropInfo) -> Result<Self, Error> {
                // Calls the `prepare_` function and returns the offset as well
                // as the function pointer to the function monomorphized with
                // the correct type.
                macro_rules! get_offset_and_fptr {
                    ($info:expr, $fun:ident, $prep_fn:ident, $count:expr $(,)?) => {
                        match $info {
                            Some((offset, ty)) => {
                                let fn_ptr = match ty {
                                    ScalarType::Char => {
                                        sink.$prep_fn::<i8>($count)?;
                                        Self::$fun::<i8>
                                    },
                                    ScalarType::UChar => {
                                        sink.$prep_fn::<u8>($count)?;
                                        Self::$fun::<u8>
                                    },
                                    ScalarType::Short => {
                                        sink.$prep_fn::<i16>($count)?;
                                        Self::$fun::<i16>
                                    },
                                    ScalarType::UShort => {
                                        sink.$prep_fn::<u16>($count)?;
                                        Self::$fun::<u16>
                                    },
                                    ScalarType::Int => {
                                        sink.$prep_fn::<i32>($count)?;
                                        Self::$fun::<i32>
                                    },
                                    ScalarType::UInt => {
                                        sink.$prep_fn::<u32>($count)?;
                                        Self::$fun::<u32>
                                    },
                                    ScalarType::Float => {
                                        sink.$prep_fn::<f32>($count)?;
                                        Self::$fun::<f32>
                                    },
                                    ScalarType::Double => {
                                        sink.$prep_fn::<f64>($count)?;
                                        Self::$fun::<f64>
                                    },
                                };

                                (offset, fn_ptr)
                            }
                            None => ([PropIndex(0); 3], Self::noop as FnPropHandler<Self>),
                        }
                    }
                }

                // For property `vertex_indices`
                let (vertex_indices_idx, read_face_vertex_indices) = match info.vertex_indices {
                    Some((offset, ty)) => {
                        let fun = match ty {
                            ScalarType::Char => Self::read_face_vertex_indices::<i8>,
                            ScalarType::UChar => Self::read_face_vertex_indices::<u8>,
                            ScalarType::Short => Self::read_face_vertex_indices::<i16>,
                            ScalarType::UShort => Self::read_face_vertex_indices::<u16>,
                            ScalarType::Int => Self::read_face_vertex_indices::<i32>,
                            ScalarType::UInt => Self::read_face_vertex_indices::<u32>,

                            // We check before that the type is an integer.
                            ScalarType::Float | ScalarType::Double => unreachable!(),
                        };

                        (offset, fun)
                    }
                    None => (
                        PropIndex(0),
                        Self::vertex_indices_bug as fn(&mut _, &_, &_) -> _,
                    ),
                };

                // For vertex properties x, y, z.
                let (vertex_position_idx, read_vertex_position) = get_offset_and_fptr!(
                    info.vertex_position,
                    read_vertex_position,
                    prepare_vertex_positions,
                    info.vertex_count,
                );

                // For vertex properties nx, ny, nz.
                let (vertex_normal_idx, read_vertex_normal) = get_offset_and_fptr!(
                    info.vertex_normal,
                    read_vertex_normal,
                    prepare_vertex_normals,
                    info.vertex_count,
                );

                // For vertex properties red, green, blue, [alpha]
                let (vertex_color_idx, read_vertex_color) = match info.vertex_color {
                    Some((offset, alpha)) => {
                        let fun = match alpha {
                            true => {
                                sink.prepare_vertex_colors::<[u8; 4]>(info.vertex_count)?;
                                Self::read_vertex_color::<[u8; 4]> as FnPropHandler<Self>
                            },
                            false => {
                                sink.prepare_vertex_colors::<[u8; 3]>(info.vertex_count)?;
                                Self::read_vertex_color::<[u8; 3]> as FnPropHandler<Self>
                            },
                        };
                        (offset, fun)
                    }
                    None => ([PropIndex(0); 4], Self::noop as FnPropHandler<Self>),
                };

                // For face properties nx, ny, nz.
                let (face_normal_idx, read_face_normal) = get_offset_and_fptr!(
                    info.face_normal,
                    read_face_normal,
                    prepare_face_normals,
                    info.face_count.unwrap(),
                );

                // For face properties red, green, blue, [alpha]
                let (face_color_idx, read_face_color) = match info.face_color {
                    Some((offset, alpha)) => {
                        let fun = if alpha {
                            sink.prepare_face_colors::<[u8; 4]>(info.face_count.unwrap())?;
                            Self::read_face_color::<[u8; 4]> as FnPropHandler<Self>
                        } else {
                            sink.prepare_face_colors::<[u8; 3]>(info.face_count.unwrap())?;
                            Self::read_face_color::<[u8; 3]> as FnPropHandler<Self>
                        };
                        (offset, fun)
                    }
                    None => ([PropIndex(0); 4], Self::noop as FnPropHandler<Self>),
                };

                // For property `vertex_indices`
                let (endpoint_indices_idx, read_edge_endpoint_indices) = match info.edge_endpoints {
                    Some((offsets, ty)) => {
                        let fun = match ty {
                            ScalarType::Char => Self::read_edge_endpoint_indices::<i8>,
                            ScalarType::UChar => Self::read_edge_endpoint_indices::<u8>,
                            ScalarType::Short => Self::read_edge_endpoint_indices::<i16>,
                            ScalarType::UShort => Self::read_edge_endpoint_indices::<u16>,
                            ScalarType::Int => Self::read_edge_endpoint_indices::<i32>,
                            ScalarType::UInt => Self::read_edge_endpoint_indices::<u32>,

                            // We check before that the type is an integer.
                            ScalarType::Float | ScalarType::Double => unreachable!(),
                        };

                        (offsets, fun)
                    }
                    None => (
                        [PropIndex(0); 2],
                        Self::edge_endpoints_bug as fn(&mut _, &_) -> _,
                    ),
                };

                // For edge properties red, green, blue, [alpha]
                let (edge_color_idx, read_edge_color) = match info.edge_color {
                    Some((offset, alpha)) => {
                        let fun = if alpha {
                            sink.prepare_edge_colors::<[u8; 4]>(info.edge_count.unwrap())?;
                            Self::read_edge_color::<[u8; 4]> as FnPropHandler<Self>
                        } else {
                            sink.prepare_edge_colors::<[u8; 3]>(info.edge_count.unwrap())?;
                            Self::read_edge_color::<[u8; 3]> as FnPropHandler<Self>
                        };
                        (offset, fun)
                    }
                    None => ([PropIndex(0); 4], Self::noop as FnPropHandler<Self>),
                };


                Ok(Self {
                    sink,

                    // It will be overwritten in `element_group_start`, but if
                    // `read_raw` is buggy, this function might be called,
                    // so we will panic in that case.
                    elem_handler: HelperSink::bug_in_read_raw,

                    // We initialize the handles to dummy values. They are
                    // overwritten in `handle_vertex` and `handle_face`.
                    curr_vertex_handle: VertexHandle::from_usize(0),
                    curr_face_handle: FaceHandle::from_usize(0),
                    curr_edge_handle: EdgeHandle::from_usize(0),

                    vertex_handles: IndexHandleMap::new(),
                    vertex_count: 0,

                    vertex_position_idx,
                    read_vertex_position,
                    vertex_normal_idx,
                    read_vertex_normal,
                    vertex_color_idx,
                    read_vertex_color,

                    face_handles: IndexHandleMap::new(),
                    face_count: 0,

                    vertex_indices_idx,
                    read_face_vertex_indices,
                    vertex_handles_buffer: Vec::with_capacity(3),
                    face_normal_idx,
                    read_face_normal,
                    face_color_idx,
                    read_face_color,

                    edge_handles: IndexHandleMap::new(),
                    edge_count: 0,

                    endpoint_indices_idx,
                    read_edge_endpoint_indices,
                    edge_color_idx,
                    read_edge_color,
                })
            }


            // ----- Element handler ------------------------------------------
            /// Initial element handler. Will hopefully never be called.
            fn bug_in_read_raw(&mut self, _: &RawElement) -> Result<(), Error> {
                panic!(
                    "bug in `ply::Reader::read_raw`: `element()` called \
                        before `element_group_start`"
                );
            }

            fn ignore_elem(&mut self, _: &RawElement) -> Result<(), Error> {
                Ok(())
            }

            /// Called for each element in the `vertex` group.
            fn handle_vertex(&mut self, elem: &RawElement) -> Result<(), Error> {
                // Add vertex
                let vh = self.sink.add_vertex();
                self.vertex_handles.add(self.vertex_count, vh);
                self.vertex_count += 1;
                self.curr_vertex_handle = vh;

                // Read vertex properties
                (self.read_vertex_position)(self, elem);
                (self.read_vertex_normal)(self, elem);
                (self.read_vertex_color)(self, elem);

                Ok(())
            }

            /// Called for each element in the `face` group.
            fn handle_face(&mut self, elem: &RawElement) -> Result<(), Error> {
                // We checked before that this property is indeed a list
                let vi_list = elem.decode_list_at(self.vertex_indices_idx).unwrap();

                // Make sure the list is not too short.
                if vi_list.list_len < 3 {
                    return Err(Error::new(|| ErrorKind::InvalidInput(format!(
                        "the face at index {} has only {} vertices, but at least 3 are required \
                            per face",
                        self.face_count,
                        vi_list.list_len,
                    ))));
                }

                // Add face
                let fh = (self.read_face_vertex_indices)(self, &elem, &vi_list)?;
                self.face_handles.add(self.face_count, fh);
                self.face_count += 1;
                self.curr_face_handle = fh;

                // Face properties
                (self.read_face_normal)(self, elem);
                (self.read_face_color)(self, elem);

                Ok(())
            }

            /// Called for each element in the `edge` group.
            fn handle_edge(&mut self, elem: &RawElement) -> Result<(), Error> {
                // Add edge
                let eh = (self.read_edge_endpoint_indices)(self, elem)?;
                self.edge_handles.add(self.edge_count, eh);
                self.edge_count += 1;
                self.curr_edge_handle = eh;

                // Edge properties
                (self.read_edge_color)(self, elem);

                Ok(())
            }

            // ----- Vertex property handler ----------------------------------
            /// Used for all properties that are not in the file: just do
            /// nothing.
            fn noop(&mut self, _: &RawElement) {}

            /// Read the x, y and z property and pass the position to the sink.
            fn read_vertex_position<T: Primitive + FromBytes>(&mut self, elem: &RawElement) {
                let [x_idx, y_idx, z_idx] = self.vertex_position_idx;
                let x = T::from_bytes_ne(&elem.data[elem.prop_infos[x_idx].offset..]);
                let y = T::from_bytes_ne(&elem.data[elem.prop_infos[y_idx].offset..]);
                let z = T::from_bytes_ne(&elem.data[elem.prop_infos[z_idx].offset..]);

                self.sink.set_vertex_position(
                    self.curr_vertex_handle,
                    Point3::new(x, y, z),
                );
            }

            /// Read the x, y and z property and pass the position to the sink.
            fn read_vertex_normal<T: Primitive + FromBytes>(&mut self, elem: &RawElement) {
                let [x_idx, y_idx, z_idx] = self.vertex_normal_idx;
                let x = T::from_bytes_ne(&elem.data[elem.prop_infos[x_idx].offset..]);
                let y = T::from_bytes_ne(&elem.data[elem.prop_infos[y_idx].offset..]);
                let z = T::from_bytes_ne(&elem.data[elem.prop_infos[z_idx].offset..]);

                self.sink.set_vertex_normal(
                    self.curr_vertex_handle,
                    Vector3::new(x, y, z),
                );
            }

            fn read_vertex_color<C: ColorLike<Channel = u8>>(&mut self, elem: &RawElement) {
                let [r_idx, g_idx, b_idx, a_idx] = self.vertex_color_idx;
                let r = elem.data[elem.prop_infos[r_idx].offset];
                let g = elem.data[elem.prop_infos[g_idx].offset];
                let b = elem.data[elem.prop_infos[b_idx].offset];

                if C::HAS_ALPHA {
                    let a = elem.data[elem.prop_infos[a_idx].offset];
                    self.sink.set_vertex_color(self.curr_vertex_handle, [r, g, b, a]);
                } else {
                    self.sink.set_vertex_color(self.curr_vertex_handle, [r, g, b]);
                }
            }

            // ----- face property handler ----------------------------------
            /// Only dummy function that should never be called.
            fn vertex_indices_bug(
                &mut self,
                _: &RawElement,
                _: &RawListInfo,
            ) -> Result<FaceHandle, Error> {
                panic!("internal bug in PLY reader")
            }

            /// Reads three values from the raw data at the specified offset,
            /// interprets them as vertex indices and adds a face to the sink.
            fn read_face_vertex_indices<T: Primitive + FromBytes + PlyInteger>(
                &mut self,
                elem: &RawElement,
                vi_list: &RawListInfo,
            ) -> Result<FaceHandle, Error> {
                // Obtain the relevant raw slice (all the list data).
                let start = vi_list.data_offset;
                let end = start + RawOffset(vi_list.list_len * T::SIZE as u32);
                let data = &elem.data[start..end];

                // Iterate over the list of indices to vertices, convert them
                // to handles and add the handles to `vertex_handles_buffer`.
                self.vertex_handles_buffer.clear();
                for raw in data.chunks(T::SIZE) {
                    let index = T::from_bytes_ne(raw).as_usize();

                    // TODO: the closure should probably be extracted into an
                    // `inline(never)` function that is not generic, to tackle
                    // code bloat.
                    let handle = self.vertex_handles.get(index).ok_or_else(|| {
                        Error::new(|| {
                            let msg = format!(
                                "invalid vertex index {} in PLY file: a face's `vertice_indices` \
                                    property refers to that vertex index, but no vertex with such \
                                    a high index exists in the file",
                                index,
                            );
                            ErrorKind::InvalidInput(msg)
                        })
                    })?;

                    // TODO: we can probably improve performance a bit by
                    // preallocating the vector, setting the len to the list
                    // len and just write the elements. That way, `push`
                    // doesn't have to perform a capacity check each time.
                    // However, this optimization would require `unsafe` code.
                    self.vertex_handles_buffer.push(handle);
                }

                // Add the face to the mesh.
                self.sink.add_face(&self.vertex_handles_buffer)
            }

            /// Read the x, y and z property and pass the position to the sink.
            fn read_face_normal<T: Primitive + FromBytes>(&mut self, elem: &RawElement) {
                let [x_idx, y_idx, z_idx] = self.face_normal_idx;
                let x = T::from_bytes_ne(&elem.data[elem.prop_infos[x_idx].offset..]);
                let y = T::from_bytes_ne(&elem.data[elem.prop_infos[y_idx].offset..]);
                let z = T::from_bytes_ne(&elem.data[elem.prop_infos[z_idx].offset..]);

                self.sink.set_face_normal(
                    self.curr_face_handle,
                    Vector3::new(x, y, z),
                );
            }

            fn read_face_color<C: ColorLike<Channel = u8>>(&mut self, elem: &RawElement) {
                let [r_idx, g_idx, b_idx, a_idx] = self.face_color_idx;
                let r = elem.data[elem.prop_infos[r_idx].offset];
                let g = elem.data[elem.prop_infos[g_idx].offset];
                let b = elem.data[elem.prop_infos[b_idx].offset];

                if C::HAS_ALPHA {
                    let a = elem.data[elem.prop_infos[a_idx].offset];
                    self.sink.set_face_color(self.curr_face_handle, [r, g, b, a]);
                } else {
                    self.sink.set_face_color(self.curr_face_handle, [r, g, b]);
                }
            }


            // ----- edge property handler ----------------------------------
            /// Only dummy function that should never be called.
            fn edge_endpoints_bug(&mut self, _: &RawElement) -> Result<EdgeHandle, Error> {
                panic!("internal bug in PLY reader: no edge endpoints")
            }

            /// Reads three values from the raw data at the specified offset,
            /// interprets them as vertex indices and adds a face to the sink.
            fn read_edge_endpoint_indices<T: Primitive + FromBytes + PlyInteger>(
                &mut self,
                elem: &RawElement,
            ) -> Result<EdgeHandle, Error> {
                let get_handle = |index| {
                    self.vertex_handles.get(index).ok_or_else(|| {
                        Error::new(|| {
                            let msg = format!(
                                "invalid vertex index {} in PLY file: a face's `vertice_indices` \
                                    property refers to that vertex index, but no vertex with such \
                                    a high index exists in the file",
                                index,
                            );
                            ErrorKind::InvalidInput(msg)
                        })
                    })
                };

                let [idx0, idx1] = self.endpoint_indices_idx;
                let vi0 = T::from_bytes_ne(&elem.data[elem.prop_infos[idx0].offset..]);
                let vi1 = T::from_bytes_ne(&elem.data[elem.prop_infos[idx1].offset..]);

                let v0 = get_handle(vi0.as_usize())?;
                let v1 = get_handle(vi1.as_usize())?;

                // Retrieve edge handle
                let edge = self.sink.get_edge_between([v0, v1])?;
                edge.ok_or_else(|| {
                    Error::new(|| {
                        let msg = format!(
                            "invalid edge: PLY file refers to edge between vertex indices {} and \
                                {}, but not such edge exists",
                            vi0.as_usize(),
                            vi1.as_usize(),
                        );
                        ErrorKind::InvalidInput(msg)
                    })
                })
            }

            fn read_edge_color<C: ColorLike<Channel = u8>>(&mut self, elem: &RawElement) {
                let [r_idx, g_idx, b_idx, a_idx] = self.edge_color_idx;
                let r = elem.data[elem.prop_infos[r_idx].offset];
                let g = elem.data[elem.prop_infos[g_idx].offset];
                let b = elem.data[elem.prop_infos[b_idx].offset];

                if C::HAS_ALPHA {
                    let a = elem.data[elem.prop_infos[a_idx].offset];
                    self.sink.set_edge_color(self.curr_edge_handle, [r, g, b, a]);
                } else {
                    self.sink.set_edge_color(self.curr_edge_handle, [r, g, b]);
                }
            }
        }

        impl<S: MemSink> RawSink for HelperSink<'_, S> {
            fn element_group_start(&mut self, def: &ElementDef) -> Result<(), Error> {
                match &*def.name {
                    "vertex" => self.elem_handler = Self::handle_vertex,
                    "face" => self.elem_handler = Self::handle_face,
                    "edge" => self.elem_handler = Self::handle_edge,
                    _ => self.elem_handler = Self::ignore_elem,
                }

                Ok(())
            }

            fn element(&mut self, elem: &RawElement) -> Result<(), Error> {
                (self.elem_handler)(self, elem)
            }
        }


        // ===== Interpret and collect header information (and do checks) =====
        fn u64_to_hsize(v: u64, elem: &str) -> Result<hsize, Error> {
            v.try_into().map_err(|_| Error::new(|| ErrorKind::InvalidInput(
                format!("too many {} (LOX meshes can only contain 2^32 elements, \
                    unless you enabled the 'large-handle' feature)", elem)
            )))
        }

        // Make sure the file contains vertices
        let vertex_pos = self.elements.iter().position(|e| e.name == "vertex").ok_or_else(|| {
            Error::new(|| ErrorKind::InvalidInput("no 'vertex' elements in PLY file".into()))
        })?;
        let vertex_group = &self.elements[vertex_pos];

        let vertex_count = u64_to_hsize(vertex_group.count, "vertices")?;
        let mut prop_info = PropInfo {
            vertex_count,
            vertex_position: vertex_group.check_vec3_prop(["x", "y", "z"], "positions")?,
            vertex_normal: vertex_group.check_vec3_prop(["nx", "ny", "nz"], "normals")?,
            vertex_color: vertex_group.check_color_prop()?,
            face_count: None,
            vertex_indices: None,
            face_normal: None,
            face_color: None,
            edge_count: None,
            edge_endpoints: None,
            edge_color: None,
        };

        // Check faces
        if let Some(face_pos) = self.elements.iter().position(|e| e.name == "face") {
            // Faces can only be in the file after vertices
            if face_pos < vertex_pos {
                return Err(Error::new(|| ErrorKind::InvalidInput(
                    "found 'face' elements before 'vertex' elements (that's not allowed)".into()
                )));
            }

            let face_group = &self.elements[face_pos];
            prop_info.face_count = Some(u64_to_hsize(face_group.count, "faces")?);

            // The property `vertex_indices` is required
            if let Some(vi_idx) = face_group.prop_pos("vertex_indices") {
                let vi = &face_group.property_defs[vi_idx];
                if !vi.ty.is_list() {
                    return Err(Error::new(|| ErrorKind::InvalidInput(
                        "'vertex_indices' property has a scalar type (must be a list)".into()
                    )));
                }

                if vi.ty.scalar_type().is_floating_point() {
                    return Err(Error::new(|| ErrorKind::InvalidInput(
                        "'vertex_indices' list has a floating point element type (only \
                            integers are allowed)".into()
                    )));
                }

                prop_info.vertex_indices = Some((vi_idx, vi.ty.scalar_type()));
            } else {
                return Err(Error::new(|| ErrorKind::InvalidInput(
                    "'face' elements without 'vertex_indices' property".into()
                )));
            }

            // Check other face properties
            prop_info.face_normal = face_group.check_vec3_prop(["nx", "ny", "nz"], "normals")?;
            prop_info.face_color = face_group.check_color_prop()?;
        }

        // Check edges
        if let Some(edge_pos) = self.elements.iter().position(|e| e.name == "edge") {
            // Edges can only be in the file after vertices and faces.
            let face_pos = self.elements.iter().position(|e| e.name == "face");
            if face_pos.is_none() || edge_pos < face_pos.unwrap() {
                let problem = if face_pos.is_none() { "but no" } else { "before" };
                let msg = format!(
                    "found 'edge' elements {} 'face' elements (that's not allowed as \
                        LOX can't add edges on their own; edges always need to be part of a face)",
                    problem,
                );
                return Err(Error::new(|| ErrorKind::InvalidInput(msg)));
            }

            let edge_group = &self.elements[edge_pos];
            prop_info.edge_count = Some(u64_to_hsize(edge_group.count, "edges")?);

            // The properties `vertex1` and `vertex2` are required.
            let v1_idx = edge_group.prop_pos("vertex1");
            let v2_idx = edge_group.prop_pos("vertex2");
            if let (Some(v1_idx), Some(v2_idx)) = (v1_idx, v2_idx) {
                for &(name, idx) in &[("vertex1", v1_idx), ("vertex2", v2_idx)] {
                    let def = &edge_group.property_defs[idx];
                    if def.ty.is_list() {
                        return Err(Error::new(|| ErrorKind::InvalidInput(
                            format!("'{}' property has a list type (must be a scalar)", name)
                        )));
                    }

                    if def.ty.scalar_type().is_floating_point() {
                        return Err(Error::new(|| ErrorKind::InvalidInput(
                            format!("'{}' list has a floating point element type (only \
                                integers are allowed)", name)
                        )));
                    }
                }

                prop_info.edge_endpoints = Some((
                    [v1_idx, v2_idx],
                    edge_group.property_defs[v1_idx].ty.scalar_type(),
                ));
            } else {
                return Err(Error::new(|| ErrorKind::InvalidInput(
                    "'edge' elements without 'vertex1' and 'vertex2' property".into()
                )));
            }

            // Check other edge properties
            prop_info.edge_color = edge_group.check_color_prop()?;
        }
        // ===== Read the data through our helper sink =====
        sink.size_hint(MeshSizeHint {
            vertex_count: Some(prop_info.vertex_count),
            face_count: prop_info.face_count,
        });
        let mut helper_sink = HelperSink::new(sink, prop_info)?;
        self.read_raw(&mut helper_sink)
    }
}


// ===========================================================================
// ===== Body parsing: `read_element_*` implementations
// ===========================================================================

// All three `read_element_*` functions have a specific function contract in
// particular regarding the passed `raw_elem`:
//
// - `raw_elem.data` is always cleared when `read_elem` is called. It has to be
//   filled with the native-endian, packed representation of all property data
//   (in the same order as the property are specified in the file header).
// - The length of the vector `raw_elem.prop_infos` must not be modified.
//   Furthermore, regarding its elements:
//     - The `info.offset` values are unspecified when `read_element_*` is
//       called. All values must be overwritten with the correct offset value
//       by `read_element_*`.
//     - `info.ty` and `info.name` must not be modified by `read_element_*`,
//       but can be observed.
//
// This is not a very nice API, but this way we can avoid some moving data
// around in other functions. Gotta go fast.

fn read_element_bbe<P: ParseBuf>(
    buf: &mut P,
    index: &[TypeLen],
    raw_elem: &mut RawElement,
) -> Result<(), Error> {
    #[cfg(target_endian = "big")]
    {
        read_element_binary_native(buf, index, raw_elem)
    }

    #[cfg(target_endian = "little")]
    {
        read_element_binary_swapped(buf, index, raw_elem)
    }
}

fn read_element_ble<P: ParseBuf>(
    buf: &mut P,
    index: &[TypeLen],
    raw_elem: &mut RawElement,
) -> Result<(), Error> {
    #[cfg(target_endian = "big")]
    {
        read_element_binary_swapped(buf, index, raw_elem)
    }

    #[cfg(target_endian = "little")]
    {
        read_element_binary_native(buf, index, raw_elem)
    }
}

/// Reads `count` bytes from `buf` into `out`.
#[inline(always)]
fn read_bytes_into(
    buf: &mut impl ParseBuf,
    count: usize,
    out: &mut Vec<u8>,
) -> Result<(), Error> {
    buf.with_bytes(count, |b| {
        out.extend_from_slice(&b.data);
        Ok(())
    })
}

/// Reads the length of a list (as type `ty`) from `buf`. The length is
/// returned as `u32` and the bytes of the length are appended to `out`.
///
/// The `swap` closure is used to swap bytes for non-native endianess. For
/// native endianess, a no-op closure should be passed.
fn read_binary_len(
    buf: &mut impl ParseBuf,
    ty: ListLenType,
    out: &mut Vec<u8>,
    swap: impl FnOnce(&mut [u8]),
) -> Result<u32, Error> {
    let offset = out.len();
    match ty {
        ListLenType::UChar => buf.with_bytes(1, |b| {
            out.push(b.data[0]);
            Ok(b.data[0] as u32)
        }),
        ListLenType::UShort => buf.with_bytes(2, |b| {
            out.extend_from_slice(&b.data);
            swap(&mut out[offset..]);
            Ok(NativeEndian::read_u16(&out[offset..]) as u32)
        }),
        ListLenType::UInt => buf.with_bytes(4, |b| {
            out.extend_from_slice(&b.data);
            swap(&mut out[offset..]);
            Ok(NativeEndian::read_u32(&out[offset..]) as u32)
        }),
    }
}

/// Reads one element in native endianess from `buf` into `raw_elem`.
fn read_element_binary_native(
    buf: &mut impl ParseBuf,
    index: &[TypeLen],
    raw_elem: &mut RawElement,
) -> Result<(), Error> {
    let mut offset = RawOffset::from(0);

    for (prop_info, prop_len) in raw_elem.prop_infos.iter_mut().zip(index) {
        prop_info.offset = offset;

        // Just calculate the length of this property.
        let len: RawOffset = match *prop_len {
            TypeLen::Scalar(len) => len.into(),
            TypeLen::List { len_len, scalar_len } => {
                let offset_before = buf.offset();

                // Read the list lenght. The type of this property is a list
                // (the index says so), so we can unwrap here.
                let list_len = read_binary_len(
                    buf,
                    prop_info.ty.len_type().unwrap(),
                    &mut raw_elem.data,
                    |_| {}, // no byte swapping
                )?;
                offset += len_len;

                let data_len = list_data_len(list_len, scalar_len).ok_or_else(|| {
                    let span = Span::new(offset_before, buf.offset());
                    ParseError::LookAheadTooBig(Some(span))
                })?;
                RawOffset::from(data_len)
            }
        };

        read_bytes_into(buf, len.as_usize(), &mut raw_elem.data)?;
        offset += len;
    }

    Ok(())
}

/// Reads one element in non-native endianess from `buf` into `raw_elem`.
fn read_element_binary_swapped(
    buf: &mut impl ParseBuf,
    index: &[TypeLen],
    raw_elem: &mut RawElement,
) -> Result<(), Error> {
    for (prop_info, prop_len) in raw_elem.prop_infos.iter_mut().zip(index) {
        // Starting offset of the data we are about to push
        prop_info.offset = RawOffset::from(raw_elem.data.len() as u32);
        let mut start = prop_info.offset;

        match *prop_len {
            TypeLen::Scalar(size) => {
                read_bytes_into(buf, size.as_u8().into(), &mut raw_elem.data)?;
                raw_elem.data[start..].reverse();
            }
            TypeLen::List { len_len, scalar_len } => {
                let offset_before = buf.offset();

                // Read the list lenght. The type of this property is a list
                // (the index says so), so we can unwrap here.
                let list_len = read_binary_len(
                    buf,
                    prop_info.ty.len_type().unwrap(),
                    &mut raw_elem.data,
                    |s| s.reverse(),
                )?;

                // Calculate the total list length and load the raw data (still
                // in non-native endianess).
                let data_len = list_data_len(list_len, scalar_len).ok_or_else(|| {
                    let span = Span::new(offset_before, buf.offset());
                    ParseError::LookAheadTooBig(Some(span))
                })?;
                read_bytes_into(buf, data_len as usize, &mut raw_elem.data)?;

                // Swap bytes of list elements
                start += len_len;
                for _ in 0..list_len {
                    let end = start + scalar_len;
                    raw_elem.data[start..end].reverse();
                    start = end;
                }
            }
        }
    }

    Ok(())
}

fn list_data_len(list_len: u32, scalar_len: ScalarLen) -> Option<u32> {
    u32::try_from((list_len as u64) * (scalar_len as u64))
        .ok()
        .filter(|data_len| (*data_len as usize) < MAX_BUFFER_SIZE)
}

// Reads until the next whitespace or linebreak and tries to parse the string
// as `$ty`.
macro_rules! ascii_parser {
    ($buf:ident, $ty:ident) => {
        $buf.take_until(
            |b| b == b' ' || b == b'\n',
            |sd| {
                sd.assert_ascii()?
                    .parse::<$ty>()
                    .map_err(|e| {
                        let msg = format!(
                            concat!("invalid '", stringify!($ty), "' literal: {}"),
                            e,
                        );
                        sd.error(msg).into()
                    })
            }
        )
    }
}

/// Reads one element in ASCII representation from `buf` into `raw_elem`.
fn read_element_ascii<P: ParseBuf>(
    buf: &mut P,
    _: &[TypeLen],
    raw_elem: &mut RawElement,
) -> Result<(), Error> {
    let mut offset = RawOffset::from(0);
    let mut first = true;

    for prop_info in raw_elem.prop_infos.iter_mut() {
        if first {
            parse::opt_whitespace(buf)?;
            first = false;
        } else {
            parse::whitespace(buf)?;
        }

        let len: RawOffset = match prop_info.ty {
            PropertyType::Scalar(ty) => {
                read_ascii_value(buf, ty, &mut raw_elem.data)?;
                ty.len().into()
            }
            PropertyType::List { len_type, scalar_type } => {
                let list_len = match len_type {
                    ListLenType::UChar => {
                        let len = ascii_parser!(buf, u8)?;
                        len.encode_ne(&mut raw_elem.data);
                        len as u32
                    }
                    ListLenType::UShort => {
                        let len = ascii_parser!(buf, u16)?;
                        len.encode_ne(&mut raw_elem.data);
                        len as u32
                    }
                    ListLenType::UInt => {
                        let len = ascii_parser!(buf, u32)?;
                        len.encode_ne(&mut raw_elem.data);
                        len
                    }
                };

                for _ in 0..list_len {
                    parse::whitespace(buf)?;
                    read_ascii_value(buf, scalar_type, &mut raw_elem.data)?;
                }

                RawOffset::from(list_len * scalar_type.len() as u32) + len_type.len()
            }
        };

        prop_info.offset = offset;
        offset += RawOffset::from(len);
    }

    parse::linebreak(buf)?;

    Ok(())
}

/// Read a single ASCII value of type `ty` into `out`.
fn read_ascii_value(
    buf: &mut impl ParseBuf,
    ty: ScalarType,
    out: &mut Vec<u8>,
) -> Result<(), Error> {
    match ty {
        ScalarType::Char => ascii_parser!(buf, i8)?.encode_ne(out),
        ScalarType::UChar => ascii_parser!(buf, u8)?.encode_ne(out),
        ScalarType::Short => ascii_parser!(buf, i16)?.encode_ne(out),
        ScalarType::UShort => ascii_parser!(buf, u16)?.encode_ne(out),
        ScalarType::Int => ascii_parser!(buf, i32)?.encode_ne(out),
        ScalarType::UInt => ascii_parser!(buf, u32)?.encode_ne(out),
        ScalarType::Float => ascii_parser!(buf, f32)?.encode_ne(out),
        ScalarType::Double => ascii_parser!(buf, f64)?.encode_ne(out),
    }

    Ok(())
}

#[derive(Debug, Clone)]
enum TypeLen {
    Scalar(ScalarLen),
    List {
        len_len: ScalarLen,
        scalar_len: ScalarLen,
    },
}

fn get_type_lens(element_def: &ElementDef) -> Vec<TypeLen> {
    element_def
        .property_defs
        .iter()
        .map(|def| {
            match def.ty {
                PropertyType::Scalar(ty) => TypeLen::Scalar(ty.len()),
                PropertyType::List { len_type, scalar_type } => TypeLen::List {
                    len_len: len_type.len(),
                    scalar_len: scalar_type.len(),
                }
            }
        })
        .collect()
}

trait FromBytes {
    const SIZE: usize;
    fn from_bytes_ne(bytes: &[u8]) -> Self;
    fn encode_ne(&self, out: &mut Vec<u8>);
}

macro_rules! impl_from_bytes {
    ($ty:ident, $read_fun:ident, $write_fun:ident, $size:expr) => {
        impl FromBytes for $ty {
            const SIZE: usize = $size;

            #[inline(always)]
            fn from_bytes_ne(bytes: &[u8]) -> Self {
                NativeEndian::$read_fun(bytes)
            }

            #[inline(always)]
            fn encode_ne(&self, out: &mut Vec<u8>) {
                out.$write_fun::<NativeEndian>(*self).unwrap();
            }
        }
    }
}

impl FromBytes for i8 {
    const SIZE: usize = 1;

    #[inline(always)]
    fn from_bytes_ne(bytes: &[u8]) -> Self {
        bytes[0] as i8
    }

    #[inline(always)]
    fn encode_ne(&self, out: &mut Vec<u8>) {
        out.push(*self as u8);
    }
}

impl FromBytes for u8 {
    const SIZE: usize = 1;

    #[inline(always)]
    fn from_bytes_ne(bytes: &[u8]) -> Self {
        bytes[0]
    }

    #[inline(always)]
    fn encode_ne(&self, out: &mut Vec<u8>) {
        out.push(*self);
    }
}

impl_from_bytes!(u16, read_u16, write_u16, 2);
impl_from_bytes!(u32, read_u32, write_u32, 4);
impl_from_bytes!(i16, read_i16, write_i16, 2);
impl_from_bytes!(i32, read_i32, write_i32, 4);
impl_from_bytes!(f32, read_f32, write_f32, 4);
impl_from_bytes!(f64, read_f64, write_f64, 8);

trait PlyInteger {
    fn as_usize(&self) -> usize;
}

macro_rules! impl_ply_integer {
    ($ty:ident) => {
        impl PlyInteger for $ty {
            fn as_usize(&self) -> usize {
                *self as usize
            }
        }
    }
}

impl_ply_integer!(u8);
impl_ply_integer!(u16);
impl_ply_integer!(u32);
impl_ply_integer!(i8);
impl_ply_integer!(i16);
impl_ply_integer!(i32);
