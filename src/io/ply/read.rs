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

use std::{
    cmp::min,
    convert::{TryFrom, TryInto},
    fs::File,
    io,
    marker::PhantomData,
    path::Path,
};

use byteorder::{ByteOrder, NativeEndian, WriteBytesExt};
use cgmath::{Point3, Vector3};

use crate::{
    prelude::*,
    io::{
        StreamSource, MemSink, Primitive, Error, ErrorKind,
        parse::{self, ParseError, Span, Buffer, ParseBuf, MAX_BUFFER_SIZE},
        util::{IndexHandleMap, debug_fmt_bytes},
    },
    traits::marker::{True, False, Bool},
    util::MeshSizeHint,
};
use super::{
    Encoding,
    info::{
        ColorPropInfo,
        ElementInfo,
        Vec3PropInfo,
        VertexIndicesInfo,
        EdgeEndpointsInfo,
    },
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
    // The function is huge, not called often and it's useful to have it as a
    // fixed callstack point in performance traces and such. Therefore
    // inline(never).
    #[inline(never)]
    fn transfer_to<S: MemSink>(self, sink: &mut S) -> Result<(), Error> {
        // This function is rather complicated. Why? Speed. Hopefully.
        //
        // The the basic idea is the following:
        // - First,
        // - Define a `RawSink` that receives the data via `read_raw`
        //   (this is the `RawTransferSink`).
        // - Prepare everything that we can prepare beforehand: offsets into
        //   raw data buffer and function pointer. Function pointers are the
        //   important bit here: We can match over the type of a property and
        //   use the pointer to the function instantiated with that type. Then
        //   the function pointer just needs to be called and the type doesn't
        //   need to be inspected anymore. All these things are stored in the
        //   `RawTransferSink`.
        // - Finally, start reading data via `RawTransferSink`. We also have a
        //   function pointer (`elem_handler`) that points to the function
        //   handling the current element (which in turn will call other
        //   function pointers). This is updated on each `element_group_start`
        //   call.


        // Do some sanity checks and extract useful information for properties
        // we want to read.
        let info = ElementInfo::new(&self.elements)?;

        // Give some size hints to the sink before we start reading.
        sink.size_hint(MeshSizeHint {
            vertex_count: Some(info.vertex.count),
            face_count: info.face.as_ref().map(|i| i.count),
        });

        // Create the raw sink that will do the transfer. Creating it will
        // prepare offsets into the raw buffer and function pointers to do fast
        // reading.
        let mut transfer_sink = RawTransferSink::new(sink, info)?;

        // Finally read all the data into the sink.
        self.read_raw(&mut transfer_sink)
    }
}


// ================================================================================================
// ===== Helper items for implementing the `RawTransferSink`
// ================================================================================================

/// Abstracts over N-dimensional properties (currently only 3 or 4).
trait PropLayout {
    type Scalar: Primitive + FromBytes;
    /// An array of `PropIndex` elements with length N.
    type Idx;
    /// An array of `Scalar` elements with length N.
    type Out;

    /// Extracts the property from the raw element, assuming the values are
    /// stored contiguously and in order. Only the first element of `indices` is
    /// used.
    fn extract_contiguous(elem: &RawElement, indices: &Self::Idx) -> Self::Out;

    /// Extracts the property from the raw element by using the given indices.
    fn extract_separate(elem: &RawElement, indices: &Self::Idx) -> Self::Out;
}

/// Cold panic function to optimize code size.
#[cold]
#[inline(never)]
fn panic_bug_in_raw_element() -> ! {
    panic!("PLY reader bug: some offsets in `raw_element.prop_infos` point \
        outside of `raw_element.data`");
}

/// A 3 dimensional property.
struct Vec3Layout<T>(!, PhantomData<T>);
impl<T: Primitive + FromBytes> PropLayout for Vec3Layout<T> {
    type Scalar = T;
    type Idx = [PropIndex; 3];
    type Out = [T; 3];

    fn extract_contiguous(elem: &RawElement, &[idx, ..]: &Self::Idx) -> Self::Out {
        // This index operation creates a bound check that is not necessary in
        // theory (if we assume the `RawElement` is internally consistent). But
        // the performance win we get from removing this bound check is fairly
        // small and I am not willing to risk unsafety for that.
        let start = elem.prop_infos[idx].offset.0 as usize;
        let end = start + 3 * T::SIZE;

        // This check is explicit here to avoid three different bound checks
        // below. In theory, as above, not a single check would be necessary.
        if elem.data.len() < end {
            panic_bug_in_raw_element();
        }

        let data = &(*elem.data)[start..end];
        [
            T::from_bytes_ne(&data[0 * T::SIZE..1 * T::SIZE]),
            T::from_bytes_ne(&data[1 * T::SIZE..2 * T::SIZE]),
            T::from_bytes_ne(&data[2 * T::SIZE..3 * T::SIZE]),
        ]
    }
    fn extract_separate(elem: &RawElement, indices: &Self::Idx) -> Self::Out {
        // This leads to 6 bound checks which are unfortunate. We "in theory"
        // know that all `indices` are valid and all `offsets` are valid too. It
        // would be a bug if they weren't. But as this function is probably
        // called very rarely, it's not worth it introducing unsafe code.
        let offsets = indices.map(|&i| elem.prop_infos[i].offset.0 as usize);
        offsets.map(|o| T::from_bytes_ne(&(*elem.data)[o..o + T::SIZE]))
    }
}

/// A 4 dimensional property.
struct Vec4Layout<T>(!, PhantomData<T>);
impl<T: Primitive + FromBytes> PropLayout for Vec4Layout<T> {
    type Scalar = T;
    type Idx = [PropIndex; 4];
    type Out = [T; 4];

    // For comments, see the same methods of `Vec3Layout`.
    fn extract_contiguous(elem: &RawElement, &[idx, ..]: &Self::Idx) -> Self::Out {
        let start = elem.prop_infos[idx].offset.0 as usize;
        let end = start + 4 * T::SIZE;

        if elem.data.len() < end {
            panic_bug_in_raw_element();
        }

        let data = &(*elem.data)[start..end];
        [
            T::from_bytes_ne(&data[0 * T::SIZE..1 * T::SIZE]),
            T::from_bytes_ne(&data[1 * T::SIZE..2 * T::SIZE]),
            T::from_bytes_ne(&data[2 * T::SIZE..3 * T::SIZE]),
            T::from_bytes_ne(&data[3 * T::SIZE..4 * T::SIZE]),
        ]
    }
    fn extract_separate(elem: &RawElement, indices: &Self::Idx) -> Self::Out {
        let offsets = indices.map(|&i| elem.prop_infos[i].offset.0 as usize);
        offsets.map(|o| T::from_bytes_ne(&(*elem.data)[o..o + T::SIZE]))
    }
}

/// Abstracts over marker types representing a kind of property (like position,
/// normal or color). Does not contain information about the associated element
/// (e.g. vertex, face, edge).
trait PropKind {
    /// Layout of this property.
    type Layout: PropLayout;
    /// The target type that is expected by the corresponding method of
    /// `MemSink`.
    type Target: From<<Self::Layout as PropLayout>::Out>;
}

macro_rules! new_prop_kind {
    ($name:ident, $layout:ident, $target:ident) => {
        struct $name<T: Primitive + FromBytes>(!, PhantomData<T>);

        impl<T: Primitive + FromBytes> PropKind for $name<T> {
            type Layout = $layout<T>;
            type Target = $target<T>;
        }
    };
}

new_prop_kind!(PositionProp, Vec3Layout, Point3);
new_prop_kind!(NormalProp, Vec3Layout, Vector3);

enum RgbColorProp {}
impl PropKind for RgbColorProp {
    type Layout = Vec3Layout<u8>;
    type Target = [u8; 3];
}
enum RgbaColorProp {}
impl PropKind for RgbaColorProp {
    type Layout = Vec4Layout<u8>;
    type Target = [u8; 4];
}

/// A prop kind that can be associated with a specific element (through `H`) and
/// that can be inserted into a `MemSink`.
trait SinkInserter<H>: PropKind {
    fn insert<S: MemSink>(sink: &mut S, handle: H, value: Self::Target);
}

macro_rules! impl_sink_inserter {
    ($name:ident $(<$t:ident>)?, $handle:ident, $method:ident) => {
        impl $(<$t: Primitive + FromBytes>)? SinkInserter<$handle> for $name $(<$t>)? {
            fn insert<S: MemSink>(sink: &mut S, handle: $handle, value: Self::Target) {
                sink.$method(handle, value);
            }
        }
    };
}

impl_sink_inserter!(PositionProp<T>, VertexHandle, set_vertex_position);
impl_sink_inserter!(NormalProp<T>, VertexHandle, set_vertex_normal);
impl_sink_inserter!(NormalProp<T>, FaceHandle, set_face_normal);
impl_sink_inserter!(RgbColorProp, VertexHandle, set_vertex_color);
impl_sink_inserter!(RgbaColorProp, VertexHandle, set_vertex_color);
impl_sink_inserter!(RgbColorProp, FaceHandle, set_face_color);
impl_sink_inserter!(RgbaColorProp, FaceHandle, set_face_color);
impl_sink_inserter!(RgbColorProp, EdgeHandle, set_edge_color);
impl_sink_inserter!(RgbaColorProp, EdgeHandle, set_edge_color);

/// Abstracts over the layout of the values of one property in the file. They
/// can either be contiguous or separate.
trait IdxLayout {
    /// Extracts all values of this property from the given `elem` using the
    /// indices by the given `provider`.
    fn extract<Prop, Provider>(
        provider: &Provider,
        elem: &RawElement,
    ) -> <Prop::Layout as PropLayout>::Out
    where
        Prop: PropKind,
        Provider: IdxProvider<Prop>;
}

/// All values are next to each other in the right order. This is the usual case
/// and it allows for some faster special casing.
enum ContiguousIdx {}
impl IdxLayout for ContiguousIdx {
    fn extract<Prop, Provider>(
        provider: &Provider,
        elem: &RawElement,
    ) -> <Prop::Layout as PropLayout>::Out
    where
        Prop: PropKind,
        Provider: IdxProvider<Prop>,
    {
        let idxs = provider.idx();
        Prop::Layout::extract_contiguous(elem, idxs)
    }
}

/// The individual values are not in the right order or not next to each other.
/// This is very rare, but we still want to read those files.
enum SeparateIdx {}
impl IdxLayout for SeparateIdx {
    fn extract<Prop, Provider>(
        provider: &Provider,
        elem: &RawElement,
    ) -> <Prop::Layout as PropLayout>::Out
    where
        Prop: PropKind,
        Provider: IdxProvider<Prop>
    {
        let idxs = provider.idx();
        Prop::Layout::extract_separate(elem, idxs)
    }
}

/// Something that can provide indices for a specific property. These indices
/// denote the position of the invididual values of the property inside the raw
/// element data.
trait IdxProvider<Prop: PropKind> {
    fn idx(&self) -> &<Prop::Layout as PropLayout>::Idx;
}

macro_rules! impl_idx_provider {
    ($name:ident, $prop:ident $(<$t:ident>)?, $field:ident) => {
        impl $(<$t: Primitive + FromBytes>)? IdxProvider<$prop $(<$t>)?> for $name {
            fn idx(&self) -> &<<$prop $(<$t>)? as PropKind>::Layout as PropLayout>::Idx {
                &self.$field
            }
        }
    };
}

impl_idx_provider!(VertexReadState, PositionProp<T>, position_idx);
impl_idx_provider!(VertexReadState, NormalProp<T>, normal_idx);
impl_idx_provider!(VertexReadState, RgbaColorProp, color_idx);
impl_idx_provider!(FaceReadState, NormalProp<T>, normal_idx);
impl_idx_provider!(FaceReadState, RgbaColorProp, color_idx);
impl_idx_provider!(EdgeReadState, RgbaColorProp, color_idx);

// Manual impls for RGB as we want to resuse the 4 element RGBA index.
impl IdxProvider<RgbColorProp> for VertexReadState {
    fn idx(&self) -> &<<RgbColorProp as PropKind>::Layout as PropLayout>::Idx {
        (&self.color_idx[..3]).try_into().unwrap()
    }
}
impl IdxProvider<RgbColorProp> for FaceReadState {
    fn idx(&self) -> &<<RgbColorProp as PropKind>::Layout as PropLayout>::Idx {
        (&self.color_idx[..3]).try_into().unwrap()
    }
}
impl IdxProvider<RgbColorProp> for EdgeReadState {
    fn idx(&self) -> &<<RgbColorProp as PropKind>::Layout as PropLayout>::Idx {
        (&self.color_idx[..3]).try_into().unwrap()
    }
}


/// Something that is associated with a specific element (through the `Handle`)
/// and can provide the handle of the currently handled element.
trait ElementReadState {
    type Handle;
    fn current_handle(&self) -> Self::Handle;
}

macro_rules! impl_element_read_state {
    ($name:ident, $handle:ident) => {
        impl ElementReadState for $name {
            type Handle = $handle;
            #[inline(always)]
            fn current_handle(&self) -> Self::Handle {
                self.current_handle
            }
        }
    };
}

impl_element_read_state!(VertexReadState, VertexHandle);
impl_element_read_state!(FaceReadState, FaceHandle);
impl_element_read_state!(EdgeReadState, EdgeHandle);


// ================================================================================================
// ===== Raw sink for `Reader::transfer_to`
// ================================================================================================

/// Vertex related state needed while reading.
struct VertexReadState {
    count: usize,
    handles: IndexHandleMap<VertexHandle>,
    current_handle: VertexHandle,
    position_idx: [PropIndex; 3],
    normal_idx: [PropIndex; 3],
    color_idx: [PropIndex; 4],
}
/// Face related state needed while reading.
struct FaceReadState {
    count: usize,
    current_handle: FaceHandle,
    vertex_indices_idx: PropIndex,
    normal_idx: [PropIndex; 3],
    color_idx: [PropIndex; 4],
}
/// Edge related state needed while reading.
struct EdgeReadState {
    count: usize,
    current_handle: EdgeHandle,
    endpoint_indices_idx: [PropIndex; 2],
    color_idx: [PropIndex; 4],
}

/// Super generic property reading function. This function is monomorphized
/// appropriately in `ReadState::new` and those instances are saved as function
/// pointers. These are then called at the appropriate time.
///
/// The `sink` and `elem` should be self explanatory; the `state` argument is
/// the state for the appropriate element (e.g. `VertexReadState`).
///
/// Now the type arguments:
/// - `Sink` is simply the type of the sink.
/// - `State` is the type of the state (again, `VertexReadState` for example).
/// - `Prop` is a marker type implementing `PropKind`. This denotes what
///   property to read. It subsequently defines the layout (number of values to
///   read), the method to call on the sink as well as the type passed to the
///   sink.
/// - `Layout` specifies whether or not all values of the property are stored
///   contiguously.
fn read_prop<Sink, State, Prop, Layout>(
    sink: &mut Sink,
    elem: &RawElement,
    state: &State,
)
where
    Sink: MemSink,
    State: IdxProvider<Prop> + ElementReadState,
    Prop: PropKind + SinkInserter<State::Handle>,
    Prop::Target: From<<Prop::Layout as PropLayout>::Out>,
    Layout: IdxLayout,
{
    let prop_data = Layout::extract::<Prop, State>(&state, elem);
    Prop::insert(sink, state.current_handle(), prop_data.into());
}

fn bug_read_prop<Sink, State>(_: &mut Sink, _: &RawElement, _: &State) {
    panic!("bug in PLY `RawTransferSink`: property reader of non-existant property called");
}

type VertexPropHandler<S> = fn(&mut S, &RawElement, &VertexReadState);
type FacePropHandler<S> = fn(&mut S, &RawElement, &FaceReadState);
type EdgePropHandler<S> = fn(&mut S, &RawElement, &EdgeReadState);

/// A raw sink that transfers the raw data to the given `MemSink`. This is the
/// core of `Reader::transfer_to`.
struct RawTransferSink<'a, S: MemSink> {
    sink: &'a mut S,

    /// The function handling the current element type (the last passed to
    /// `element_group_start`).
    handle_element: fn(&mut Self, &RawElement) -> Result<(), Error>,

    /// Most of the element specific state.
    vertex_state: VertexReadState,
    face_state: FaceReadState,
    edge_state: EdgeReadState,

    // Generic property handlers.
    read_vertex_position: VertexPropHandler<S>,
    read_vertex_normal: VertexPropHandler<S>,
    read_vertex_color: VertexPropHandler<S>,

    read_face_normal: FacePropHandler<S>,
    read_face_color: FacePropHandler<S>,

    read_edge_color: EdgePropHandler<S>,

    /// Function to read the `vertex_indices` property and add the face. Returns
    /// `Err` if an vertex index is not valid or if the sink returns an error
    /// from `add_face`.
    read_face_vertex_indices:
        fn(&mut Self, &RawElement, &RawListInfo) -> Result<FaceHandle, Error>,

    /// This is a temporary buffer for `read_face_vertex_indices` to store
    /// vertex handles in. It shouldn't be used except inside of
    /// `read_face_vertex_indices`.
    vertex_handles_buffer: Vec<VertexHandle>,

    /// Function to read the `vertex1` and `vertex2` properties. Returns the
    /// edge handle of the edge between the two vertices, or an `Err` if no such
    /// edge exists.
    read_edge_endpoint_indices: fn(&mut Self, &RawElement) -> Result<EdgeHandle, Error>,

    info: ElementInfo,
}

impl<'a, S: MemSink> RawTransferSink<'a, S> {
    fn new(sink: &'a mut S, info: ElementInfo) -> Result<Self, Error> {
        // Two helper macros to avoid code duplication. They will call the
        // `prepare_` function and create the correct function pointer to the
        // `read_prop` function and push it onto `fns_vec`.
        macro_rules! prepare_prop {
            ($info:expr, $prep_fn:ident, $prop:ident, $count:expr, $state:ident $(,)?) => {
                if let Some(Vec3PropInfo { ty, idx }) = $info {
                    macro_rules! fn_ptr {
                        ($scalar:ident) => {
                            if idx.is_contiguous() {
                                read_prop::<S, $state, $prop<$scalar>, ContiguousIdx>
                            } else {
                                read_prop::<S, $state, $prop<$scalar>, SeparateIdx>
                            }

                        };
                    }

                    let fn_ptr = match ty {
                        ScalarType::Char => {
                            sink.$prep_fn::<i8>($count)?;
                            fn_ptr!(i8)
                        },
                        ScalarType::UChar => {
                            sink.$prep_fn::<u8>($count)?;
                            fn_ptr!(u8)
                        },
                        ScalarType::Short => {
                            sink.$prep_fn::<i16>($count)?;
                            fn_ptr!(i16)
                        },
                        ScalarType::UShort => {
                            sink.$prep_fn::<u16>($count)?;
                            fn_ptr!(u16)
                        },
                        ScalarType::Int => {
                            sink.$prep_fn::<i32>($count)?;
                            fn_ptr!(i32)
                        },
                        ScalarType::UInt => {
                            sink.$prep_fn::<u32>($count)?;
                            fn_ptr!(u32)
                        },
                        ScalarType::Float => {
                            sink.$prep_fn::<f32>($count)?;
                            fn_ptr!(f32)
                        },
                        ScalarType::Double => {
                            sink.$prep_fn::<f64>($count)?;
                            fn_ptr!(f64)
                        },
                    };

                    Some((fn_ptr, idx.indices()))
                } else {
                    None
                }
            }
        }

        macro_rules! prepare_color_prop {
            ($info:expr, $prep_fn:ident, $count:expr, $state:ident $(,)?) => {
                if let Some(info) = $info {
                    let (fn_ptr, idx) = match info {
                        ColorPropInfo::ContiguousRgb(ri) => {
                            sink.$prep_fn::<[u8; 3]>($count)?;
                            let f = read_prop::<S, $state, RgbColorProp, ContiguousIdx>;
                            let idx = [ri, PropIndex(0), PropIndex(0), PropIndex(0)];
                            (f as fn(&mut _, &_, &_), idx)
                        }
                        ColorPropInfo::SeparateRgb([ri, gi, bi]) => {
                            sink.$prep_fn::<[u8; 3]>($count)?;
                            let f = read_prop::<S, $state, RgbColorProp, SeparateIdx>;
                            let idx = [ri, gi, bi, PropIndex(0)];
                            (f as fn(&mut _, &_, &_), idx)
                        }
                        ColorPropInfo::ContiguousRgba(ri) => {
                            sink.$prep_fn::<[u8; 4]>($count)?;
                            let f = read_prop::<S, $state, RgbaColorProp, ContiguousIdx>;
                            let idx = [ri, PropIndex(0), PropIndex(0), PropIndex(0)];
                            (f as fn(&mut _, &_, &_), idx)
                        }
                        ColorPropInfo::SeparateRgba(idx) => {
                            sink.$prep_fn::<[u8; 4]>($count)?;
                            let f = read_prop::<S, $state, RgbColorProp, SeparateIdx>;
                            (f as fn(&mut _, &_, &_), idx)
                        }
                    };

                    Some((fn_ptr, idx))
                } else {
                    None
                }
            }
        }


        // Prepare handlers and indices for generic vertex properties.
        let vertex_position = prepare_prop!(
            info.vertex.position,
            prepare_vertex_positions,
            PositionProp,
            info.vertex.count,
            VertexReadState,
        );
        let vertex_normal = prepare_prop!(
            info.vertex.normal,
            prepare_vertex_normals,
            NormalProp,
            info.vertex.count,
            VertexReadState,
        );
        let vertex_color = prepare_color_prop!(
            info.vertex.color,
            prepare_vertex_colors,
            info.vertex.count,
            VertexReadState,
        );

        // Prepare handlers and indices for generic face properties.
        let mut face_normal = None;
        let mut face_color = None;
        if let Some(face_info) = info.face.as_ref() {
            face_normal = prepare_prop!(
                face_info.normal,
                prepare_face_normals,
                NormalProp,
                face_info.count,
                FaceReadState,
            );
            face_color = prepare_color_prop!(
                face_info.color,
                prepare_face_colors,
                face_info.count,
                FaceReadState,
            );
        }

        // Prepare handlers and indices for generic edge properties.
        let mut edge_color = None;
        if let Some(edge_info) = info.edge.as_ref() {
            edge_color = prepare_color_prop!(
                edge_info.color,
                prepare_edge_colors,
                edge_info.count,
                EdgeReadState,
            );
        }

        // Correctly instantiate the function reading the 'vertex_indices'
        // property which is used to create the faces.
        let (vertex_indices_idx, read_face_vertex_indices)
            = match info.face.as_ref().map(|f| f.vertex_indices)
        {
            Some(VertexIndicesInfo { idx, ty }) => {
                let fun = match ty {
                    ScalarType::Char => Self::read_face_vertex_indices::<i8>,
                    ScalarType::UChar => Self::read_face_vertex_indices::<u8>,
                    ScalarType::Short => Self::read_face_vertex_indices::<i16>,
                    ScalarType::UShort => Self::read_face_vertex_indices::<u16>,
                    ScalarType::Int => Self::read_face_vertex_indices::<i32>,
                    ScalarType::UInt => Self::read_face_vertex_indices::<u32>,

                    // Checked by `VertexIndicesInfo::new`
                    ScalarType::Float | ScalarType::Double => unreachable!(),
                };

                (idx, fun)
            }
            None => (
                PropIndex(0),
                Self::vertex_indices_bug as fn(&mut _, &_, &_) -> _,
            ),
        };

        // Correctly instantiate the function reading the 'vertex1' and
        // 'vertex2' properties which are used to define an edge.
        let (endpoint_indices_idx, read_edge_endpoint_indices)
            = match info.edge.as_ref().map(|e| e.endpoints)
        {
            Some(EdgeEndpointsInfo { idx, ty }) => {
                let fun = match ty {
                    ScalarType::Char => Self::read_edge_endpoint_indices::<i8>,
                    ScalarType::UChar => Self::read_edge_endpoint_indices::<u8>,
                    ScalarType::Short => Self::read_edge_endpoint_indices::<i16>,
                    ScalarType::UShort => Self::read_edge_endpoint_indices::<u16>,
                    ScalarType::Int => Self::read_edge_endpoint_indices::<i32>,
                    ScalarType::UInt => Self::read_edge_endpoint_indices::<u32>,

                    // Checked by `EdgeEndpointsInfo::new`
                    ScalarType::Float | ScalarType::Double => unreachable!(),
                };

                (idx, fun)
            }
            None => (
                [PropIndex(0); 2],
                Self::edge_endpoints_bug as fn(&mut _, &_) -> _,
            ),
        };

        // Unpack generic properties and use appropriate defaults.
        let (read_vertex_position, vertex_position_idx) = vertex_position
            .unwrap_or((bug_read_prop::<S, VertexReadState>, [PropIndex(0); 3]));
        let (read_vertex_normal, vertex_normal_idx) = vertex_normal
            .unwrap_or((bug_read_prop::<S, VertexReadState>, [PropIndex(0); 3]));
        let (read_vertex_color, vertex_color_idx) = vertex_color
            .unwrap_or((bug_read_prop::<S, VertexReadState>, [PropIndex(0); 4]));

        let (read_face_normal, face_normal_idx) = face_normal
            .unwrap_or((bug_read_prop::<S, FaceReadState>, [PropIndex(0); 3]));
        let (read_face_color, face_color_idx) = face_color
            .unwrap_or((bug_read_prop::<S, FaceReadState>, [PropIndex(0); 4]));

        let (read_edge_color, edge_color_idx) = edge_color
            .unwrap_or((bug_read_prop::<S, EdgeReadState>, [PropIndex(0); 4]));


        Ok(Self {
            sink,
            handle_element: Self::element_handler_bug,
            info,

            // Generic prop handler
            read_vertex_position,
            read_vertex_normal,
            read_vertex_color,
            read_face_normal,
            read_face_color,
            read_edge_color,

            // Special prop handler
            read_face_vertex_indices,
            vertex_handles_buffer: Vec::new(),
            read_edge_endpoint_indices,

            // Element states
            vertex_state: VertexReadState {
                count: 0,
                handles: IndexHandleMap::new(),
                current_handle: VertexHandle::new(0),
                position_idx: vertex_position_idx,
                normal_idx: vertex_normal_idx,
                color_idx: vertex_color_idx,
            },
            face_state: FaceReadState {
                count: 0,
                current_handle: FaceHandle::new(0),
                vertex_indices_idx,
                normal_idx: face_normal_idx,
                color_idx: face_color_idx,
            },
            edge_state: EdgeReadState {
                count: 0,
                current_handle: EdgeHandle::new(0),
                endpoint_indices_idx,
                color_idx: edge_color_idx,
            },
        })
    }

    // ===== Element handling functions ==========================================================
    fn ignore_element(&mut self, _: &RawElement) -> Result<(), Error> {
        Ok(())
    }

    fn element_handler_bug(&mut self, _: &RawElement) -> Result<(), Error> {
        panic!(
            "bug in `ply::Reader::read_raw`: `element()` called before `element_group_start()`"
        );
    }

    fn handle_vertex<HasPosition: Bool, HasNormal: Bool, HasColor: Bool>(
        &mut self,
        elem: &RawElement,
    ) -> Result<(), Error> {
        // Add vertex
        let vh = self.sink.add_vertex();
        self.vertex_state.handles.add(self.vertex_state.count, vh);
        self.vertex_state.count += 1;
        self.vertex_state.current_handle = vh;

        // Read vertex properties
        if HasPosition::VALUE {
            (self.read_vertex_position)(&mut self.sink, elem, &self.vertex_state);
        }
        if HasNormal::VALUE {
            (self.read_vertex_normal)(&mut self.sink, elem, &self.vertex_state);
        }
        if HasColor::VALUE {
            (self.read_vertex_color)(&mut self.sink, elem, &self.vertex_state);
        }

        Ok(())
    }

    fn handle_face<HasNormal: Bool, HasColor: Bool>(
        &mut self,
        elem: &RawElement,
    ) -> Result<(), Error> {
        // `VertexIndicesInfo::new` already made sure that this is a
        // list.
        let vi_list = elem.decode_list_at(self.face_state.vertex_indices_idx)
            .expect("bug in PLY reader: 'vertex_indices' not a list?");

        // Make sure the list is not too short.
        if vi_list.list_len < 3 {
            return Err(invalid_input!(
                "the face at index {} has only {} vertices, but at least 3 are required \
                    per face",
                self.face_state.count,
                vi_list.list_len,
            ));
        }

        // Add face
        let fh = (self.read_face_vertex_indices)(self, &elem, &vi_list)?;
        self.face_state.count += 1;
        self.face_state.current_handle = fh;

        // Read generic face properties
        if HasNormal::VALUE {
            (self.read_face_normal)(&mut self.sink, elem, &self.face_state);
        }
        if HasColor::VALUE {
            (self.read_face_color)(&mut self.sink, elem, &self.face_state);
        }

        Ok(())
    }

    fn handle_edge<HasColor: Bool>(&mut self, elem: &RawElement) -> Result<(), Error> {
        // Retrieve edge
        let eh = (self.read_edge_endpoint_indices)(self, elem)?;
        self.edge_state.count += 1;
        self.edge_state.current_handle = eh;

        // Read generic edge properties
        if HasColor::VALUE {
            (self.read_edge_color)(&mut self.sink, elem, &self.edge_state);
        }

        Ok(())
    }

    // ===== Special property reading functions ==================================================

    /// Only dummy function that should never be called.
    fn vertex_indices_bug(
        &mut self,
        _: &RawElement,
        _: &RawListInfo,
    ) -> Result<FaceHandle, Error> {
        panic!("internal bug in PLY reader: 'vertex_indices' handler called but no faces")
    }

    /// Reads from the raw data at the specified offset, interprets them as
    /// vertex indices and adds a face to the sink.
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
            let handle = self.vertex_state.handles.get(index).ok_or_else(|| {
                invalid_input!(
                    "invalid vertex index {} in PLY file: a face's `vertice_indices` \
                        property refers to that vertex index, but no vertex with such \
                        a high index exists in the file",
                    index,
                )
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

    /// Only dummy function that should never be called.
    fn edge_endpoints_bug(&mut self, _: &RawElement) -> Result<EdgeHandle, Error> {
        panic!("internal bug in PLY reader: no edge endpoints but edges")
    }

    /// Reads three values from the raw data at the specified offset,
    /// interprets them as vertex indices and adds a face to the sink.
    fn read_edge_endpoint_indices<T: Primitive + FromBytes + PlyInteger>(
        &mut self,
        elem: &RawElement,
    ) -> Result<EdgeHandle, Error> {
        let get_handle = |index| {
            self.vertex_state.handles.get(index).ok_or_else(|| {
                invalid_input!(
                    "invalid vertex index {} in PLY file: a face's `vertice_indices` \
                        property refers to that vertex index, but no vertex with such \
                        a high index exists in the file",
                    index,
                )
            })
        };

        let [idx0, idx1] = self.edge_state.endpoint_indices_idx;
        let vi0 = T::from_bytes_ne(&elem.data[elem.prop_infos[idx0].offset..]);
        let vi1 = T::from_bytes_ne(&elem.data[elem.prop_infos[idx1].offset..]);

        let v0 = get_handle(vi0.as_usize())?;
        let v1 = get_handle(vi1.as_usize())?;

        // Retrieve edge handle
        let edge = self.sink.get_edge_between([v0, v1])?;
        edge.ok_or_else(|| {
            invalid_input!(
                "invalid edge: PLY file refers to edge between vertex indices {} and \
                    {}, but not such edge exists",
                vi0.as_usize(),
                vi1.as_usize(),
            )
        })
    }
}

impl<S: MemSink> RawSink for RawTransferSink<'_, S> {
    fn element_group_start(&mut self, def: &ElementDef) -> Result<(), Error> {
        self.handle_element = match &*def.name {
            "vertex" => {
                let info = &self.info.vertex;
                match (info.position.is_some(), info.normal.is_some(), info.color.is_some()) {
                    (false, false, false) => Self::handle_vertex::<False, False, False>,
                    (false, false, true ) => Self::handle_vertex::<False, False, True >,
                    (false, true , false) => Self::handle_vertex::<False, True , False>,
                    (false, true , true ) => Self::handle_vertex::<False, True , True >,
                    (true , false, false) => Self::handle_vertex::<True , False, False>,
                    (true , false, true ) => Self::handle_vertex::<True , False, True >,
                    (true , true , false) => Self::handle_vertex::<True , True , False>,
                    (true , true , true ) => Self::handle_vertex::<True , True , True >,
                }
            }
            "face" => {
                let info = self.info.face.as_ref().unwrap();
                match (info.normal.is_some(), info.color.is_some()) {
                    (false, false) => Self::handle_face::<False, False>,
                    (false, true ) => Self::handle_face::<False, True >,
                    (true , false) => Self::handle_face::<True , False>,
                    (true , true ) => Self::handle_face::<True , True >,
                }
            }
            "edge" => {
                let info = self.info.edge.as_ref().unwrap();
                match info.color.is_some() {
                    false => Self::handle_edge::<False>,
                    true  => Self::handle_edge::<True >,
                }
            }
            _ => Self::ignore_element,
        };

        Ok(())
    }

    fn element(&mut self, elem: &RawElement) -> Result<(), Error> {
        (self.handle_element)(self, elem)
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
            #[inline(always)]
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
