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
    marker::PhantomData,
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

macro_rules! invalid_input {
    ($($t:tt)+) => {
        Error::new(|| ErrorKind::InvalidInput(format!($($t)+)))
    };
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
        // - First, do some sanity checks and extract useful information for
        //   properties we want to read (like the position of that property).
        //   This is done directly in this function further below.
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



        let info = ElementInfo::new(&self.elements)?;

        sink.size_hint(MeshSizeHint {
            vertex_count: Some(info.vertex.count),
            face_count: info.face.as_ref().map(|i| i.count),
        });
        let mut helper_sink = ReadState::new(sink, &info)?;
        self.read_raw(&mut helper_sink)
    }
}


// ================================================================================================
// ===== Semantic element and property information
// ================================================================================================

/// Information about elements and their semantic properties. Stores the indices
/// and scalar types of those properties.
#[derive(Debug, Clone)]
struct ElementInfo {
    vertex: VertexInfo,
    face: Option<FaceInfo>,
    edge: Option<EdgeInfo>,
}

#[derive(Debug, Clone)]
struct VertexInfo {
    count: hsize,
    position: Option<Vec3PropInfo>,
    normal: Option<Vec3PropInfo>,
    color: Option<ColorPropInfo>,
}

#[derive(Debug, Clone)]
struct FaceInfo {
    count: hsize,
    vertex_indices: VertexIndicesInfo,
    normal: Option<Vec3PropInfo>,
    color: Option<ColorPropInfo>,
}

#[derive(Debug, Clone)]
struct EdgeInfo {
    count: hsize,
    endpoints: EdgeEndpointsInfo,
    color: Option<ColorPropInfo>,
}

/// Information about the `vertex_indices` property of the 'face' element.
#[derive(Debug, Clone, Copy)]
struct VertexIndicesInfo {
    ty: ScalarType,
    idx: PropIndex,
}

// Information about the `vertex1` and `vertex2` properties defining the two
// endpoints of an edge.
#[derive(Debug, Clone, Copy)]
struct EdgeEndpointsInfo {
    idx: [PropIndex; 2],
    ty: ScalarType,
}

/// Information about a 3-element property (position or normal).
#[derive(Debug, Clone, Copy)]
struct Vec3PropInfo {
    ty: ScalarType,
    idx: Vec3PropIndex,
}

/// Index information about the three single elements of a vec3 like property.
#[derive(Debug, Clone, Copy)]
enum Vec3PropIndex {
    /// All three properties are right next to each other and in the right order
    /// (x, y, z). The `PropIndex` denotes the index of the first (x) property.
    Contiguous(PropIndex),
    /// Unusual case: the properties are in the wrong order or not next to each
    /// other. The indices are given separately for x, y, and z in order.
    Separate([PropIndex; 3]),
}

/// Information about a color property.
#[derive(Debug, Clone, Copy)]
enum ColorPropInfo {
    /// No alpha value, the rgb properties are right next to each other and in
    /// the right order. The `PropIndex` denotes the index of the r property.
    ContiguousRgb(PropIndex),
    /// No alpha value. Unusual case: the properties are in the wrong order or
    /// not next to each other. The indices are given separately for r, g, and b
    /// in order.
    SeparateRgb([PropIndex; 3]),
    /// With alpha value, the rgba properties are right next to each other and
    /// in the right order. The `PropIndex` denotes the index of the r property.
    ContiguousRgba(PropIndex),
    /// With alpha value. Unusual case: the properties are in the wrong order or
    /// not next to each other. The indices are given separately for r, g, b,
    /// and a in order.
    SeparateRgba([PropIndex; 4]),
}


impl ElementInfo {
    /// Gathers semantic information about vertex, face and edge elements and
    /// their respective properties.
    ///
    /// Vertices are required, the other elements are not. Furthermore, faces
    /// have to be stored after vertices and edges have to be stored after
    /// faces. An error is returned if any of those properties is violated.
    fn new(elements: &[ElementDef]) -> Result<Self, Error> {
        let vertex_pos = elements.iter()
            .position(|e| VERTEX_ELEMENT_NAMES.contains(&e.name.as_str()))
            .ok_or_else(|| invalid_input!("no 'vertex' elements in PLY file"))?;

        let face_pos = elements.iter().position(|e| FACE_ELEMENT_NAMES.contains(&e.name.as_str()));
        if let Some(face_pos) = face_pos {
            // Faces can only be in the file after vertices.
            if face_pos < vertex_pos {
                return Err(invalid_input!(
                    "found 'face' elements before 'vertex' elements (that's not allowed)"
                ));
            }
        }

        let edge_pos = elements.iter().position(|e| EDGE_ELEMENT_NAMES.contains(&e.name.as_str()));
        if let Some(edge_pos) = edge_pos {
            // Edges can only be in the file after vertices and faces.
            if face_pos.is_none() || edge_pos < face_pos.unwrap() {
                let problem = if face_pos.is_none() { "but no" } else { "before" };
                return Err(invalid_input!(
                    "found 'edge' elements {} 'face' elements (that's not allowed as \
                        LOX can't add edges on their own; edges always need to be part of a face)",
                    problem,
                ));
            }
        }

        Ok(Self {
            vertex: VertexInfo::new(&elements[vertex_pos])?,
            face: face_pos.map(|pos| FaceInfo::new(&elements[pos])).transpose()?,
            edge: edge_pos.map(|pos| EdgeInfo::new(&elements[pos])).transpose()?,
        })
    }
}

impl VertexInfo {
    fn new(group: &ElementDef) -> Result<Self, Error> {
        Ok(Self {
            count: u64_to_hsize(group.count, "vertices")?,
            position: Vec3PropInfo::new(group, ["x", "y", "z"], "positions")?,
            normal: Vec3PropInfo::new(group, ["nx", "ny", "nz"], "normals")?,
            color: ColorPropInfo::new(group)?,
        })
    }
}

impl FaceInfo {
    fn new(group: &ElementDef) -> Result<Self, Error> {
        Ok(Self {
            count: u64_to_hsize(group.count, "faces")?,
            vertex_indices: VertexIndicesInfo::new(group)?,
            normal: Vec3PropInfo::new(group, ["nx", "ny", "nz"], "normals")?,
            color: ColorPropInfo::new(group)?,
        })
    }
}

impl EdgeInfo {
    fn new(group: &ElementDef) -> Result<Self, Error> {
        Ok(Self {
            count: u64_to_hsize(group.count, "edges")?,
            endpoints: EdgeEndpointsInfo::new(group)?,
            color: ColorPropInfo::new(group)?,
        })
    }
}

impl VertexIndicesInfo {
    /// Gathers information about the `vertex_indices` property.
    ///
    /// This property is treated as required: if it doesn't exist in the given
    /// group, an error is returned. Additionally, an error is returned if the
    /// property is not a list or the list scalar type is floating point.
    fn new(group: &ElementDef) -> Result<Self, Error> {
        let vi_idx = match group.prop_pos("vertex_indices") {
            Some(x) => x,
            None => {
                return Err(invalid_input!("'face' elements without 'vertex_indices' property"));
            }
        };

        let vi = &group.property_defs[vi_idx];
        if !vi.ty.is_list() {
            return Err(
                invalid_input!("'vertex_indices' property has a scalar type (must be a list)")
            );
        }

        if vi.ty.scalar_type().is_floating_point() {
            return Err(invalid_input!(
                "'vertex_indices' list has a floating point element type (only \
                    integers are allowed)"
            ));
        }

        Ok(Self {
            ty: vi.ty.scalar_type(),
            idx: vi_idx,
        })
    }
}

impl EdgeEndpointsInfo {
    /// Gathers information about the properties "vertex1" and "vertex2".
    ///
    /// The properties are required; if they are missing, an error is returned.
    /// Furthermore, an error is returned if either of those properties is a
    /// list or has a floating point type.
    fn new(group: &ElementDef) -> Result<Self, Error> {
        let v1_idx = group.prop_pos("vertex1");
        let v2_idx = group.prop_pos("vertex2");

        // The properties `vertex1` and `vertex2` are required.
        let (v1_idx, v2_idx) = match (v1_idx, v2_idx) {
            (Some(a), Some(b)) => (a, b),
            _ => {
                return Err(invalid_input!(
                    "'edge' element is missing 'vertex1' or 'vertex2' property (or both)"
                ));
            }
        };

        for &(name, idx) in &[("vertex1", v1_idx), ("vertex2", v2_idx)] {
            let def = &group.property_defs[idx];
            if def.ty.is_list() {
                return Err(
                    invalid_input!("'{}' property has a list type (must be a scalar)", name)
                );
            }

            if def.ty.scalar_type().is_floating_point() {
                return Err(invalid_input!(
                    "'{}' list has a floating point element type (only integers are allowed)",
                    name
                ));
            }
        }

        Ok(Self {
            idx: [v1_idx, v2_idx],
            ty: group.property_defs[v1_idx].ty.scalar_type(),
        })
    }
}

impl Vec3PropInfo {
    /// Gathers information about a three-element property with the property
    /// names given in `names` within the given element `group`.
    ///
    /// If the first property name is not found, `Ok(None)` is returned. If it
    /// is found, the two other properties are required -- if they are not
    /// found, an error is returned. Additionally, the following things are
    /// checked (and an error returned if they are violated):
    /// - all three properties must have the same type
    /// - the type of the properties must be a scalar type (not a list)
    ///
    /// If everything works well and the three properties are found,
    /// `Ok(Some(_))` is returned.
    ///
    /// `prop_name_plural` is a human-readable name/description for the property
    /// you are searching for. It's used for error messages. Typical uses of
    /// this method are:
    /// - `Vec3PropInfo::new(_, ["x", "y", "z"], "positions")`
    /// - `Vec3PropInfo::new(_, ["nx", "ny", "nz"], "normals")`
    pub fn new(
        group: &ElementDef,
        names: [&str; 3],
        prop_name_plural: &str,
    ) -> Result<Option<Self>, Error> {
        let [xs, ys, zs] = names;

        let px_idx = match group.prop_pos(xs) {
            Some(x) => x,
            None => return Ok(None),
        };

        let py_idx = group.prop_pos(ys).ok_or_else(|| invalid_input!(
            "element '{}' has '{}' property, but no '{}' property (only 3D {} supported)",
            group.name,
            xs,
            ys,
            prop_name_plural,
        ))?;
        let pz_idx = group.prop_pos(zs).ok_or_else(|| invalid_input!(
            "elem '{}' has '{}' property, but no '{}' property (only 3D {} supported)",
            group.name,
            xs,
            zs,
            prop_name_plural,
        ))?;

        let px = &group.property_defs[px_idx];
        let py = &group.property_defs[py_idx];
        let pz = &group.property_defs[pz_idx];

        if px.ty.is_list() {
            return Err(invalid_input!(
                "property '{}' (element '{}') has a list type (only scalars allowed)",
                xs,
                group.name,
            ));
        }

        if px.ty != py.ty || px.ty != pz.ty {
            return Err(invalid_input!(
                "properties '{}', '{}' and '{}' (element '{}') don't have the same type",
                xs,
                ys,
                zs,
                group.name,
            ));
        }

        Ok(Some(Self {
            ty: px.ty.scalar_type(),
            idx: Vec3PropIndex::new([px_idx, py_idx, pz_idx]),
        }))
    }
}

impl Vec3PropIndex {
    /// Creates a new instance from three given indices. If the indices are
    /// contiguous, `Self::Contiguous` is returned, `Self::Separate` otherwise.
    fn new([x, y, z]: [PropIndex; 3]) -> Self {
        if y.0 == x.0 + 1 && z.0 == y.0 + 1 {
            Self::Contiguous(x)
        } else {
            Self::Separate([x, y, z])
        }
    }

    fn indices(&self) -> [PropIndex; 3] {
        match *self {
            Self::Contiguous(x) => [x, PropIndex(x.0 + 1), PropIndex(x.0 + 2)],
            Self::Separate(all) => all,
        }
    }
}

impl ColorPropInfo {
    /// Gathers information about a color property (names 'red', 'green, 'blue'
    /// and (optionally) 'alpha') in the given 'group'
    ///
    /// If no 'red' property is found, `OK(None)` is returned. If it is found,
    /// 'green' and 'blue' have to exist as well or else an error is returned.
    /// All properties (including 'alpha') must have the type `uchar` (this is
    /// apparently the only color format PLY supports). If that's not the case,
    /// an error is returned.
    ///
    /// If everything goes well, `Ok(Some(_))` is returned.
    pub fn new(group: &ElementDef) -> Result<Option<Self>, Error> {
        let red_idx = match group.prop_pos("red") {
            Some(x) => x,
            None => return Ok(None),
        };

        let green_idx = group.prop_pos("green").ok_or_else(|| invalid_input!(
            "element '{}' has 'red' property, but no 'green' property \
                (only RGB and RGBA colors supported)",
            group.name,
        ))?;
        let blue_idx = group.prop_pos("blue").ok_or_else(|| invalid_input!(
            "element '{}' has 'red' property, but no 'blue' property \
                (only RGB and RGBA colors supported)",
            group.name,
        ))?;

        let red = &group.property_defs[red_idx];
        let green = &group.property_defs[green_idx];
        let blue = &group.property_defs[blue_idx];

        let check_type = |name, ty: PropertyType| {
            if ty.is_list() {
                return Err(invalid_input!(
                    "property '{}' (element '{}') is a list (should be scalar 'uchar')",
                    name,
                    group.name,
                ));
            }

            if ty.scalar_type() != ScalarType::UChar {
                return Err(invalid_input!(
                    "property '{}' (element '{}') has type '{}' (should be 'uchar')",
                    name,
                    group.name,
                    ty.scalar_type().ply_type_name(),
                ));
            }

            Ok(())
        };

        check_type("red", red.ty)?;
        check_type("green", green.ty)?;
        check_type("blue", blue.ty)?;

        let rgb_contiguous = red_idx.0 + 1 == green_idx.0 && green_idx.0 + 1 == blue_idx.0;

        let out = match (group.prop_pos("alpha"), rgb_contiguous) {
            (Some(alpha_idx), _) => {
                let alpha = &group.property_defs[alpha_idx];
                check_type("alpha", alpha.ty)?;

                if rgb_contiguous && blue_idx.0 + 1 == alpha_idx.0 {
                    Self::ContiguousRgba(red_idx)
                } else {
                    Self::SeparateRgba([red_idx, green_idx, blue_idx, alpha_idx])
                }
            }
            (None, true) => Self::ContiguousRgb(red_idx),
            (None, false) => Self::SeparateRgb([red_idx, green_idx, blue_idx]),
        };

        Ok(Some(out))
    }

    // TODO: this neeeds to be removed at some point
    fn into_dumb(&self) -> ([PropIndex; 4], bool) {
        match *self {
            Self::ContiguousRgb(r)
                => ([r, PropIndex(r.0 + 1), PropIndex(r.0 + 2), PropIndex(0)], false),
            Self::SeparateRgb([r, g, b]) => ([r, g, b, PropIndex(0)], false),
            Self::ContiguousRgba(r)
                => ([r, PropIndex(r.0 + 1), PropIndex(r.0 + 2), PropIndex(r.0 + 3)], true),
            Self::SeparateRgba(indices) => (indices, true),
        }
    }
}


// ================================================================================================
// ===== Other stuff
// ================================================================================================

// The names we accepts for vertex, face and edge elements, respectively.
const VERTEX_ELEMENT_NAMES: &[&str] = &["vertex", "point"];
const FACE_ELEMENT_NAMES: &[&str] = &["face"];
const EDGE_ELEMENT_NAMES: &[&str] = &["edge"];

// Tries to convert the given `u64` value to a `hsize`. If the value is too
// large, a descriptive `InvalidInput` error is returned.
fn u64_to_hsize(v: u64, elem: &str) -> Result<hsize, Error> {
    v.try_into().map_err(|_| invalid_input!(
        "too many {} (LOX meshes can only contain 2^32 elements, \
            unless you enabled the 'large-handle' feature)",
        elem,
    ))
}


// ================================================================================================
// ===== Raw sink for `Reader::transfer_to`
// ================================================================================================


/// Helper type alias for property handler functions.
type FnPropHandler<S> = fn(&mut S, &RawElement);

/// A raw sink that transfers the raw data to the given `MemSink`. This is the
/// core of `Reader::transfer_to`.
struct RawTransferSink<'a, S: MemSink> {
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


/// Abstracts over N-dimensional properties (currently only 3 or 4).
trait PropLayout {
    type Scalar: Primitive + FromBytes;
    /// An array of `PropIndex` elements with length N.
    type Idx;
    /// An array of `Scalar` elements with length N.
    type Out;

    /// Stupid helper function to extract the first index from `Idx`. This could
    /// be avoided if array would implement `Index`.
    fn first(idx: &Self::Idx) -> PropIndex;
    /// Maps the given array `idx` with the function `f`. Calls `f` N times.
    fn map(idx: &Self::Idx, f: impl FnMut(PropIndex, usize) -> Self::Scalar) -> Self::Out;
}

/// A 3 dimensional property.
struct Vec3Layout<T>(!, PhantomData<T>);
impl<T: Primitive + FromBytes> PropLayout for Vec3Layout<T> {
    type Scalar = T;
    type Idx = [PropIndex; 3];
    type Out = [T; 3];
    fn first(idx: &Self::Idx) -> PropIndex {
        idx[0]
    }
    fn map(idx: &Self::Idx, mut f: impl FnMut(PropIndex, usize) -> Self::Scalar) -> Self::Out {
        [f(idx[0], 0), f(idx[1], 1), f(idx[2], 2)]
    }
}

/// A 4 dimensional property.
struct Vec4Layout<T>(!, PhantomData<T>);
impl<T: Primitive + FromBytes> PropLayout for Vec4Layout<T> {
    type Scalar = T;
    type Idx = [PropIndex; 4];
    type Out = [T; 4];
    fn first(idx: &Self::Idx) -> PropIndex {
        idx[0]
    }
    fn map(idx: &Self::Idx, mut f: impl FnMut(PropIndex, usize) -> Self::Scalar) -> Self::Out {
        [f(idx[0], 0), f(idx[1], 1), f(idx[2], 2), f(idx[3], 3)]
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
        Provider: IdxProvider<Prop>
    {
        let idxs = provider.idx();
        let offset = elem.prop_infos[Prop::Layout::first(idxs)].offset;
        Prop::Layout::map(idxs, |_, i| {
            <Prop::Layout as PropLayout>::Scalar::from_bytes_ne(
                &elem.data[RawOffset(offset.0 + (i * <Prop::Layout as PropLayout>::Scalar::SIZE) as u32)..]
            )
        })
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
        Prop::Layout::map(provider.idx(), |idx, _| {
            <Prop::Layout as PropLayout>::Scalar::from_bytes_ne(
                &elem.data[elem.prop_infos[idx].offset..]
            )
        })
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
            fn current_handle(&self) -> Self::Handle {
                self.current_handle
            }
        }
    };
}

impl_element_read_state!(VertexReadState, VertexHandle);
impl_element_read_state!(FaceReadState, FaceHandle);
impl_element_read_state!(EdgeReadState, EdgeHandle);

struct VertexReadState {
    count: usize,
    handles: IndexHandleMap<VertexHandle>,
    current_handle: VertexHandle,
    position_idx: [PropIndex; 3],
    normal_idx: [PropIndex; 3],
    color_idx: [PropIndex; 4],
}
struct FaceReadState {
    count: usize,
    current_handle: FaceHandle,
    vertex_indices_idx: PropIndex,
    normal_idx: [PropIndex; 3],
    color_idx: [PropIndex; 4],
}
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
/// TODO
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

/// Part of the reader state.
#[derive(Debug, Clone, Copy)]
enum CurrentElement {
    None,
    Vertex,
    Face,
    Edge,
    Ignored,
}

struct ReadState<'a, S: MemSink> {
    sink: &'a mut S,

    current_element: CurrentElement,

    vertex_state: VertexReadState,
    face_state: FaceReadState,
    edge_state: EdgeReadState,

    // Generic property handler as a list of function pointers.
    vertex_prop_fns: Vec<VertexPropHandler<S>>,
    face_prop_fns: Vec<FacePropHandler<S>>,
    edge_prop_fns: Vec<EdgePropHandler<S>>,

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
}

type VertexPropHandler<S> = fn(&mut S, &RawElement, &VertexReadState);
type FacePropHandler<S> = fn(&mut S, &RawElement, &FaceReadState);
type EdgePropHandler<S> = fn(&mut S, &RawElement, &EdgeReadState);

impl<'a, S: MemSink> ReadState<'a, S> {
    fn new(sink: &'a mut S, info: &ElementInfo) -> Result<Self, Error> {
        // Calls the `prepare_` function and returns the offset as well as the
        // function pointer to the `read_prop` function monomorphized with the
        // correct types.
        macro_rules! prepare_prop {
            (
                $info:expr,
                $prep_fn:ident,
                $prop:ident,
                $count:expr,
                $state:ident,
                $fns_vec:expr,
                $idx:ident $(,)?
            ) => {
                if let Some(Vec3PropInfo { ty, idx }) = $info {
                    let fn_ptr = match ty {
                        ScalarType::Char => {
                            sink.$prep_fn::<i8>($count)?;
                            read_prop::<S, $state, $prop<i8>, SeparateIdx>
                        },
                        ScalarType::UChar => {
                            sink.$prep_fn::<u8>($count)?;
                            read_prop::<S, $state, $prop<u8>, SeparateIdx>
                        },
                        ScalarType::Short => {
                            sink.$prep_fn::<i16>($count)?;
                            read_prop::<S, $state, $prop<i16>, SeparateIdx>
                        },
                        ScalarType::UShort => {
                            sink.$prep_fn::<u16>($count)?;
                            read_prop::<S, $state, $prop<u16>, SeparateIdx>
                        },
                        ScalarType::Int => {
                            sink.$prep_fn::<i32>($count)?;
                            read_prop::<S, $state, $prop<i32>, SeparateIdx>
                        },
                        ScalarType::UInt => {
                            sink.$prep_fn::<u32>($count)?;
                            read_prop::<S, $state, $prop<u32>, SeparateIdx>
                        },
                        ScalarType::Float => {
                            sink.$prep_fn::<f32>($count)?;
                            read_prop::<S, $state, $prop<f32>, SeparateIdx>
                        },
                        ScalarType::Double => {
                            sink.$prep_fn::<f64>($count)?;
                            read_prop::<S, $state, $prop<f64>, SeparateIdx>
                        },
                    };

                    $fns_vec.push(fn_ptr);
                    $idx = idx.indices();
                }
            }
        }

        macro_rules! prepare_color_prop {
            (
                $info:expr,
                $prep_fn:ident,
                $count:expr,
                $state:ident,
                $fns_vec:expr,
                $idx:ident $(,)?
            ) => {
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

                    $fns_vec.push(fn_ptr);
                    $idx = idx;
                }
            }
        }


        // Prepare handlers and indices for generic vertex properties.
        let mut vertex_prop_fns: Vec<VertexPropHandler<S>> = Vec::new();
        let mut vertex_position_idx = [PropIndex(0); 3];
        let mut vertex_normal_idx = [PropIndex(0); 3];
        let mut vertex_color_idx = [PropIndex(0); 4];

        prepare_prop!(
            info.vertex.position,
            prepare_vertex_positions,
            PositionProp,
            info.vertex.count,
            VertexReadState,
            vertex_prop_fns,
            vertex_position_idx,
        );
        prepare_prop!(
            info.vertex.normal,
            prepare_vertex_normals,
            NormalProp,
            info.vertex.count,
            VertexReadState,
            vertex_prop_fns,
            vertex_normal_idx,
        );
        prepare_color_prop!(
            info.vertex.color,
            prepare_vertex_colors,
            info.vertex.count,
            VertexReadState,
            vertex_prop_fns,
            vertex_color_idx,
        );

        // Prepare handlers and indices for generic face properties.
        let mut face_prop_fns: Vec<FacePropHandler<S>> = Vec::new();
        let mut face_normal_idx = [PropIndex(0); 3];
        let mut face_color_idx = [PropIndex(0); 4];
        if let Some(face_info) = info.face.as_ref() {
            prepare_prop!(
                face_info.normal,
                prepare_face_normals,
                NormalProp,
                face_info.count,
                FaceReadState,
                face_prop_fns,
                face_normal_idx,
            );
            prepare_color_prop!(
                face_info.color,
                prepare_face_colors,
                face_info.count,
                FaceReadState,
                face_prop_fns,
                face_color_idx,
            );
        }

        // Prepare handlers and indices for generic edge properties.
        let mut edge_prop_fns: Vec<EdgePropHandler<S>> = Vec::new();
        let mut edge_color_idx = [PropIndex(0); 4];
        if let Some(edge_info) = info.edge.as_ref() {
            prepare_color_prop!(
                edge_info.color,
                prepare_edge_colors,
                edge_info.count,
                EdgeReadState,
                edge_prop_fns,
                edge_color_idx,
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


        Ok(Self {
            sink,
            current_element: CurrentElement::None,

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

            vertex_prop_fns,
            face_prop_fns,
            edge_prop_fns,

            read_face_vertex_indices,
            vertex_handles_buffer: Vec::new(),
            read_edge_endpoint_indices,
        })
    }

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

impl<S: MemSink> RawSink for ReadState<'_, S> {
    fn element_group_start(&mut self, def: &ElementDef) -> Result<(), Error> {
        self.current_element = match &*def.name {
            "vertex" => CurrentElement::Vertex,
            "face" => CurrentElement::Face,
            "edge" => CurrentElement::Edge,
            _ => CurrentElement::Ignored,
        };

        Ok(())
    }

    fn element(&mut self, elem: &RawElement) -> Result<(), Error> {
        match self.current_element {
            CurrentElement::None => {
                panic!(
                    "bug in `ply::Reader::read_raw`: `element()` called \
                        before `element_group_start()`"
                );
            }
            CurrentElement::Ignored => {}
            CurrentElement::Vertex => {
                // Add vertex
                let vh = self.sink.add_vertex();
                self.vertex_state.handles.add(self.vertex_state.count, vh);
                self.vertex_state.count += 1;
                self.vertex_state.current_handle = vh;

                // Read vertex properties
                for f in &self.vertex_prop_fns {
                    f(&mut self.sink, elem, &self.vertex_state);
                }
            }
            CurrentElement::Face => {
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
                for f in &self.face_prop_fns {
                    f(&mut self.sink, elem, &self.face_state);
                }
            }
            CurrentElement::Edge => {
                // Retrieve edge
                let eh = (self.read_edge_endpoint_indices)(self, elem)?;
                self.edge_state.count += 1;
                self.edge_state.current_handle = eh;

                // Read generic edge properties
                for f in &self.edge_prop_fns {
                    f(&mut self.sink, elem, &self.edge_state);
                }
            }
        }

        Ok(())
    }
}




impl<'a, S: MemSink> RawTransferSink<'a, S> {
    fn new(sink: &'a mut S, info: ElementInfo) -> Result<Self, Error> {
        // Calls the `prepare_` function and returns the offset as well
        // as the function pointer to the function monomorphized with
        // the correct type.
        macro_rules! get_offset_and_fptr {
            ($info:expr, $fun:ident, $prep_fn:ident, $count:expr $(,)?) => {
                match $info {
                    Some(Vec3PropInfo { idx, ty }) => {
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

                        (idx.indices(), fn_ptr)
                    }
                    None => ([PropIndex(0); 3], Self::noop as FnPropHandler<Self>),
                }
            }
        }

        // For vertex properties x, y, z.
        let (vertex_position_idx, read_vertex_position) = get_offset_and_fptr!(
            info.vertex.position,
            read_vertex_position,
            prepare_vertex_positions,
            info.vertex.count,
        );

        // For vertex properties nx, ny, nz.
        let (vertex_normal_idx, read_vertex_normal) = get_offset_and_fptr!(
            info.vertex.normal,
            read_vertex_normal,
            prepare_vertex_normals,
            info.vertex.count,
        );

        // For vertex properties red, green, blue, [alpha]
        let (vertex_color_idx, read_vertex_color) = match info.vertex.color.map(|i| i.into_dumb()) {
            Some((offset, alpha)) => {
                let fun = match alpha {
                    true => {
                        sink.prepare_vertex_colors::<[u8; 4]>(info.vertex.count)?;
                        Self::read_vertex_color::<[u8; 4]> as FnPropHandler<Self>
                    },
                    false => {
                        sink.prepare_vertex_colors::<[u8; 3]>(info.vertex.count)?;
                        Self::read_vertex_color::<[u8; 3]> as FnPropHandler<Self>
                    },
                };
                (offset, fun)
            }
            None => ([PropIndex(0); 4], Self::noop as FnPropHandler<Self>),
        };


        // For property `vertex_indices`
        let (vertex_indices_idx, read_face_vertex_indices) = match info.face.as_ref().map(|f| f.vertex_indices) {
            Some(VertexIndicesInfo { idx, ty }) => {
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

                (idx, fun)
            }
            None => (
                PropIndex(0),
                Self::vertex_indices_bug as fn(&mut _, &_, &_) -> _,
            ),
        };

        // For face properties nx, ny, nz.
        let (face_normal_idx, read_face_normal) = get_offset_and_fptr!(
            info.face.as_ref().and_then(|f| f.normal),
            read_face_normal,
            prepare_face_normals,
            info.face.as_ref().unwrap().count,
        );

        // For face properties red, green, blue, [alpha]
        let (face_color_idx, read_face_color) = match info.face.as_ref().and_then(|f| f.color.map(|i| i.into_dumb())) {
            Some((offset, alpha)) => {
                let fun = if alpha {
                    sink.prepare_face_colors::<[u8; 4]>(info.face.as_ref().unwrap().count)?;
                    Self::read_face_color::<[u8; 4]> as FnPropHandler<Self>
                } else {
                    sink.prepare_face_colors::<[u8; 3]>(info.face.as_ref().unwrap().count)?;
                    Self::read_face_color::<[u8; 3]> as FnPropHandler<Self>
                };
                (offset, fun)
            }
            None => ([PropIndex(0); 4], Self::noop as FnPropHandler<Self>),
        };

        // For property `vertex_indices`
        let (endpoint_indices_idx, read_edge_endpoint_indices) = match info.edge.as_ref().map(|e| e.endpoints) {
            Some(EdgeEndpointsInfo { idx, ty }) => {
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

                (idx, fun)
            }
            None => (
                [PropIndex(0); 2],
                Self::edge_endpoints_bug as fn(&mut _, &_) -> _,
            ),
        };

        // For edge properties red, green, blue, [alpha]
        let (edge_color_idx, read_edge_color) = match info.edge.as_ref().and_then(|e| e.color.map(|i| i.into_dumb())) {
            Some((offset, alpha)) => {
                let fun = if alpha {
                    sink.prepare_edge_colors::<[u8; 4]>(info.edge.as_ref().unwrap().count)?;
                    Self::read_edge_color::<[u8; 4]> as FnPropHandler<Self>
                } else {
                    sink.prepare_edge_colors::<[u8; 3]>(info.edge.as_ref().unwrap().count)?;
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
            elem_handler: RawTransferSink::bug_in_read_raw,

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

impl<S: MemSink> RawSink for RawTransferSink<'_, S> {
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
