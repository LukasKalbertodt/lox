//! Parsing PLY files.
//!
//! Random notes on parsing:
//!
//! - The "specification" talks about "carriage-return terminated lines", but
//!   this is incorrect, as in: all files I could find end their lines with
//!   '\n' (0x0A) and not '\r' (0x0D). This includes the example file linked in
//!   the specification! So in this file we use `parse::whitespace` which just
//!   works with '\n'.
//!
//!

#![allow(unused_imports)] // TODO

use std::{
    cmp::min,
    fmt,
    fs::File,
    marker::PhantomData,
    io,
    ops,
    path::Path,
    str::FromStr,
};

use byteorder::{ByteOrder, LittleEndian, NativeEndian, ReadBytesExt, WriteBytesExt};
use cgmath::{Point3, Vector3};
use derive_more::{Add, AddAssign, Deref, DerefMut, From, Sub, SubAssign};
use failure::Fail;
use smallvec::SmallVec;

use crate::{
    self as lox, // for proc macros
    prelude::*,
    io::{
        StreamSource, MemSink, Primitive, Error,
        parse::{self, ParseError, Span, SpannedData, Buffer, ParseBuf},
        util::IndexHandleMap,
    },
    util::debug_fmt_bytes,
};
use super::Encoding;


// ----------------------------------------------------------------------------

// ===========================================================================
// ===== Definition of `Reader`
// ===========================================================================

/// A reader able to read PLY files.
///
/// You can create a reader with [`Reader::open`] or [`Reader::new`]. TODO
#[derive(Debug)]
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
            match e {
                Error::Parse(_) => ParseError::Custom(
                    "not a valid PLY file (does not start with \"ply\\n\")".into(),
                    Span::new(0, 4),
                ).into(),
                other => other,
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
    /// the file).
    pub fn comments(&self) -> &[String] {
        &self.comments
    }

    /// Reads the whole file into a [`RawResult`].
    pub fn into_raw_result(self) -> Result<RawResult, Error> {
        let mut out = RawResult::empty();
        self.read_raw_into(&mut out)?;
        Ok(out)
    }

    /// Reads the whole file into the given raw sink.
    ///
    /// This is a low level building block that you usually don't want to use
    /// directly.
    pub fn read_raw_into(mut self, sink: &mut impl RawSink) -> Result<(), Error> {
        let buf = &mut self.buf;

        let read_element = match self.encoding {
            Encoding::BinaryBigEndian => read_element_bbe::<Buffer<R>>,
            Encoding::BinaryLittleEndian => read_element_ble::<Buffer<R>>,
            Encoding::Ascii => read_element_ascii::<Buffer<R>>,
        };

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

            // Just read as many elements as specfied in the header. A faulty
            // number in the header won't lead to any DOS-like dangerous
            // things. The time and memory we use here is still limited by the
            // file size.
            for _ in 0..element_def.count {
                elem.data.clear();
                read_element(buf, &index, &mut elem)?;

                // Send read properties to the sink.
                sink.element(&elem)?;
            }
        }

        Ok(())
    }
}

impl<R: io::Read> StreamSource for Reader<R> {
    fn transfer_to<S: MemSink>(self, sink: &mut S) -> Result<(), Error> {
        // This function is rather complicated. Why? Speed. Hopefully.
        //
        // The the basic idea is the following:
        // - First, do some sanity checks and extract useful information for
        //   properties we want to read (like the position of that property).
        //   This is done directly in this function further below.
        // - Define a `RawSink` that receives the data via `read_raw_into`
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
        struct PropInfo {
            vertex_position: Option<([PropIndex; 3], ScalarType)>,
            vertex_normal: Option<([PropIndex; 3], ScalarType)>,
            vertex_indices: Option<(PropIndex, ScalarType)>,
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


            // ----- State for face handling -----
            face_handles: IndexHandleMap<FaceHandle>,
            face_count: usize,

            /// The position of the `vertex_indices` property in the list of
            /// face properties (usually 0)
            vertex_indices_idx: PropIndex,

            /// Function to read the `vertex_indices` property and add the
            /// face. Returns `Err(idx)` if an vertex index is not valid.
            read_face_vertex_indices:
                fn(&mut Self, RawOffset, &RawData) -> Result<FaceHandle, usize>,

        }

        impl<'a, S: MemSink> HelperSink<'a, S> {
            fn new(sink: &'a mut S, info: PropInfo) -> Self {
                // Return the function pointer of the method `$fun`
                // instantiated with the primitive type `$ty`.
                macro_rules! typed_fn_ptr {
                    ($ty:ident, $fun:ident) => {
                        match $ty {
                            ScalarType::Char => Self::$fun::<i8>,
                            ScalarType::UChar => Self::$fun::<u8>,
                            ScalarType::Short => Self::$fun::<i16>,
                            ScalarType::UShort => Self::$fun::<u16>,
                            ScalarType::Int => Self::$fun::<i32>,
                            ScalarType::UInt => Self::$fun::<u32>,
                            ScalarType::Float => Self::$fun::<f32>,
                            ScalarType::Double => Self::$fun::<f64>,
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
                        Self::vertex_indices_bug as fn(&mut _, _, &_) -> _,
                    ),
                };

                // For vertex properties x, y, z.
                let (vertex_position_idx, read_vertex_position) = match info.vertex_position {
                    Some((offset, ty)) => (offset, typed_fn_ptr!(ty, read_vertex_position)),
                    None => ([PropIndex(0); 3], Self::noop as FnPropHandler<Self>),
                };

                // For vertex properties nx, ny, nz.
                let (vertex_normal_idx, read_vertex_normal) = match info.vertex_normal {
                    Some((offset, ty)) => (offset, typed_fn_ptr!(ty, read_vertex_normal)),
                    None => ([PropIndex(0); 3], Self::noop as FnPropHandler<Self>),
                };


                Self {
                    sink: sink,

                    // It will be overwritten in `element_group_start`, but if
                    // `read_raw_into` is buggy, this function might be called,
                    // so we will panic in that case.
                    elem_handler: HelperSink::bug_in_read_raw,

                    // We initialize the handles to dummy values. They are
                    // overwritten in `handle_vertex` and `handle_face`.
                    curr_vertex_handle: VertexHandle::from_usize(0),
                    curr_face_handle: FaceHandle::from_usize(0),

                    vertex_handles: IndexHandleMap::new(),
                    vertex_count: 0,

                    vertex_position_idx,
                    read_vertex_position,

                    vertex_normal_idx,
                    read_vertex_normal,

                    face_handles: IndexHandleMap::new(),
                    face_count: 0,

                    vertex_indices_idx,
                    read_face_vertex_indices,
                }
            }


            // ----- Element handler ------------------------------------------
            /// Initial element handler. Will hopefully never be called.
            fn bug_in_read_raw(&mut self, _: &RawElement) -> Result<(), Error> {
                panic!(
                    "bug in `ply::Reader::read_raw_into`: `element()` called \
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

                Ok(())
            }

            /// Called for each element in the `face` group.
            fn handle_face(&mut self, elem: &RawElement) -> Result<(), Error> {
                // We checked before that this property is indeed a list
                let vi_list = elem.decode_list_at(self.vertex_indices_idx).unwrap();

                if vi_list.list_len != 3 {
                    return Err(Error::InvalidInput(format!(
                        "the face at position {} has {} vertices (right now, only triangular \
                            faces are supported)",
                        self.face_count,
                        vi_list.list_len,
                    )));
                }

                // Add face
                let fh = (self.read_face_vertex_indices)(self, vi_list.data_offset, &elem.data)
                    .map_err(|idx| {
                        Error::InvalidInput(format!("invalid vertex index {} in PLY file", idx))
                    })?;
                self.face_handles.add(self.face_count, fh);
                self.face_count += 1;
                self.curr_face_handle = fh;

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

            // ----- face property handler ----------------------------------
            /// Only dummy function that should never be called.
            fn vertex_indices_bug(
                &mut self,
                _: RawOffset,
                _: &RawData,
            ) -> Result<FaceHandle, usize> {
                panic!("internal bug in PLY reader")
            }

            /// Reads three values from the raw data at the specified offset,
            /// interprets them as vertex indices and adds a face to the sink.
            fn read_face_vertex_indices<T: Primitive + FromBytes + PlyInteger>(
                &mut self,
                offset: RawOffset,
                data: &RawData,
            ) -> Result<FaceHandle, usize> {
                let a = T::from_bytes_ne(&data[offset + RawOffset(0 * T::SIZE as u32)..]);
                let b = T::from_bytes_ne(&data[offset + RawOffset(1 * T::SIZE as u32)..]);
                let c = T::from_bytes_ne(&data[offset + RawOffset(2 * T::SIZE as u32)..]);

                let [a, b, c] = [a.as_usize(), b.as_usize(), c.as_usize()];
                let vhs = [
                    self.vertex_handles.get(a).ok_or(a)?,
                    self.vertex_handles.get(b).ok_or(b)?,
                    self.vertex_handles.get(c).ok_or(c)?,
                ];
                Ok(self.sink.add_face(vhs))
            }
        }

        impl<S: MemSink> RawSink for HelperSink<'_, S> {
            fn element_group_start(&mut self, def: &ElementDef) -> Result<(), Error> {
                match &*def.name {
                    "vertex" => self.elem_handler = Self::handle_vertex,
                    "face" => self.elem_handler = Self::handle_face,
                    _ => self.elem_handler = Self::ignore_elem,
                }

                Ok(())
            }

            fn element(&mut self, elem: &RawElement) -> Result<(), Error> {
                (self.elem_handler)(self, elem)
            }
        }


        // ===== Interpret and collect header information (and do checks) =====
        let mut prop_info = PropInfo {
            vertex_position: None,
            vertex_normal: None,
            vertex_indices: None,
        };

        // Make sure the file contains vertices
        let vertex_pos = self.elements.iter().position(|e| e.name == "vertex")
            .ok_or_else(|| Error::InvalidInput("no 'vertex' elements in PLY file".into()))?;
        let vertex_group = &self.elements[vertex_pos];

        // Check vertex position
        if let Some(px_idx) = vertex_group.prop_pos("x") {
            let py_idx = vertex_group.prop_pos("y").ok_or(Error::InvalidInput(
                "vertex has 'x' property, but no 'y' property (only 3D positions supported)".into()
            ))?;
            let pz_idx = vertex_group.prop_pos("z").ok_or(Error::InvalidInput(
                "vertex has 'x' property, but no 'z' property (only 3D positions supported)".into()
            ))?;

            let px = &vertex_group.property_defs[px_idx];
            let py = &vertex_group.property_defs[py_idx];
            let pz = &vertex_group.property_defs[pz_idx];

            if px.ty.is_list() {
                return Err(Error::InvalidInput(
                    "vertex property 'x' has a list type (only scalars allowed)".into()
                ));
            }

            if px.ty != py.ty || px.ty != pz.ty {
                return Err(Error::InvalidInput(
                    "vertex properties 'x', 'y' and 'z' don't have the same type".into()
                ));
            }

            prop_info.vertex_position = Some((
                [px_idx, py_idx, pz_idx],
                px.ty.scalar_type(),
            ))
        }

        // Check vertex normal
        if let Some(pnx_idx) = vertex_group.prop_pos("nx") {
            let pny_idx = vertex_group.prop_pos("ny").ok_or(Error::InvalidInput(
                "vertex has 'nx' property, but no 'ny' property (only 3D normals supported)".into()
            ))?;
            let pnz_idx = vertex_group.prop_pos("nz").ok_or(Error::InvalidInput(
                "vertex has 'nx' property, but no 'nz' property (only 3D normals supported)".into()
            ))?;

            let pnx = &vertex_group.property_defs[pnx_idx];
            let pny = &vertex_group.property_defs[pny_idx];
            let pnz = &vertex_group.property_defs[pnz_idx];

            if pnx.ty.is_list() {
                return Err(Error::InvalidInput(
                    "vertex property 'nx' has a list type (only scalars allowed)".into()
                ));
            }

            if pnx.ty != pny.ty || pnx.ty != pnz.ty {
                return Err(Error::InvalidInput(
                    "vertex properties 'nx', 'ny' and 'nz' don't have the same type".into()
                ));
            }

            prop_info.vertex_normal = Some((
                [pnx_idx, pny_idx, pnz_idx],
                pnx.ty.scalar_type(),
            ))
        }

        // Check faces
        if let Some(face_pos) = self.elements.iter().position(|e| e.name == "face") {
            // Faces can only be in the file after vertices
            if face_pos < vertex_pos {
                return Err(Error::InvalidInput(
                    "found 'face' elements before 'vertex' elements (that's now allowed)".into()
                ));
            }

            let face_group = &self.elements[face_pos];

            // The property `vertex_indices` is required
            if let Some(vi_idx) = face_group.prop_pos("vertex_indices") {
                let vi = &face_group.property_defs[vi_idx];
                if !vi.ty.is_list() {
                    return Err(Error::InvalidInput(
                        "'vertex_indices' property has a scalar type (must be a list)".into()
                    ));
                }

                if vi.ty.scalar_type().is_floating_point() {
                    return Err(Error::InvalidInput(
                        "'vertex_indices' list has a floating point element type (only \
                            integers are allowed)".into()
                    ));
                }

                prop_info.vertex_indices = Some((vi_idx, vi.ty.scalar_type()));
            } else {
                return Err(Error::InvalidInput(
                    "'face' elements without 'vertex_indices' property".into()
                ));
            }
        }

        // ===== Read the data through our helper sink =====
        // TODO: size hint
        let mut helper_sink = HelperSink::new(sink, prop_info);
        self.read_raw_into(&mut helper_sink)
    }
}


// ===========================================================================
// ===== `RawElement` definitions
// ===========================================================================

/// Represents on element with all its property values.
///
/// The property values are encoded as a densely packed byte array. This type
/// also includes meta data about the properties. These metadata are mostly the
/// same for all elements of one element group.
#[derive(Clone)]
pub struct RawElement {
    /// The packed data of all properties in native endianess.
    pub data: RawData,

    /// Some meta information about each property in this element.
    pub prop_infos: PropVec<RawPropertyInfo>,
}

/// Metadata about a property.
#[derive(Debug, Clone)]
pub struct RawPropertyInfo {
    /// The byte offset in the packed, native-endian data at which this
    /// property starts.
    pub offset: RawOffset,

    /// The type of this property.
    pub ty: PropertyType,

    /// Name of this property.
    pub name: String,
}

impl RawElement {
    /// Decodes the list at the given property index and returns information
    /// about that list. Returns `None` if there is a scalar value at the given
    /// position. Panics if `idx` is invalid.
    fn decode_list_at(&self, idx: PropIndex) -> Option<RawListInfo> {
        match self.prop_infos[idx].ty {
            PropertyType::Scalar(_) => None,
            PropertyType::List { len_type, scalar_type } => {
                let len_offset = self.prop_infos[idx].offset;
                let list_len = match len_type {
                    ListLenType::UChar => self.data[len_offset] as u32,
                    ListLenType::UShort => NativeEndian::read_u16(&self.data[len_offset..]) as u32,
                    ListLenType::UInt => NativeEndian::read_u32(&self.data[len_offset..]),
                };

                Some(RawListInfo {
                    list_len,
                    len_offset,
                    data_offset: len_offset + len_type.len(),
                    len_type,
                    scalar_type,
                })
            }
        }
    }

    /// Decodes the property at the given index and returns it as dynamically
    /// typed `Property` value.
    fn prop_at(&self, idx: PropIndex) -> Property {
        fn read_scalar(buf: &[u8], ty: ScalarType) -> Property {
            match ty {
                ScalarType::Char => Property::Char(buf[0] as i8),
                ScalarType::UChar => Property::UChar(buf[0]),
                ScalarType::Short => Property::Short(NativeEndian::read_i16(buf)),
                ScalarType::UShort => Property::UShort(NativeEndian::read_u16(buf)),
                ScalarType::Int => Property::Int(NativeEndian::read_i32(buf)),
                ScalarType::UInt => Property::UInt(NativeEndian::read_u32(buf)),
                ScalarType::Float => Property::Float(NativeEndian::read_f32(buf)),
                ScalarType::Double => Property::Double(NativeEndian::read_f64(buf)),
            }
        }

        let info = &self.prop_infos[idx];

        match info.ty {
            PropertyType::Scalar(ty) => read_scalar(&self.data[info.offset..], ty),
            PropertyType::List { scalar_type, .. } => {
                let list_info = self.decode_list_at(idx).unwrap();

                macro_rules! list {
                    ($variant:ident, |$buf:ident| $e:expr) => {{
                        let mut list = SmallVec::new();
                        let mut offset = list_info.data_offset;
                        for _ in 0..list_info.list_len {
                            let $buf = &self.data[offset..];
                            list.push($e);
                            offset += list_info.scalar_type.len();
                        }

                        Property::$variant(list)
                    }}
                }

                match scalar_type {
                    ScalarType::Char => list!(CharList, |buf| buf[0] as i8),
                    ScalarType::UChar => list!(UCharList, |buf| buf[0]),
                    ScalarType::Short => list!(ShortList, |buf| NativeEndian::read_i16(buf)),
                    ScalarType::UShort => list!(UShortList, |buf| NativeEndian::read_u16(buf)),
                    ScalarType::Int => list!(IntList, |buf| NativeEndian::read_i32(buf)),
                    ScalarType::UInt => list!(UIntList, |buf| NativeEndian::read_u32(buf)),
                    ScalarType::Float => list!(FloatList, |buf| NativeEndian::read_f32(buf)),
                    ScalarType::Double => list!(DoubleList, |buf| NativeEndian::read_f64(buf)),
                }
            }
        }
    }

    /// Returns an iterator over all properties of this element.
    pub fn iter(&self) -> RawElementIter<'_> {
        RawElementIter {
            elem: self,
            idx: 0.into(),
        }
    }
}

impl fmt::Debug for RawElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = f.debug_struct("RawElement");

        for (prop, info) in self.iter().zip(self.prop_infos.iter()) {
            // TODO: maybe add offset
            s.field(&info.name, &prop);
        }

        s.finish()
    }
}

/// Iterator over the property values of a [`RawElement`]. Can be obtained via
/// [`RawElement::iter`].
#[derive(Debug)]
pub struct RawElementIter<'a> {
    elem: &'a RawElement,
    idx: PropIndex,
}

impl Iterator for RawElementIter<'_> {
    type Item = Property;
    fn next(&mut self) -> Option<Self::Item> {
        if self.elem.prop_infos.len() <= self.idx.as_usize() {
            None
        } else {
            let out = self.elem.prop_at(self.idx);
            self.idx += PropIndex::from(1);
            Some(out)
        }
    }
}

/// Meta data about a decoded list.
struct RawListInfo {
    /// Number of elements in the list.
    pub list_len: u32,

    /// The byte offset of the length field in the [`RawElement`] this list is
    /// stored in.
    pub len_offset: RawOffset,

    /// The byte offset of the first list element in the [`RawElement`] this
    /// list is stored in.
    pub data_offset: RawOffset,

    /// The type in which the list length is stored.
    pub len_type: ListLenType,

    /// The type of the list's elements.
    pub scalar_type: ScalarType,
}


// ===========================================================================
// ===== Strongly typed wrapper for certain things
// ===========================================================================

/// A byte offset into the raw data of one element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Add, Sub, AddAssign, SubAssign, From)]
pub struct RawOffset(pub u32);

impl RawOffset {
    fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

impl From<ScalarLen> for RawOffset {
    fn from(src: ScalarLen) -> Self {
        (src.as_u8() as u32).into()
    }
}

impl ops::Add<ScalarLen> for RawOffset {
    type Output = RawOffset;
    fn add(self, len: ScalarLen) -> Self::Output {
        (self.0 + len.as_u8() as u32).into()
    }
}

impl ops::AddAssign<ScalarLen> for RawOffset {
    fn add_assign(&mut self, rhs: ScalarLen) {
        *self = *self + rhs;
    }
}

/// Length of an STL scalar value in bytes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarLen {
    One = 1,
    Two = 2,
    Four = 4,
    Eight = 8,
}

impl ScalarLen {
    fn as_u8(&self) -> u8 {
        *self as u8
    }
}

/// Index of a specific property in the ordered list of properties of one
/// element group. Can be used to index a [`PropVec`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, From, Add, Sub, AddAssign, SubAssign)]
pub struct PropIndex(pub u8);

impl PropIndex {
    fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

/// Raw data of one element in native endianess. Can be indexed by `RawOffset`.
///
/// For PLY files stored in native endianess, this is an exact chunk from
/// the file. For ASCII files and files in non-native endianess, the
/// properties are first converted to this format.
#[derive(Debug, Clone, From)]
pub struct RawData(Vec<u8>);

impl ops::Index<RawOffset> for RawData {
    type Output = u8;
    fn index(&self, idx: RawOffset) -> &Self::Output {
        &self.0[idx.as_usize()]
    }
}

impl ops::Index<ops::Range<RawOffset>> for RawData {
    type Output = [u8];
    fn index(&self, range: ops::Range<RawOffset>) -> &Self::Output {
        &self.0[range.start.as_usize()..range.end.as_usize()]
    }
}

impl ops::Index<ops::RangeFrom<RawOffset>> for RawData {
    type Output = [u8];
    fn index(&self, range: ops::RangeFrom<RawOffset>) -> &Self::Output {
        &self.0[range.start.as_usize()..]
    }
}

impl ops::Index<ops::RangeTo<RawOffset>> for RawData {
    type Output = [u8];
    fn index(&self, range: ops::RangeTo<RawOffset>) -> &Self::Output {
        &self.0[..range.end.as_usize()]
    }
}

impl ops::Index<ops::RangeFull> for RawData {
    type Output = [u8];
    fn index(&self, _: ops::RangeFull) -> &Self::Output {
        &self.0[..]
    }
}

impl ops::IndexMut<RawOffset> for RawData {
    fn index_mut(&mut self, idx: RawOffset) -> &mut Self::Output {
        &mut self.0[idx.as_usize()]
    }
}

impl ops::IndexMut<ops::Range<RawOffset>> for RawData {
    fn index_mut(&mut self, range: ops::Range<RawOffset>) -> &mut Self::Output {
        &mut self.0[range.start.as_usize()..range.end.as_usize()]
    }
}

impl ops::IndexMut<ops::RangeFrom<RawOffset>> for RawData {
    fn index_mut(&mut self, range: ops::RangeFrom<RawOffset>) -> &mut Self::Output {
        &mut self.0[range.start.as_usize()..]
    }
}

impl ops::IndexMut<ops::RangeTo<RawOffset>> for RawData {
    fn index_mut(&mut self, range: ops::RangeTo<RawOffset>) -> &mut Self::Output {
        &mut self.0[..range.end.as_usize()]
    }
}

impl ops::IndexMut<ops::RangeFull> for RawData {
    fn index_mut(&mut self, _: ops::RangeFull) -> &mut Self::Output {
        &mut self.0[..]
    }
}

impl ops::Deref for RawData {
    type Target = Vec<u8>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ops::DerefMut for RawData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// A vector that holds data for each property. Can be indexed by
/// [`PropIndex`].
///
/// This is simply a wrapper around a `Vec` to use strong typing. This should
/// only be indexed with `PropIndex`.
#[derive(Debug, Clone, From)]
pub struct PropVec<T>(Vec<T>);

impl<T> ops::Index<PropIndex> for PropVec<T> {
    type Output = T;
    fn index(&self, idx: PropIndex) -> &Self::Output {
        &self.0[idx.as_usize()]
    }
}

impl<T> ops::Deref for PropVec<T> {
    type Target = Vec<T>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> ops::DerefMut for PropVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
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
            out.extend_from_slice(&b.data);
            Ok(b.data[offset] as u32)
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
                // Read the list lenght. The type of this property is a list
                // (the index says so), so we can unwrap here.
                let list_len = read_binary_len(
                    buf,
                    prop_info.ty.len_type().unwrap(),
                    &mut raw_elem.data,
                    |_| {}, // no byte swapping
                )?;
                offset += len_len;

                RawOffset::from(list_len * (scalar_len.as_u8() as u32))
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
                let data_len = list_len * (scalar_len as u32);
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
            fn from_bytes_ne(bytes: &[u8]) -> Self {
                NativeEndian::$read_fun(bytes)
            }
            fn encode_ne(&self, out: &mut Vec<u8>) {
                out.$write_fun::<NativeEndian>(*self).unwrap();
            }
        }
    }
}

impl FromBytes for i8 {
    const SIZE: usize = 1;
    fn from_bytes_ne(bytes: &[u8]) -> Self {
        bytes[0] as i8
    }
    fn encode_ne(&self, out: &mut Vec<u8>) {
        out.push(*self as u8);
    }
}

impl FromBytes for u8 {
    const SIZE: usize = 1;
    fn from_bytes_ne(bytes: &[u8]) -> Self {
        bytes[0]
    }
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


// ===========================================================================
// ===== Data structures to hold header and body data of a PLY file
// ===========================================================================
/// The header definition of one element group.
#[derive(Debug, Clone)]
pub struct ElementDef {
    pub name: String,

    /// Number of elements in this group.
    pub count: u64,

    /// Definitions for all properties of elements in this group.
    pub property_defs: PropVec<PropertyDef>,
}

impl ElementDef {
    fn prop_pos(&self, prop_name: &str) -> Option<PropIndex> {
        self.property_defs.iter()
            .position(|p| p.name == prop_name)
            .map(|idx| PropIndex(idx as u8))
    }
}

/// Te header definition of one property of an element.
#[derive(Debug, Clone)]
pub struct PropertyDef {
    pub ty: PropertyType,
    pub name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PropertyType {
    Scalar(ScalarType),
    List {
        len_type: ListLenType,
        scalar_type: ScalarType,
    }
}

impl PropertyType {
    fn len_type(&self) -> Option<ListLenType> {
        match self {
            PropertyType::Scalar(_) => None,
            PropertyType::List { len_type, .. } => Some(*len_type),
        }
    }

    fn scalar_type(&self) -> ScalarType {
        match *self {
            PropertyType::Scalar(scalar_type) => scalar_type,
            PropertyType::List { scalar_type, .. } => scalar_type,
        }
    }

    fn is_list(&self) -> bool {
        self.len_type().is_some()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ListLenType {
    UChar,
    UShort,
    UInt,
}

impl ListLenType {
    pub fn from_scalar_type(ty: ScalarType) -> Option<Self> {
        match ty {
            ScalarType::UChar => Some(ListLenType::UChar),
            ScalarType::UShort => Some(ListLenType::UShort),
            ScalarType::UInt => Some(ListLenType::UInt),
            _ => None,
        }
    }

    pub fn to_scalar_type(self) -> ScalarType {
        match self {
            ListLenType::UChar => ScalarType::UChar,
            ListLenType::UShort => ScalarType::UShort,
            ListLenType::UInt => ScalarType::UInt,
        }
    }

    /// Returns the number of bytes this type occupies.
    pub fn len(&self) -> ScalarLen {
        match self {
            ListLenType::UChar => ScalarLen::One,
            ListLenType::UShort => ScalarLen::Two,
            ListLenType::UInt => ScalarLen::Four,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarType {
    Char,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Float,
    Double,
}

impl ScalarType {
    /// Returns `true` if and only if the type is either `float` or `double`.
    pub fn is_floating_point(&self) -> bool {
        *self == ScalarType::Float || *self == ScalarType::Double
    }

    /// Returns `true` if and only if the type is one of `uchar`, `ushort` or
    /// `uint`.
    pub fn is_unsigned_integer(&self) -> bool {
        match self {
            ScalarType::UChar | ScalarType::UShort | ScalarType::UInt => true,
            _ => false,
        }
    }

    /// Returns `true` if and only if the type is one of `char`, `short` or
    /// `int`.
    pub fn is_signed_integer(&self) -> bool {
        match self {
            ScalarType::Char | ScalarType::Short | ScalarType::Int => true,
            _ => false,
        }
    }

    /// Returns the number of bytes this type occupies.
    pub fn len(&self) -> ScalarLen {
        match self {
            ScalarType::Char => ScalarLen::One,
            ScalarType::UChar => ScalarLen::One,
            ScalarType::Short => ScalarLen::Two,
            ScalarType::UShort => ScalarLen::Two,
            ScalarType::Int => ScalarLen::Four,
            ScalarType::UInt => ScalarLen::Four,
            ScalarType::Float => ScalarLen::Four,
            ScalarType::Double => ScalarLen::Eight,
        }
    }
}

impl fmt::Display for ScalarTypeParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\"{}\" is not a valid PLY scalar type", self.0)
    }
}

impl fmt::Debug for ScalarTypeParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

/// The error emitted when the `FromStr` implementation for `ScalarType` cannot
/// parse the given string.
pub struct ScalarTypeParseError(String);

impl FromStr for ScalarType {
    type Err = ScalarTypeParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "char" => Ok(ScalarType::Char),
            "uchar" => Ok(ScalarType::UChar),
            "short" => Ok(ScalarType::Short),
            "ushort" => Ok(ScalarType::UShort),
            "int" => Ok(ScalarType::Int),
            "uint" => Ok(ScalarType::UInt),
            "float" => Ok(ScalarType::Float),
            "double" => Ok(ScalarType::Double),
            other => Err(ScalarTypeParseError(other.to_string())),
        }
    }
}

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

/// One property value of some PLY type.
///
/// The sizes of the smallvecs are choosen so that the inline variant won't
/// inflict a size overhead (on x64). This still means that the most common
/// form of list, the three-tuple `vertex_indices`, will fit inline.
#[derive(Debug, Clone, PartialEq)]
pub enum Property {
    Char(i8),
    UChar(u8),
    Short(i16),
    UShort(u16),
    Int(i32),
    UInt(u32),
    Float(f32),
    Double(f64),
    CharList(SmallVec<[i8; 16]>),
    UCharList(SmallVec<[u8; 16]>),
    ShortList(SmallVec<[i16; 8]>),
    UShortList(SmallVec<[u16; 8]>),
    IntList(SmallVec<[i32; 4]>),
    UIntList(SmallVec<[u32; 4]>),
    FloatList(SmallVec<[f32; 4]>),
    DoubleList(SmallVec<[f64; 2]>),
}

impl Property {
    /// Returns the value as integer, or `None` if the property does not have
    /// an integer type.
    pub fn as_integer(&self) -> Option<i64> {
        match *self {
            Property::Char(v) => Some(v.into()),
            Property::UChar(v) => Some(v.into()),
            Property::Short(v) => Some(v.into()),
            Property::UShort(v) => Some(v.into()),
            Property::Int(v) => Some(v.into()),
            Property::UInt(v) => Some(v.into()),
            _ => None,
        }
    }

    /// Returns the value as unsigned integer, or `None` if the property does
    /// not have an unsigned integer type.
    pub fn as_unsigned_integer(&self) -> Option<u32> {
        match *self {
            Property::UChar(v) => Some(v.into()),
            Property::UShort(v) => Some(v.into()),
            Property::UInt(v) => Some(v),
            _ => None,
        }
    }

    /// Returns the value as signed integer, or `None` if the property does
    /// not have a signed integer type.
    pub fn as_signed_integer(&self) -> Option<i32> {
        match *self {
            Property::Char(v) => Some(v.into()),
            Property::Short(v) => Some(v.into()),
            Property::Int(v) => Some(v),
            _ => None,
        }
    }

    /// Returns the value as float, or `None` if the property does not have a
    /// float type.
    pub fn as_floating_point(&self) -> Option<f64> {
        match *self {
            Property::Float(v) => Some(v.into()),
            Property::Double(v) => Some(v),
            _ => None,
        }
    }
}


// ===========================================================================
// ===== RawSink
// ===========================================================================
/// A type that can accept raw data from a PLY file. This is mainly used for
/// [`Reader::read_raw_into`].
pub trait RawSink {
    /// Is called when a new element group begins. `def` describes the layout
    /// of all elements in this group. This method is *always* called before
    /// `element` is called.
    fn element_group_start(&mut self, def: &ElementDef) -> Result<(), Error>;

    /// Is called for each element that is read. When called, the element
    /// belongs to the last element group (the last `element_group_start`
    /// call).
    fn element(&mut self, elem: &RawElement) -> Result<(), Error>;
}

impl RawSink for RawResult {
    fn element_group_start(&mut self, def: &ElementDef) -> Result<(), Error> {
        self.element_groups.push(RawElementGroup {
            def: def.clone(),
            elements: vec![],
        });
        Ok(())
    }

    fn element(&mut self, elem: &RawElement) -> Result<(), Error> {
        self.element_groups
            .last_mut()
            .unwrap()
            .elements
            .push(elem.clone());
        Ok(())
    }
}

#[derive(Debug, Clone, Empty)]
pub struct RawResult {
    pub element_groups: Vec<RawElementGroup>,
}

#[derive(Debug, Clone)]
pub struct RawElementGroup {
    pub def: ElementDef,
    pub elements: Vec<RawElement>,
}
