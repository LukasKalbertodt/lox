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
use cgmath::Point3;
use derive_more::{Add, AddAssign, Deref, DerefMut, From, Sub, SubAssign};
use failure::Fail;
use smallvec::SmallVec;

use crate::{
    TransferError,
    prelude::*,
    io::{
        StreamingSource, MemSink, Primitive,
        parse::{
            self, Input, Span, debug_fmt_bytes, SpannedData,
            buf::{Buffer},
        },
    },
};
use super::{Error, Encoding};



// ===========================================================================
// ===== Parsing functions
// ===========================================================================
// TODO: move this somewhere better (and remove duplicate code in STL)
macro_rules! parser {
    ($name:ident = |$buf:ident| $body:expr) => {
        parser!($name = |$buf| -> () { $body });
    };
    ($name:ident = |$buf:ident| -> $out:ty $body:block) => {
        fn $name($buf: &mut impl Input) -> Result<$out, parse::Error> {
            $body
        }
    };
}

// Optionally skip whitespace
parser!(opt_whitespace = |buf| buf.skip_until(|b| b != b' '));

// Requires at least one whitespace, skips all whitespace that follows
// it.
parser!(whitespace = |buf| {
    buf.expect_tag(b" ")?;
    opt_whitespace(buf)?;
    Ok(())
});

/// Requires a '\n' linebreak with optional whitespace before it. The
/// "specification" talks about "carriage-return terminated lines", but this is
/// incorrect, as in: all files I could find end their lines with '\n' (0x0A)
/// and not '\r' (0x0D). This includes the example file linked in the
/// specification!
parser!(linebreak = |buf| {
    opt_whitespace(buf)?;
    buf.expect_tag(b"\n")?;
    Ok(())
});

/// Calls the passed parser and requires a linebreak at the end.
fn line<I, F, O>(buf: &mut I, func: F) -> Result<O, parse::Error>
where
    I: Input,
    F: FnOnce(&mut I) -> Result<O, parse::Error>,
{
    let out = func(buf)?;
    linebreak(buf)?;
    Ok(out)
}


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
        fn add_comment(
            buf: &mut impl Input,
            comments: &mut Vec<String>,
        ) -> Result<(), parse::Error> {
            line(buf, |buf| {
                buf.take_until(b'\n', |line| {
                    comments.push(line.assert_ascii()?[7..].trim_start().to_string());
                    Ok(())
                })
            })
        }

        /// Parses a scalar type delimited by whitespace (whitespace is not
        /// read by this function).
        fn parse_scalar_type(buf: &mut impl Input) -> Result<ScalarType, parse::Error> {
            buf.take_until(b' ', |word| {
                word.assert_ascii()?
                    .parse::<ScalarType>()
                    .map_err(|e| word.error(e.to_string()))
            })
        }

        /// Parses a single word delimited by whitespace or newline.
        fn parse_ident(buf: &mut impl Input) -> Result<String, parse::Error> {
            buf.take_until(|b| b == b' ' || b == b'\n', |s| {
                s.assert_ascii().map(|s| s.to_string())
            })
        }

        // Wrap reader into parse buffer.
        let mut buf = Buffer::new(reader)?;

        let mut comments = Vec::new();


        // ===== Parse magic number and format line ===========================
        // PLY files always start with `ply\n`. This serves as magic number.
        buf.expect_tag(b"ply\n").map_err(|e| {
            match e {
                parse::Error::Io(e) => parse::Error::Io(e),
                _ => parse::Error::Custom(
                    "not a valid PLY file (does not start with \"ply\\n\")".into(),
                    Span::new(0, 4),
                ),
            }
        })?;

        // Read any comment lines that might be here
        while buf.is_next(b"comment")? {
            add_comment(&mut buf, &mut comments)?;
        }

        // Parse format line. This is required to be before everything else in
        // the header (except the magic number and potential comments).
        let encoding = line(&mut buf, |buf| {
            buf.expect_tag(b"format")?;
            whitespace(buf)?;

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
                        Err(line.error(msg))
                    }
                }
            })?;

            whitespace(buf)?;
            buf.expect_tag(b"1.0")?;

            Ok(encoding)
        })?;


        // ===== Parse elements and their properties =========================
        let mut elements = Vec::new();

        // Line by line until we reach the end of the header
        while !buf.is_next(b"end_header")? {
            match () {
                () if buf.is_next(b"comment ")? => add_comment(&mut buf, &mut comments)?,

                // Element definition, e.g. `element vertex 8`
                () if buf.is_next(b"element ")? => {
                    buf.consume(b"element".len());
                    whitespace(&mut buf)?;

                    let name = parse_ident(&mut buf)?;
                    whitespace(&mut buf)?;

                    let count = buf.take_until(|b| b == b' ' || b == b'\n', |n| {
                        match n.assert_ascii()?.parse::<u64>() {
                            Ok(v) => Ok(v),
                            Err(e) => {
                                let msg = format!("invalid integer as element count ({})", e);
                                Err(n.error(msg))
                            }
                        }
                    })?;

                    elements.push(ElementDef {
                        name,
                        count,
                        property_defs: Vec::new().into(),
                    });

                    line(&mut buf, |buf| opt_whitespace(buf))?;
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
                    whitespace(&mut buf)?;

                    if buf.is_next(b"list")? {
                        buf.consume(b"list".len());
                        whitespace(&mut buf)?;

                        let len_type = parse_scalar_type(&mut buf)?;
                        whitespace(&mut buf)?;
                        let scalar_type = parse_scalar_type(&mut buf)?;
                        whitespace(&mut buf)?;
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

                            parse::Error::Custom(msg, span)
                        })?;

                        let ty = PropertyType::List { len_type, scalar_type };
                        elem.property_defs.push(PropertyDef { name, ty });
                    } else {
                        let ty = PropertyType::Scalar(parse_scalar_type(&mut buf)?);
                        whitespace(&mut buf)?;
                        let name = parse_ident(&mut buf)?;

                        elem.property_defs.push(PropertyDef { name, ty });
                    }

                    line(&mut buf, |buf| opt_whitespace(buf))?;
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
        opt_whitespace(&mut buf)?;
        linebreak(&mut buf)?;


        Ok(Self { buf, comments, encoding, elements })
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

        // // Keep this vector on the outside to retain allocations
        // let mut properties = Vec::new();


        // let read_element = match self.encoding {
        //     Encoding::BinaryBigEndian => BbeEncoding::read_element::<Buffer<R>>,
        //     Encoding::BinaryLittleEndian => BleEncoding::read_element::<Buffer<R>>,
        //     Encoding::Ascii => AsciiEncoding::read_element::<Buffer<R>>,
        // };

        // Iterate through each element group
        for element_def in &self.elements {
            sink.element_group_start(&element_def);

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
                sink.element(&elem);
            }
        }

        Ok(())
    }
}

impl<R: io::Read> StreamingSource for Reader<R> {
    type Error = Error;
    fn transfer_to<S: MemSink>(mut self, sink: &mut S) -> Result<(), Self::Error> {
        fn read_pos<T: FromBytes + Primitive, S: MemSink>(
            sink: &mut S,
            raw: &[u8],
            handle: VertexHandle,
            offsets: &[RawOffset; 3],
        ) {
            let x = T::from_bytes_ne(&raw[offsets[0].as_usize()..]);
            let y = T::from_bytes_ne(&raw[offsets[1].as_usize()..]);
            let z = T::from_bytes_ne(&raw[offsets[2].as_usize()..]);
            sink.set_vertex_position(handle, Point3::new(x, y, z));
        }


        let buf = &mut self.buf;

        let mut vertex_handles = Vec::new();

        let read_element = match self.encoding {
            Encoding::BinaryBigEndian => read_element_bbe::<Buffer<R>>,
            Encoding::BinaryLittleEndian => read_element_ble::<Buffer<R>>,
            Encoding::Ascii => read_element_ascii::<Buffer<R>>,
        };



        // Iterate through each element group
        // let mut raw = Vec::new();
        // let mut starts = Vec::new();
        for element_def in &self.elements {
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

            let index = get_type_lens(element_def);

            macro_rules! read_elem {
                () => {{
                    elem.data.clear();
                    read_element(buf, &index, &mut elem)
                        .expect("fucki wucki");
                }}
            }

            match &*element_def.name {
                "vertex" => {
                    // Preparations for vertex parsing
                    let pos_type = element_def.property_defs.iter()
                        .find(|def| def.name == "x")
                        .expect("no position data in PLY") // TODO
                        .ty;

                    let x_index = element_def.property_defs.iter()
                        .position(|def| def.name == "x")
                        .expect("no position data in PLY"); // TODO
                    let y_index = element_def.property_defs.iter()
                        .position(|def| def.name == "y")
                        .expect("no position data in PLY"); // TODO
                    let z_index = element_def.property_defs.iter()
                        .position(|def| def.name == "z")
                        .expect("no position data in PLY"); // TODO

                    // TODO: make data structure to nicely get indices
                    let x_index = PropIndex::from(x_index as u8);
                    let y_index = PropIndex::from(y_index as u8);
                    let z_index = PropIndex::from(z_index as u8);


                    let pos_type = match pos_type {
                        PropertyType::Scalar(ty) => ty,
                        PropertyType::List { .. } => panic!("bad position type") // TODO
                    };
                    let read_pos = match pos_type {
                        ScalarType::Char => read_pos::<i8, S>,
                        ScalarType::UChar => read_pos::<u8, S>,
                        ScalarType::Short => read_pos::<i16, S>,
                        ScalarType::UShort => read_pos::<u16, S>,
                        ScalarType::Int => read_pos::<i32, S>,
                        ScalarType::UInt => read_pos::<u32, S>,
                        ScalarType::Float => read_pos::<f32, S>,
                        ScalarType::Double => read_pos::<f64, S>,
                    };

                    for _ in 0..element_def.count {
                        read_elem!();
                        // println!("{:02x?}", raw);
                        // println!("{:?}", starts);

                        let handle = sink.add_vertex();
                        vertex_handles.push(handle);
                        let pos_offsets = [
                            elem.prop_infos[x_index].offset,
                            elem.prop_infos[y_index].offset,
                            elem.prop_infos[z_index].offset,
                        ];
                        read_pos(sink, &elem.data, handle, &pos_offsets);
                    }
                }
                "face" => {
                    // println!("----");
                    // TODO: check "vertex" came before

                    let vi_index = element_def.property_defs.iter()
                        .position(|def| def.name == "vertex_indices")
                        .expect("no vertex indices data in PLY"); // TODO
                    // TODO: build a nice structure to find index
                    let vi_index = PropIndex::from(vi_index as u8);

                    fn read_vertex_handles<T: FromBytes + Into<u32>>(
                        raw: &[u8],
                    ) -> [u32; 3] {
                        let a = T::from_bytes_ne(&raw[0 * T::SIZE..]).into();
                        let b = T::from_bytes_ne(&raw[1 * T::SIZE..]).into();
                        let c = T::from_bytes_ne(&raw[2 * T::SIZE..]).into();
                        [a, b, c]
                    }


                    let vi_scalar_type = element_def.property_defs[vi_index].ty.scalar_type();
                    let read_vertex_handles = match vi_scalar_type {
                        ScalarType::Char | ScalarType::UChar => read_vertex_handles::<u8>,
                        ScalarType::Short | ScalarType::UShort => read_vertex_handles::<u16>,
                        ScalarType::Int | ScalarType::UInt => read_vertex_handles::<u32>,
                        _ => unreachable!(), // TODO: actually check above
                    };

                    for _ in 0..element_def.count {
                        read_elem!();
                        // println!("{:02x?}", raw);
                        // println!("{:?}", starts);
                        let vi_list = elem.list_at(vi_index)
                            .expect("fucki wucki `vertex_indices` has to be list");

                        if vi_list.list_len != 3 {
                            panic!("fucki wucki no triangle");
                        }

                        let vis = read_vertex_handles(&elem.data[vi_list.data_offset..]);
                        // println!("{:?}", vis);

                        let handles = [
                            // TODO: error handling
                            *vertex_handles.get(vis[0] as usize)
                                .expect("fucki wucki incorrect index"),
                            *vertex_handles.get(vis[1] as usize)
                                .expect("fucki wucki incorrect index"),
                            *vertex_handles.get(vis[2] as usize)
                                .expect("fucki wucki incorrect index"),
                        ];

                        sink.add_face(handles);


                        // let handle = sink.add_vertex();
                        // vertex_handles.push(handle);
                        // read_pos(
                        //     sink,
                        //     &raw,
                        //     handle,
                        //     &[starts[x_index], starts[y_index], starts[z_index]],
                        // );
                    }
                }
                _ => unimplemented!()
            }
        }
        Ok(())
    }
}


// ===========================================================================
// ===== `RawElement` definitions
// ===========================================================================

#[derive(Clone)]
pub struct RawElement {
    /// The packed data of all properties in native endianess.
    ///
    /// For PLY files stored in native endianess, this is an exact chunk from
    /// the file. For ASCII files and files in non-native endianess, the
    /// properties are first converted to this format.
    pub data: RawData,

    /// Some information about each property in this element.
    pub prop_infos: PropVec<RawPropertyInfo>,
}

#[derive(Debug, Clone)]
pub struct RawPropertyInfo {
    /// The byte offset in the packed, native-endian data at which this
    /// property starts.
    offset: RawOffset,

    /// The type of this property.
    ty: PropertyType,

    /// Name of this property.
    name: String,
}

impl RawElement {
    fn list_at(&self, idx: PropIndex) -> Option<RawListInfo> {
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
            PropertyType::Scalar(ty) => read_scalar(&self.data[..], ty),
            PropertyType::List { scalar_type, .. } => {
                let list_info = self.list_at(idx).unwrap();

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

    fn iter(&self) -> RawElementIter<'_> {
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

struct RawElementIter<'a> {
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


struct RawListInfo {
    pub list_len: u32,
    pub len_offset: RawOffset,
    pub data_offset: RawOffset,
    pub len_type: ListLenType,
    pub scalar_type: ScalarType,
}


// ===========================================================================
// ===== Strongly typed wrapper for certain things
// ===========================================================================

/// A byte offset into the raw data of one element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Add, Sub, AddAssign, SubAssign, From)]
pub struct RawOffset(u32);

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
/// element group.
#[derive(Debug, Clone, Copy, PartialEq, Eq, From, Add, Sub, AddAssign, SubAssign)]
struct PropIndex(u8);

impl PropIndex {
    fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

/// Raw data of one element. Can be indexed by `RawOffset`.
#[derive(Debug, Clone, From)]
pub struct RawData(Vec<u8>);

// ----- Index impls for `RawData`

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

/// A vector that holds data for each property. Can be indexed by `PropIndex`.
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

fn read_element_bbe<I: Input>(
    buf: &mut I,
    index: &[TypeLen],
    raw_elem: &mut RawElement,
) -> Result<(), parse::Error> {
    #[cfg(target_endian = "big")]
    {
        read_element_binary_native(buf, index, raw_elem)
    }

    #[cfg(target_endian = "little")]
    {
        read_element_binary_swapped(buf, index, raw_elem)
    }
}

fn read_element_ble<I: Input>(
    buf: &mut I,
    index: &[TypeLen],
    raw_elem: &mut RawElement,
) -> Result<(), parse::Error> {
    #[cfg(target_endian = "big")]
    {
        read_element_binary_swapped(buf, index, raw_elem)
    }

    #[cfg(target_endian = "little")]
    {
        read_element_binary_native(buf, index, raw_elem)
    }
}

#[inline(always)]
fn read_bytes_into(
    buf: &mut impl Input,
    count: usize,
    out: &mut Vec<u8>,
) -> Result<(), parse::Error> {
    buf.with_bytes(count, |b| {
        out.extend_from_slice(&b.data);
        Ok(())
    })
}

fn read_element_binary_native(
    buf: &mut impl Input,
    index: &[TypeLen],
    raw_elem: &mut RawElement,
) -> Result<(), parse::Error> {
    let mut offset = RawOffset::from(0);

    for (prop_info, prop_len) in raw_elem.prop_infos.iter_mut().zip(index) {
        let len: RawOffset = match *prop_len {
            TypeLen::Scalar(len) => len.into(),
            TypeLen::List { scalar_len, .. } => {
                // Read the list lenght. The type of this property is a list
                // (the index says so), so we can unwrap here.
                let list_len = read_binary_len(
                    buf,
                    prop_info.ty.len_type().unwrap(),
                    offset.as_usize(),
                    &mut raw_elem.data,
                    |_| {}, // no byte swapping
                )?;

                RawOffset::from(list_len * (scalar_len.as_u8() as u32))
            }
        };

        read_bytes_into(buf, len.as_usize(), &mut raw_elem.data)?;
        prop_info.offset = offset;
        offset += len;
    }

    Ok(())
}

fn read_binary_len(
    buf: &mut impl Input,
    ty: ListLenType,
    offset: usize,
    out: &mut Vec<u8>,
    swap: impl FnOnce(&mut [u8]),
) -> Result<u32, parse::Error> {
    match ty {
        ListLenType::UChar => buf.with_bytes(1, |b| {
            out.extend_from_slice(&b.data);
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

fn read_element_binary_swapped(
    buf: &mut impl Input,
    index: &[TypeLen],
    raw_elem: &mut RawElement,
) -> Result<(), parse::Error> {
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
                    start.as_usize(),
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
                        sd.error(msg)
                    })
            }
        )
    }
}

fn read_element_ascii<I: Input>(
    buf: &mut I,
    _: &[TypeLen],
    raw_elem: &mut RawElement,
) -> Result<(), parse::Error> {
    let mut offset = RawOffset::from(0);
    let mut first = true;

    for prop_info in raw_elem.prop_infos.iter_mut() {
        if first {
            opt_whitespace(buf)?;
            first = false;
        } else {
            whitespace(buf)?;
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
                    whitespace(buf)?;
                    read_ascii_value(buf, scalar_type, &mut raw_elem.data)?;
                }

                RawOffset::from(list_len * scalar_type.len() as u32) + len_type.len()
            }
        };

        prop_info.offset = offset;
        offset += RawOffset::from(len);
    }

    linebreak(buf)?;

    Ok(())
}

/// Read a single ASCII value of type `ty` into `out`.
fn read_ascii_value(
    buf: &mut impl Input,
    ty: ScalarType,
    out: &mut Vec<u8>,
) -> Result<(), parse::Error> {
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
    name: String,

    /// Number of elements in this group.
    count: u64,

    /// Definitions for all properties of elements in this group.
    property_defs: PropVec<PropertyDef>,
}

/// Te header definition of one property of an element.
#[derive(Debug, Clone)]
pub struct PropertyDef {
    ty: PropertyType,
    name: String,
}

#[derive(Debug, Clone, Copy)]
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

/// One property value of some PLY type.
///
/// The sizes of the smallvecs are choosen so that the inline variant won't
/// inflict a size overhead (on x64). This still means that the most common
/// form of list, the three-tuple `vertex_indices`, will fit inline.
#[derive(Debug, Clone)]
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
    fn element_group_start(&mut self, def: &ElementDef);

    /// Is called for each element that is read. When called, the element
    /// belongs to the last element group (the last `element_group_start`
    /// call).
    fn element(&mut self, elem: &RawElement);
}

// impl RawSink for RawResult {
//     fn element_group_start(&mut self, def: &ElementDef) {
//         self.element_groups.push(ElementGroup {
//             def: def.clone(),
//             elements: vec![],
//         });
//     }
//     fn element(&mut self, properties: &[Property]) {
//         self.element_groups
//             .last_mut()
//             .unwrap()
//             .elements
//             .push(Element { properties: properties.to_vec() });
//     }
// }

// #[derive(Debug)]
// pub struct RawResult {
//     element_groups: Vec<ElementGroup>,
// }

// impl RawResult {
//     /// Creates an instance with no name and no triangles.
//     pub fn new() -> Self {
//         Self {
//             element_groups: Vec::new(),
//         }
//     }
// }

// #[derive(Debug)]
// pub struct ElementGroup {
//     def: ElementDef,
//     elements: Vec<Element>,
// }

// #[derive(Debug)]
// pub struct Element {
//     // TODO: this is really not very space efficient...
//     properties: Vec<Property>,
// }
