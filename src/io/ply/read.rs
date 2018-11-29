#![allow(unused_imports)] // TODO

use std::{
    cmp::min,
    fmt,
    fs::File,
    marker::PhantomData,
    io,
    path::Path,
    str::FromStr,
};

use byteorder::{LittleEndian, ReadBytesExt};
use failure::Fail;
use smallvec::SmallVec;

use crate::{
    TransferError,
    prelude::*,
    io::{
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
                        property_defs: vec![],
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
                        if !len_type.is_unsigned_integer()  {
                            return Err(parse::Error::Custom(
                                format!("only unsigned integers can be used to store list \
                                    lengths (property '{}')", name),
                                Span::new(line_start, buf.offset()),
                            ).into());
                        }

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

    pub fn read_raw_into(mut self, _sink: &mut impl RawSink) -> Result<(), Error> {
        let buf = &mut self.buf;

        let mut properties = Vec::new();
        for element_def in &self.elements {
            for _ in 0..element_def.count {
                properties.clear();

                match self.encoding {
                    Encoding::Ascii => unimplemented!(),
                    Encoding::BinaryBigEndian => {
                        parse_element::<BbeEncoding, _>(buf, &element_def, &mut properties)?;
                    }
                    Encoding::BinaryLittleEndian => {
                        parse_element::<BleEncoding, _>(buf, &element_def, &mut properties)?;
                    }
                }

                println!("{:?}", properties);
            }
        }

        Ok(())
    }
}

// ===========================================================================
// ===== Helpers for body parsing
// ===========================================================================
trait EncodingReader {
    fn read_i8(buf: &mut impl Input) -> Result<i8, parse::Error>;
    fn read_u8(buf: &mut impl Input) -> Result<u8, parse::Error>;
    fn read_i16(buf: &mut impl Input) -> Result<i16, parse::Error>;
    fn read_u16(buf: &mut impl Input) -> Result<u16, parse::Error>;
    fn read_i32(buf: &mut impl Input) -> Result<i32, parse::Error>;
    fn read_u32(buf: &mut impl Input) -> Result<u32, parse::Error>;
    fn read_f32(buf: &mut impl Input) -> Result<f32, parse::Error>;
    fn read_f64(buf: &mut impl Input) -> Result<f64, parse::Error>;
}

enum BbeEncoding {}
impl EncodingReader for BbeEncoding {
    fn read_i8(buf: &mut impl Input) -> Result<i8, parse::Error> { parse::i8_we(buf) }
    fn read_u8(buf: &mut impl Input) -> Result<u8, parse::Error> { parse::u8_we(buf) }
    fn read_i16(buf: &mut impl Input) -> Result<i16, parse::Error> { parse::i16_be(buf) }
    fn read_u16(buf: &mut impl Input) -> Result<u16, parse::Error> { parse::u16_be(buf) }
    fn read_i32(buf: &mut impl Input) -> Result<i32, parse::Error> { parse::i32_be(buf) }
    fn read_u32(buf: &mut impl Input) -> Result<u32, parse::Error> { parse::u32_be(buf) }
    fn read_f32(buf: &mut impl Input) -> Result<f32, parse::Error> { parse::f32_be(buf) }
    fn read_f64(buf: &mut impl Input) -> Result<f64, parse::Error> { parse::f64_be(buf) }
}

enum BleEncoding {}
impl EncodingReader for BleEncoding {
    fn read_i8(buf: &mut impl Input) -> Result<i8, parse::Error> { parse::i8_we(buf) }
    fn read_u8(buf: &mut impl Input) -> Result<u8, parse::Error> { parse::u8_we(buf) }
    fn read_i16(buf: &mut impl Input) -> Result<i16, parse::Error> { parse::i16_le(buf) }
    fn read_u16(buf: &mut impl Input) -> Result<u16, parse::Error> { parse::u16_le(buf) }
    fn read_i32(buf: &mut impl Input) -> Result<i32, parse::Error> { parse::i32_le(buf) }
    fn read_u32(buf: &mut impl Input) -> Result<u32, parse::Error> { parse::u32_le(buf) }
    fn read_f32(buf: &mut impl Input) -> Result<f32, parse::Error> { parse::f32_le(buf) }
    fn read_f64(buf: &mut impl Input) -> Result<f64, parse::Error> { parse::f64_le(buf) }
}

fn parse_element<E: EncodingReader, I: Input>(
    buf: &mut I,
    def: &ElementDef,
    out: &mut Vec<Property>,
) -> Result<(), Error> {
    /// Reads a single value of type `ty` and returns it as `Property`. The
    /// returned `Property` is one of the scalar variants, i.e. not one of the
    /// `*List` variants!
    fn read_scalar<E: EncodingReader, I: Input>(
        buf: &mut I,
        ty: ScalarType,
    ) -> Result<Property, Error> {
        let p = match ty {
            ScalarType::Char => Property::Char(E::read_i8(buf)?),
            ScalarType::UChar => Property::UChar(E::read_u8(buf)?),
            ScalarType::Short => Property::Short(E::read_i16(buf)?),
            ScalarType::UShort => Property::UShort(E::read_u16(buf)?),
            ScalarType::Int => Property::Int(E::read_i32(buf)?),
            ScalarType::UInt => Property::UInt(E::read_u32(buf)?),
            ScalarType::Float => Property::Float(E::read_f32(buf)?),
            ScalarType::Double => Property::Double(E::read_f64(buf)?),
        };

        Ok(p)
    }

    for prop_def in &def.property_defs {
        let property = match prop_def.ty {
            PropertyType::Scalar(ty) => read_scalar::<E, _>(buf, ty)?,
            PropertyType::List { len_type, scalar_type } => {
                // We know that the `len_type` is an unsigned integer type,
                // because it was checked while parsing the header.
                let len = read_scalar::<E, _>(buf, len_type)?
                    .as_unsigned_integer()
                    .unwrap();

                macro_rules! read_list {
                    ($variant:ident, $read_fun:ident) => {{
                        let mut list = SmallVec::new();
                        for _ in 0..len {
                            list.push(E::$read_fun(buf)?);
                        }

                        Property::$variant(list)
                    }}
                }

                match scalar_type {
                    ScalarType::Char => read_list!(CharList, read_i8),
                    ScalarType::UChar => read_list!(UCharList, read_u8),
                    ScalarType::Short => read_list!(ShortList, read_i16),
                    ScalarType::UShort => read_list!(UShortList, read_u16),
                    ScalarType::Int => read_list!(IntList, read_i32),
                    ScalarType::UInt => read_list!(UIntList, read_u32),
                    ScalarType::Float => read_list!(FloatList, read_f32),
                    ScalarType::Double => read_list!(DoubleList, read_f64),
                }
            }
        };
        out.push(property);
    }

    Ok(())
}



#[derive(Debug)]
pub struct ElementDef {
    name: String,
    count: u64,
    property_defs: Vec<PropertyDef>,
}

#[derive(Debug)]
pub struct PropertyDef {
    ty: PropertyType,
    name: String,
}

#[derive(Debug)]
pub enum PropertyType {
    Scalar(ScalarType),
    List {
        len_type: ScalarType,
        scalar_type: ScalarType,
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
    pub fn is_floating_point(&self) -> bool {
        *self == ScalarType::Float || *self == ScalarType::Double
    }

    pub fn is_unsigned_integer(&self) -> bool {
        match self {
            ScalarType::UChar | ScalarType::UShort | ScalarType::UInt => true,
            _ => false,
        }
    }

    pub fn is_signed_integer(&self) -> bool {
        match self {
            ScalarType::Char | ScalarType::Short | ScalarType::Int => true,
            _ => false,
        }
    }

    pub fn size(&self) -> usize {
        match self {
            ScalarType::Char => 1,
            ScalarType::UChar => 1,
            ScalarType::Short => 2,
            ScalarType::UShort => 2,
            ScalarType::Int => 4,
            ScalarType::UInt => 4,
            ScalarType::Float => 4,
            ScalarType::Double => 8,
        }
    }
}

pub struct ScalarTypeParseError(String);

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

pub trait RawSink {
}

impl RawSink for () {}

#[derive(Debug)]
pub struct RawResult {
    elements: Vec<ElementGroup>
}

#[derive(Debug)]
pub struct ElementGroup {
    def: ElementDef,
    elements: Vec<Element>,
}

#[derive(Debug)]
pub struct Element {
    // TODO: this is really not very space efficient...
    properties: Vec<Property>,
}

#[derive(Debug)]
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

    pub fn as_unsigned_integer(&self) -> Option<u32> {
        match *self {
            Property::UChar(v) => Some(v.into()),
            Property::UShort(v) => Some(v.into()),
            Property::UInt(v) => Some(v),
            _ => None,
        }
    }

    pub fn as_signed_integer(&self) -> Option<i32> {
        match *self {
            Property::Char(v) => Some(v.into()),
            Property::Short(v) => Some(v.into()),
            Property::Int(v) => Some(v),
            _ => None,
        }
    }

    pub fn as_floating_point(&self) -> Option<f64> {
        match *self {
            Property::Float(v) => Some(v.into()),
            Property::Double(v) => Some(v),
            _ => None,
        }
    }
}
