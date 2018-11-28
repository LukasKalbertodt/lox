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
    elements: Vec<Element>,
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
                buf.take_until(None, b'\n', |line| {
                    comments.push(line.assert_ascii()?[7..].trim_start().to_string());
                    Ok(())
                })
            })
        }

        /// Parses a scalar type delimited by whitespace (whitespace is not
        /// read by this function).
        fn parse_scalar_type(buf: &mut impl Input) -> Result<ScalarType, parse::Error> {
            buf.take_until(None, b' ', |word| {
                word.assert_ascii()?
                    .parse::<ScalarType>()
                    .map_err(|e| word.error(e.to_string()))
            })
        }

        /// Parses a single word delimited by whitespace or newline.
        fn parse_ident(buf: &mut impl Input) -> Result<String, parse::Error> {
            buf.take_until(None, |b| b == b' ' || b == b'\n', |s| {
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

            const MAX_FORMAT_LEN: usize = 20;

            let encoding = buf.take_until(MAX_FORMAT_LEN, b' ', |line| {
                match line.data {
                    b"ascii" => Ok(Encoding::Ascii),
                    b"binary_little_endian" => Ok(Encoding::BinaryLittleEndian),
                    b"binary_big_endian" => Ok(Encoding::BinaryBigEndian),
                    other => {
                        let len = min(other.len(), MAX_FORMAT_LEN);
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

                    let count = buf.take_until(None, |b| b == b' ' || b == b'\n', |n| {
                        match n.assert_ascii()?.parse::<u64>() {
                            Ok(v) => Ok(v),
                            Err(e) => {
                                let msg = format!("invalid integer as element count ({})", e);
                                Err(n.error(msg))
                            }
                        }
                    })?;

                    elements.push(Element {
                        name,
                        count,
                        properties: vec![],
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

                        // We don't allow floating point types to specify the
                        // length as they don't make a lot of sense.
                        if len_type.is_floating_point() {
                            return Err(parse::Error::Custom(
                                format!("floating point types cannot be used to store list \
                                    lengths (property '{}')", name),
                                Span::new(line_start, buf.offset()),
                            ).into());
                        }

                        let ty = PropertyType::List { len_type, scalar_type };
                        elem.properties.push(Property { name, ty });
                    } else {
                        let ty = PropertyType::Scalar(parse_scalar_type(&mut buf)?);
                        whitespace(&mut buf)?;
                        let name = parse_ident(&mut buf)?;

                        elem.properties.push(Property { name, ty });
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


        Ok(Self { buf, comments, format, elements })
    }
}

#[derive(Debug)]
struct Element {
    name: String,
    count: u64,
    properties: Vec<Property>,
}

#[derive(Debug)]
struct Property {
    ty: PropertyType,
    name: String,
}

#[derive(Debug)]
enum PropertyType {
    Scalar(ScalarType),
    List {
        len_type: ScalarType,
        scalar_type: ScalarType,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ScalarType {
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
}

struct ScalarTypeParseError(String);

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
