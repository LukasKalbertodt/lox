#![allow(unused_imports)] // TODO

use std::{
    cmp::min,
    fmt,
    fs::File,
    marker::PhantomData,
    io,
    path::Path,
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
use super::{Error, Format};



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
    format: Format,
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
        let format = line(&mut buf, |buf| {
            buf.expect_tag(b"format")?;
            whitespace(buf)?;

            const MAX_FORMAT_LEN: usize = 20;

            let format = buf.take_until(MAX_FORMAT_LEN, b' ', |line| {
                match line.data {
                    b"ascii" => Ok(Format::Ascii),
                    b"binary_little_endian" => Ok(Format::BinaryLittleEndian),
                    b"binary_big_endian" => Ok(Format::BinaryBigEndian),
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

            Ok(format)
        })?;


        // ===== Parse elements and their properties =========================
        let mut elements = Vec::new();

        // Line by line until we reach the end of the header
        while !buf.is_next(b"end_header")? {
            match () {
                () if buf.is_next(b"comment ")? => add_comment(&mut buf, &mut comments)?,
                () if buf.is_next(b"element ")? => {
                    buf.skip_until(b'\n')?;
                    buf.consume(1);
                }
                () if buf.is_next(b"property ")? => {
                    buf.skip_until(b'\n')?;
                    buf.consume(1);
                }
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

}
