use std::{
    fmt,
    io,
    ops,
};

use failure::Fail;

use crate::{
    io::Error,
    util::debug_fmt_bytes,
};

mod buf;

pub(crate) use buf::Buffer;


/// A parse buffer: a buffered reader that offers methods for parsing binary or
/// ASCII data.
///
/// This trait is only implemented for [`Buffer`], so you might ask: why does
/// this trait exist instead of using `Buffer` directly? The problem is that
/// `Buffer` has the generic type parameter `R` for the reader. All parsers
/// would have to be generic over the reader anyway, so we don't win anything.
/// The trait exists because writing `&mut impl ParseBuf` is easier than `&mut
/// Buffer<impl io::Read>`. Finally, the complete interface of the parse buffer
/// can be seen in this trait (without any internal methods of `Buffer`).
pub(crate) trait ParseBuf: io::Read {
    /// Makes sure that at least `num_bytes` bytes are in the internal buffer.
    ///
    /// If the internal buffer already contains sufficiently many bytes, this
    /// method does nothing.
    ///
    /// If additional data must be loaded from the underlying reader and an EOF
    /// is reached, this method returns an error. See `saturating_prepare` for
    /// an alternative.
    fn prepare(&mut self, num_bytes: usize) -> Result<(), Error>;

    /// Makes sure that at least `num_bytes` bytes are in the internal buffer
    /// or EOF is reached.
    ///
    /// If the internal buffer already contains sufficiently many bytes, this
    /// method does nothing.
    ///
    /// This is different from `prepare` as this doesn't error when
    /// encountering an EOF. Instead, this method simply loads fewer bytes into
    /// the internal buffer in that case.
    fn saturating_prepare(&mut self, num_bytes: usize) -> Result<(), Error>;

    /// Mark the next `num_bytes` bytes as consumed (i.e. remove them from the
    /// internal buffer).
    ///
    /// Panics if `num_bytes` is greater than the internal buffer len.
    fn consume(&mut self, num_bytes: usize);

    /// Returns `true` **iff** the underlying reader is exhausted and the
    /// internal buffer is empty.
    fn is_eof(&mut self) -> Result<bool, Error>;

    /// Returns the number of bytes that were already consumed. This is the
    /// offset in the stream of the underlying reader.
    fn offset(&self) -> usize;

    /// Returns a slice to the internal buffer.
    fn raw_buf(&self) -> &[u8];


    fn len(&self) -> usize {
        self.raw_buf().len()
    }

    /// Returns `SpannedData` that represents the first `num_bytes` of the
    /// internal buffer.
    ///
    /// Panics if `num_bytes` is greater than the internal buffer len.
    fn spanned_data(&self, num_bytes: usize) -> SpannedData<'_> {
        SpannedData {
            data: &self.raw_buf()[..num_bytes],
            span: Span::new(self.offset(), self.offset() + num_bytes),
        }
    }

    /// Skips `num_bytes` bytes.
    fn skip(&mut self, num_bytes: usize) -> Result<(), Error> {
        // TODO: we can do better than this.
        self.prepare(num_bytes)?;
        self.consume(num_bytes);

        Ok(())
    }

    /// Skips all bytes until `stopper` says that the skipping should stop or
    /// EOF is reached.
    fn skip_until(&mut self, stopper: impl Stopper) -> Result<(), Error> {
        loop {
            if self.is_eof()? {
                break;
            }

            if self.len() == 0 {
                self.prepare(1)?;
            }

            if stopper.should_stop(self.raw_buf()[0]) {
                break;
            }

            self.consume(1);
        }

        Ok(())
    }

    /// Prepares `num_bytes` and passes them as `SpannedData` to the given
    /// `func`. If `func` returns successfully, those bytes are consumed.
    fn with_bytes<F, O>(&mut self, num_bytes: usize, func: F) -> Result<O, Error>
    where
        F: FnOnce(SpannedData) -> Result<O, Error>,
    {
        self.prepare(num_bytes)?;
        let out = func(self.spanned_data(num_bytes))?;
        self.consume(num_bytes);

        Ok(out)
    }

    /// Prepares bytes until `stopper` returns `true` and passes them as
    /// `SpannedData` to the given `func`. If `func` returns successfully,
    /// those bytes are consumed.
    fn take_until<F, O>(
        &mut self,
        stopper: impl Stopper,
        func: F,
    ) -> Result<O, Error>
    where
        F: FnOnce(SpannedData) -> Result<O, Error>
    {
        let mut pos = 0;
        loop {
            if self.len() <= pos {
                self.prepare(pos + 1)?;
            }

            if stopper.should_stop(self.raw_buf()[pos]) {
                break;
            }

            pos += 1;
        }

        let out = func(self.spanned_data(pos))?;
        self.consume(pos);

        Ok(out)
    }

    /// Makes sure that EOF is reached and returns an error if that's not the
    /// case.
    fn assert_eof(&mut self) -> Result<(), Error> {
        if !self.is_eof()? {
            Err(ParseError::UnexpectedAdditionalData.into())
        } else {
            Ok(())
        }
    }

    /// Makes sure `tag` comes next in the data. If that's the case, those
    /// bytes are consumed. If not, an error is returned.
    fn expect_tag(&mut self, tag: &[u8]) -> Result<(), Error> {
        self.with_bytes(tag.len(), |sd| {
            if sd.data != tag {
                let msg = format!(
                    "expected {}, found {}",
                    debug_fmt_bytes(tag),
                    debug_fmt_bytes(sd.data),
                );
                return Err(sd.error(msg).into());
            }

            Ok(())
        })
    }

    /// Checks if `expected` comes next in the data. Does not consume anything.
    fn is_next(&mut self, expected: &[u8]) -> Result<bool, Error> {
        self.saturating_prepare(expected.len())?;
        Ok(self.raw_buf().starts_with(expected))
    }
}

/// A slice of input data with its span in the input stream.
#[derive(Debug)]
pub struct SpannedData<'a> {
    pub data: &'a [u8],
    pub span: Span,
}

impl<'a> SpannedData<'a> {
    /// Returns a custom `ParseError` with the given message and the span of
    /// this data.
    pub fn error(&self, msg: impl Into<String>) -> ParseError {
        ParseError::Custom(msg.into(), self.span)
    }

    /// Makes sure this data is all ASCII. If that's the case, the data is
    /// returned as `&str` string. If not, an error is returned.
    pub fn assert_ascii(&self) -> Result<&'a str, ParseError> {
        if !self.data.is_ascii() {
            Err(ParseError::NotAscii(self.span))
        } else {
            Ok(std::str::from_utf8(self.data).unwrap())
        }
    }
}

/// Represents a span in the input stream (with start and end).
#[derive(Clone, Copy, Debug)]
pub struct Span {
    lo: usize,
    hi: usize,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Self { lo, hi }
    }

    pub fn single(at: usize) -> Self {
        Self {
            lo: at,
            hi: at + 1,
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}..{}", self.lo, self.hi)
    }
}

/// Things that can go wrong when parsing.
#[derive(Debug, Fail)]
pub enum ParseError {
    /// Data was expected, but EOF was encountered at the given offset.
    #[fail(display = "unexpected EOF while parsing (at {})", _0)]
    UnexpectedEof(usize),

    /// EOF was expected, but additional data was found.
    #[fail(display = "expected EOF, but additional data was found")]
    UnexpectedAdditionalData,

    /// ASCII data was expected, but non-ASCII bytes were found.
    #[fail(display = "unexpected non-ASCII data at {}", _0)]
    NotAscii(Span),

    /// The internal parse buffer got too big (larger than
    /// `buf::MAX_BUFFER_SIZE`, which currently is 4MB).
    ///
    /// This shouldn't happen, as parsers only consume rather small chunks at a
    /// time. So this either means a very degenerate file was loaded or the
    /// parser is buggy. In either way, we don't want to die the slow OOM
    /// death, but rather error right away.
    #[fail(
        display = "parsing lookahead got too big (due to a really degenerated \
            file or a parser bug)"
    )]
    LookAheadTooBig,

    /// A custom error message with an attached span.
    #[fail(display = "{} (at {})", _0, _1)]
    Custom(String, Span)
}

/// Something that decides when to stop traversing a byte stream. Used for
/// `ParseBuf::skip_until` and `ParseBuf::take_until`.
pub(crate) trait Stopper {
    fn should_stop(&self, byte: u8) -> bool;
}

impl Stopper for u8 {
    fn should_stop(&self, byte: u8) -> bool {
        byte == *self
    }
}

impl<F: Fn(u8) -> bool> Stopper for F {
    fn should_stop(&self, byte: u8) -> bool {
        self(byte)
    }
}

// ===========================================================================
// ===== Several useful parser functions
// ===========================================================================

/// Skips whitespace (only ' ', no other whitespace characters) until
/// encountering a non-space character.
pub(crate) fn opt_whitespace(buf: &mut impl ParseBuf) -> Result<(), Error> {
    buf.skip_until(|b| b != b' ')
}

/// Expects at least one space character (' ') and additionally skips any space
/// characters immediately following.
pub(crate) fn whitespace(buf: &mut impl ParseBuf) -> Result<(), Error> {
    buf.expect_tag(b" ")?;
    opt_whitespace(buf)?;
    Ok(())
}

/// Expects a '\n' linebreak with optional whitespace before and after it.
pub(crate) fn linebreak(buf: &mut impl ParseBuf) -> Result<(), Error> {
    opt_whitespace(buf)?;
    buf.expect_tag(b"\n")?;
    opt_whitespace(buf)?;
    Ok(())
}

/// Skips optional whitespace, calls the passed parser and requires a
/// linebreak '\n' at the end.
pub(crate) fn line<P, F, O>(buf: &mut P, func: F) -> Result<O, Error>
where
    P: ParseBuf,
    F: FnOnce(&mut P) -> Result<O, Error>,
{
    opt_whitespace(buf)?;
    let out = func(buf)?;
    linebreak(buf)?;
    Ok(out)
}

/// Parses a whitespace (' ' or '\n') delimited ASCII `f32` value. We simply
/// use the `FromStr` impl of `f32`.
pub(crate) fn ascii_f32(buf: &mut impl ParseBuf) -> Result<f32, Error> {
    buf.take_until(
        |b| b == b' ' || b == b'\n',
        |sd| {
            sd.assert_ascii()?
                .parse::<f32>()
                .map_err(|e| sd.error(format!("invalid float literal: {}", e)).into())
        }
    )
}
