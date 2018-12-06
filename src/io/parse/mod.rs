use std::{
    fmt,
    io,
    ops,
};

use failure::Fail;



pub(crate) mod buf;

pub(crate) trait Input: io::Read + ops::Deref<Target = [u8]> {
    fn prepare(&mut self, num_bytes: usize) -> Result<(), Error>;
    fn saturating_prepare(&mut self, num_bytes: usize) -> Result<(), Error>;
    fn consume(&mut self, num_bytes: usize);
    fn is_eof(&mut self) -> Result<bool, Error>;
    fn offset(&self) -> usize;


    fn spanned_data(&self, num_bytes: usize) -> SpannedData<'_> {
        SpannedData {
            data: &self[..num_bytes],
            span: Span::new(self.offset(), self.offset() +  num_bytes),
        }
    }

    fn skip(&mut self, num_bytes: usize) -> Result<(), Error> {
        self.prepare(num_bytes)?;
        self.consume(num_bytes);

        Ok(())
    }

    fn skip_until(&mut self, stopper: impl Stopper) -> Result<(), Error> {
        loop {
            if self.is_eof()? {
                break;
            }

            if self.len() == 0 {
                self.prepare(1)?;
            }

            if stopper.should_stop(self[0]) {
                break;
            }

            self.consume(1);
        }

        Ok(())
    }

    fn with_bytes<F, O>(&mut self, num_bytes: usize, func: F) -> Result<O, Error>
    where
        F: FnOnce(SpannedData) -> Result<O, Error>,
    {
        self.prepare(num_bytes)?;
        let out = func(self.spanned_data(num_bytes))?;
        self.consume(num_bytes);

        Ok(out)
    }

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

            if stopper.should_stop(self[pos]) {
                break;
            }

            pos += 1;
        }

        let out = func(self.spanned_data(pos))?;
        self.consume(pos);

        Ok(out)
    }

    fn assert_eof(&mut self) -> Result<(), Error> {
        if !self.is_eof()? {
            Err(Error::UnexpectedAdditionalData)
        } else {
            Ok(())
        }
    }

    fn expect_tag(&mut self, tag: &[u8]) -> Result<(), Error> {
        self.with_bytes(tag.len(), |sd| {
            if sd.data != tag {
                let msg = format!(
                    "expected {}, found {}",
                    debug_fmt_bytes(tag),
                    debug_fmt_bytes(sd.data),
                );
                return Err(sd.error(msg));
            }

            Ok(())
        })
    }

    fn is_next(&mut self, expected: &[u8]) -> Result<bool, Error> {
        self.saturating_prepare(expected.len())?;
        Ok(self.starts_with(expected))
    }
}

#[derive(Debug)]
pub struct SpannedData<'a> {
    pub data: &'a [u8],
    pub span: Span,
}

impl<'a> SpannedData<'a> {
    pub fn error(&self, msg: impl Into<String>) -> Error {
        Error::Custom(msg.into(), self.span)
    }

    pub fn assert_ascii(&self) -> Result<&'a str, Error> {
        if !self.data.is_ascii() {
            Err(Error::NotAscii(self.span))
        } else {
            Ok(std::str::from_utf8(self.data).unwrap())
        }
    }
}

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

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "IO error: {}", _0)]
    Io(io::Error),

    #[fail(display = "unexpected EOF while parsing (at {})", _0)]
    UnexpectedEof(usize),

    #[fail(display = "expected EOF, but additional data was found")]
    UnexpectedAdditionalData,

    #[fail(display = "unexpected non-ASCII data at {}", _0)]
    NotAscii(Span),

    #[fail(
        display = "parsing lookahead got too big (due to a really degenerated \
            file or a parser bug)"
    )]
    LookAheadTooBig,

    #[fail(display = "{} (at {})", _0, _1)]
    Custom(String, Span)
}

impl From<io::Error> for Error {
    fn from(src: io::Error) -> Self {
        Error::Io(src)
    }
}


macro_rules! gen_endian_parser {
    ($name:ident, $ty:ident,  $method:ident, $endian:ident) => {
        #[allow(dead_code)] // TODO
        pub(crate) fn $name(input: &mut impl Input) -> Result<$ty, Error> {
            use byteorder::ReadBytesExt;

            input.$method::<byteorder::$endian>().map_err(|e| e.into())
        }
    }
}

#[allow(dead_code)] // TODO
pub(crate) fn u8_we(input: &mut impl Input) -> Result<u8, Error> {
    use byteorder::ReadBytesExt;
    input.read_u8().map_err(|e| e.into())
}
#[allow(dead_code)] // TODO
pub(crate) fn i8_we(input: &mut impl Input) -> Result<i8, Error> {
    use byteorder::ReadBytesExt;
    input.read_i8().map_err(|e| e.into())
}

gen_endian_parser!(u16_le, u16, read_u16, LittleEndian);
gen_endian_parser!(u32_le, u32, read_u32, LittleEndian);
gen_endian_parser!(u64_le, u64, read_u64, LittleEndian);

gen_endian_parser!(i16_le, i16, read_i16, LittleEndian);
gen_endian_parser!(i32_le, i32, read_i32, LittleEndian);
gen_endian_parser!(i64_le, i64, read_i64, LittleEndian);

gen_endian_parser!(f32_le, f32, read_f32, LittleEndian);
gen_endian_parser!(f64_le, f64, read_f64, LittleEndian);

gen_endian_parser!(u16_be, u16, read_u16, BigEndian);
gen_endian_parser!(u32_be, u32, read_u32, BigEndian);
gen_endian_parser!(u64_be, u64, read_u64, BigEndian);

gen_endian_parser!(i16_be, i16, read_i16, BigEndian);
gen_endian_parser!(i32_be, i32, read_i32, BigEndian);
gen_endian_parser!(i64_be, i64, read_i64, BigEndian);

gen_endian_parser!(f32_be, f32, read_f32, BigEndian);
gen_endian_parser!(f64_be, f64, read_f64, BigEndian);


pub fn debug_fmt_bytes(data: &[u8]) -> String {
    if let Ok(s) = std::str::from_utf8(data) {
        format!("{:?}", s)
    } else {
        format!("{:?}", data)
    }
}

pub trait Stopper {
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
