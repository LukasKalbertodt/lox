use std::{
    io,
    ops,
};

use failure::Fail;



pub(crate) mod buf;

pub(crate) trait Input: io::Read + ops::Deref<Target = [u8]> {
    fn prepare(&mut self, num_bytes: usize) -> Result<(), Error>;
    fn consume(&mut self, num_bytes: usize);
    fn is_eof(&mut self) -> bool;

    fn skip(&mut self, num_bytes: usize) -> Result<(), Error> {
        self.prepare(num_bytes)?;
        self.consume(num_bytes);

        Ok(())
    }

    fn with_bytes<F, O>(&mut self, num_bytes: usize, func: F) -> Result<O, Error>
    where
        F: FnOnce(&[u8]) -> Result<O, Error>,
    {
        self.prepare(num_bytes)?;
        let out = func(&self[..num_bytes])?;
        self.consume(num_bytes);

        Ok(out)
    }

    fn assert_eof(&mut self) -> Result<(), Error> {
        if !self.is_eof() {
            Err(Error::UnexpectedAdditionalData)
        } else {
            Ok(())
        }
    }
}



#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "IO error: {}", _0)]
    Io(io::Error),

    #[fail(display = "unexpected EOF while parsing")]
    UnexpectedEof,

    #[fail(display = "expected EOF, but additional data was found")]
    UnexpectedAdditionalData,
}

impl From<io::Error> for Error {
    fn from(src: io::Error) -> Self {
        if src.kind() == io::ErrorKind::UnexpectedEof {
            Error::UnexpectedEof
        } else {
            Error::Io(src)
        }
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

gen_endian_parser!(u16_le, u16, read_u16, LittleEndian);
gen_endian_parser!(u32_le, u32, read_u32, LittleEndian);
gen_endian_parser!(u64_le, u64, read_u64, LittleEndian);

gen_endian_parser!(f32_le, f32, read_f32, LittleEndian);
gen_endian_parser!(f64_le, f64, read_f64, LittleEndian);
