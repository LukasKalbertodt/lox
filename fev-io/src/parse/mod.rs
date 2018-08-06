use std::{
    io,
    ops,
};


crate mod buf;

pub trait Input: io::Read + ops::Deref<Target = [u8]> {
    fn prepare(&mut self, num_bytes: usize) -> Result<(), Error>;
    fn consume(&mut self, num_bytes: usize);

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
}



#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    UnexpectedEof,
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
    ($name:ident, $out:ident, $len:expr, $method:ident, $endian:ident) => {
        crate fn $name(input: &mut impl Input) -> Result<$out, Error> {
            use byteorder::ReadBytesExt;

            input.$method::<byteorder::$endian>().map_err(|e| e.into())
        }
    }
}

gen_endian_parser!(u32_le, u32, 4, read_u32, LittleEndian);

gen_endian_parser!(f32_le, f32, 4, read_f32, LittleEndian);
// gen_endian_parser!(f64_le, f64, 8, read_f64, LittleEndian);
