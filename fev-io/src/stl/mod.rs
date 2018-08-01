use std::io;

use failure::Fail;

use fev_core::prop::PropLabel;
use crate::ser::PrimitiveType;


mod write;

pub use self::write::StlWriter;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Format {
    Ascii,
    Binary,
}

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "type '{}' is not supported by PLY", _0)]
    PrimitiveTypeNotSupported(PrimitiveType),

    #[fail(display = "TODO")]
    PropertyNotSupported(PropLabel),

    #[fail(display = "IO error: {}", _0)]
    Io(io::Error),
}

impl From<io::Error> for Error {
    fn from(src: io::Error) -> Self {
        Error::Io(src)
    }
}
