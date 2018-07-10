use std::{
    io,
};

use fev_core::prop::PropLabel;

use crate::{
    ser::{PrimitiveType, TypedLabel},
};


mod write;

pub use self::write::StlWriter;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StlFormat {
    Ascii,
    Binary,
}

#[derive(Debug, Fail)]
pub enum StlError {
    #[fail(display = "type '{}' is not supported by PLY", _0)]
    PrimitiveTypeNotSupported(PrimitiveType),

    #[fail(display = "TODO")]
    PropertyNotSupported(PropLabel),

    #[fail(display = "IO error: {}", _0)]
    Io(io::Error),
}

impl From<io::Error> for StlError {
    fn from(src: io::Error) -> Self {
        StlError::Io(src)
    }
}
