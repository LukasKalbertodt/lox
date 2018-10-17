use std::io;

use failure::Fail;



mod read;
mod write;

pub use self::read::{AdhocSink, Reader, Sink, Triangle, RawResult};
pub use self::write::{Serializer, Writer};


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Format {
    Ascii,
    Binary,
}

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "IO error: {}", _0)]
    Io(io::Error),
}

impl From<io::Error> for Error {
    fn from(src: io::Error) -> Self {
        Error::Io(src)
    }
}
