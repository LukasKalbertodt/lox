//! Reading from and writing to PLY files (Polygon File Format).
//!
//! TODO: explain everything.

// TODO PLY things:
// - Figure out colors (write and read)
// - How to name the `Serializer`? Maybe `WriterBuilder`?

#![allow(unused_imports)] // TODO
use std::{
    io,
    fs::File,
    path::Path,
};

use crate::{
    Empty,
    io::{
        Error, FileEncoding, StreamSink, MemSink, MemSource,
        util::IsFormat,
    },
};

pub mod raw;
mod read;
mod write;

#[cfg(test)]
mod tests;


pub use self::read::Reader;
pub use self::write::{Config, Writer};


// ----------------------------------------------------------------------------

/// File name extentions used for this file format: `.ply`.
pub const FILE_EXTENSIONS: &[&str] = &["ply"];

/// Check if the given data from the start of the file is a valid PLY file
/// start.
pub fn is_file_start(data: &[u8]) -> IsFormat {
    if data.len() >= 4 && &data[..4] == b"ply\n" {
        IsFormat::Probably
    } else {
        IsFormat::No
    }
}

/// Reads the PLY file with the given filename into an empty instance of `T`
/// and returns that instance.
///
/// If you need more control about how and what to read, take a look at
/// [`Reader`].
///
/// TODO: Example
pub fn read<T: Empty + MemSink, P: AsRef<Path>>(path: P) -> Result<T, Error> {
    T::create_from(Reader::open(path)?)
}

pub fn write<T: MemSource, P: AsRef<Path>>(path: P, src: &T) -> Result<(), Error> {
    let file = io::BufWriter::new(File::create(path)?);

    Config::binary()
        .into_writer(file)
        .transfer_from(src)
}


/// The encoding of a PLY file.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Encoding {
    /// Everything is stored as an ASCII string. You should usually not use
    /// this as this encoding is very space-inefficient.
    Ascii,

    /// Binary encoding where all numeric types are stored in big endian
    /// layout. The header is still ASCII.
    BinaryBigEndian,

    /// Binary encoding where all numeric types are stored in little endian
    /// layout. The header is still ASCII.
    BinaryLittleEndian,
}

impl Encoding {
    /// Returns the binary encoding with native endianess (little endian on
    /// x86).
    pub fn native_binary() -> Self {
        if cfg!(target_endian = "big") {
            Encoding::BinaryBigEndian
        } else {
            Encoding::BinaryLittleEndian
        }
    }
}

impl From<FileEncoding> for Encoding {
    fn from(src: FileEncoding) -> Self {
        match src {
            FileEncoding::Ascii => Encoding::Ascii,
            FileEncoding::BinaryBigEndian => Encoding::BinaryBigEndian,
            FileEncoding::BinaryLittleEndian => Encoding::BinaryLittleEndian,
        }
    }
}

impl From<Encoding> for FileEncoding {
    fn from(src: Encoding) -> Self {
        match src {
            Encoding::Ascii => FileEncoding::Ascii,
            Encoding::BinaryBigEndian => FileEncoding::BinaryBigEndian,
            Encoding::BinaryLittleEndian => FileEncoding::BinaryLittleEndian,
        }
    }
}
