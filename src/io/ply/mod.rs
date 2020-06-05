//! The PLY file format.
//!
//! PLY is a popular and flexible file format often used for meshes and point
//! clouds. It is able to store all kinds of mesh properties, including normals
//! and colors. In fact, arbitrary properties of different types can be
//! attached to any element. There can even be arbitrary elements (usually
//! `vertex` and `face`). Because everything is so flexible, there are some
//! convention on how to call important properties. For example, vertex
//! positions are stored in three properties called `x`, `y` and `z`.
//!
//! PLY files can be encoded as ASCII or as binary with either big or small
//! endianess. While the ASCII encoding is space-inefficient (as usual), the
//! binary formats are very close to be memory-optimal. The only very minor
//! waste is that the number of vertices per face is always stored -- which is
//! not necessary in the triangle-only case. But this only wastes one byte per
//! face.
//!
//! The only disadvantage of this flexibility is the parsing and writing
//! complexity. Since all elements and properties can be arbitrarily defined
//! (e.g. `x`, `y` and `z` properties don't have to be in that order) and due
//! to the dynamic typing (there are 8 different scalar types), it's not easy
//! to parse the PLY format in a fast way. However, I think this implementation
//! is fairly well optimized and should perform fine or even faster than in
//! other mesh libraries.
//!
//! ### Links:
//! - ["Specification"](http://paulbourke.net/dataformats/ply/)
//! - [Wikipedia](https://en.wikipedia.org/wiki/PLY_(file_format))
//!
//!
//! # Reading and writing
//!
//! Most of the time you don't even need to look into this module, but can
//! instead use the functions in the `io` module, like
//! [`read_file`][super::read_file] or [`write_file`][super::write_file]. If you
//! need a bit more control, take a look at [`Writer`] and [`Reader`].
//!
//!
//! # Raw APIs
//!
//! If you need full low-level control, you can use [`Writer::write_raw`] or
//! [`Reader::read_raw`]. However, this is usually not necessary.

use crate::{
    io::{
        FileEncoding,
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
pub(super) const FILE_EXTENSIONS: &[&str] = &["ply"];

/// Check if the given data from the start of the file is a valid PLY file
/// start.
pub(super) fn is_file_start(data: &[u8]) -> IsFormat {
    if data.len() >= 4 && &data[..4] == b"ply\n" {
        IsFormat::Probably
    } else {
        IsFormat::No
    }
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
    pub fn binary_native() -> Self {
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
