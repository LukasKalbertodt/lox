//! The STL file format.
//!
//! STL is a pretty old and inflexible file format. It simply stores a list of
//! triangles where each triangle specifies its normal and the position of its
//! three vertices. The normal and positions are always three 32-bit IEEE
//! floats (`f32`). Due to just storing a list of triangles, many vertices are
//! stored multiple times, making the format fairly space inefficient.
//!
//! Furthermore, for most uses of the mesh from an STL file, you need to unify
//! the vertices. The [`Reader`][stl::Reader] in this module can do this for
//! you, but it's a time consuming task, being a couple of times slower than
//! reading a file without unifying the vertices.
//!
//! Despite its many flaws, the file format is still used a lot, in particular
//! for 3D printing. The only advantage is its simplicity. If possible, try not
//! to use this format.
//!
//! ### Links:
//! - ["Specifications"](http://www.fabbers.com/tech/STL_Format)
//! - [Wikipedia](https://en.wikipedia.org/wiki/STL_(file_format))
//!
//!
//! # Reading
//!
//! Most of the time, you don't even need to look in this module as you can
//! simply use [`io::read_file`][read_file] to read mesh files. If you need
//! more control over the reading process, take a look at [the STL
//! `Reader`][stl::Reader].
//!
//!
//! # Writing
//!
//! You can usually just use [`io::write_file`][write_file] to write meshes to
//! files. If you need more control over the writing process, take a look at
//! [the STL `Writer`][stl::Writer].
//!
//!
//! # Raw APIs
//!
//! If you need full low-level control, you can use
//! [`Writer::write_raw`][stl::Writer::write_raw] or
//! [`Reader::read_raw`][stl::Reader::read_raw]. This is usually not necessary.

use crate::{
    self as lox, // for proc macros
    Empty,
    io::util::IsFormat,
};


mod read;
mod write;

#[cfg(test)]
mod tests;

pub use self::read::Reader;
pub use self::write::{Config, Writer};


// ----------------------------------------------------------------------------

/// File name extentions used for this file format: `.stl`.
pub(super) const FILE_EXTENSIONS: &[&str] = &["stl"];

/// Check if the given data from the start of the file is a valid STL file
/// start.
pub(super) fn is_file_start(data: &[u8]) -> IsFormat {
    if data.len() >= 5 && &data[..5] == b"solid" {
        IsFormat::Probably
    } else if data.len() >= 84 {
        // Binary STL could start however, so we can't know for sure. But it
        // has to be at least 84 bytes long.
        IsFormat::Maybe
    } else {
        IsFormat::No
    }
}

/// The two different encodings of STL files.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Encoding {
    /// Everything is ASCII and very space inefficient.
    Ascii,

    /// Everything is stored as binary, in little endian encoding.
    Binary,
}


// ===========================================================================
// ===== Raw data structures (`RawTriangle` and `RawResult`)
// ===========================================================================

/// A raw triangle in an STL file.
///
/// This type is used in [`RawResult`]. If you don't use the low level `raw`
/// methods, you probably don't care about this type.
#[derive(Debug, Clone, Copy)]
pub struct RawTriangle {
    /// Face normal.
    pub normal: [f32; 3],

    /// The 3D positions of the vertices in CCW order (that is, when looking at
    /// the face "from the outside").
    pub vertices: [[f32; 3]; 3],

    /// No one understands what this does. It's only stored in binary format
    /// and is usually zero. Sometimes it's abused to store a 16bit color. You
    /// shouldn't do that.
    ///
    /// If an ASCII file is parsed, this is just set to 0 (despite it being not
    /// stored in the file).
    pub attribute_byte_count: u16,
}

/// Holds the raw data from an STL file.
///
/// To obtain a `RawResult`, call [`Reader::into_raw_result`]. See its
/// documentation for more information. If you don't use the low level `raw`
/// methods, you probably don't care about this type.
#[derive(Empty, Debug, Clone)]
pub struct RawResult {
    /// The solid name if it's specified in the file.
    pub solid_name: Option<String>,

    /// All triangles from the file.
    pub triangles: Vec<RawTriangle>,
}
