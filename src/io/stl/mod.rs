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
//! simply use [`io::read`][read] to read mesh files. If however, you want to
//! only read `stl` files, you can use [`stl::read()`] instead. If you need
//! more control over the reading process, take a look at [the STL
//! `Reader`][stl::Reader].
//!
//!
//! # Writing
//!
//! You can usually just use [`io::write`][write] to write meshes to files. If
//! however, you want to only write `stl` files and don't want to specify the
//! file format via the file extension, you can use [`stl::write()`] instead.
//! If you need more control over the writing process, take a look at [the STL
//! `Writer`][stl::Writer].
//!
//!
//! # Raw APIs
//!
//! TODO

use std::{
    convert::TryFrom,
    fs::File,
    io,
    path::Path,
};

use crate::{
    self as lox, // for proc macros
    Empty,
    io::{FileEncoding, EncodingNotSupported, StreamSink, MemSink, MemSource, Error, IsFormat},
};


mod read;
mod write;

#[cfg(test)]
mod tests;

pub use self::read::Reader;
pub use self::write::{Config, Writer};


// ----------------------------------------------------------------------------

/// File name extentions used for this file format: `.stl`.
pub const FILE_EXTENSIONS: &[&str] = &["stl"];

/// Check if the given data from the start of the file is a valid STL file
/// start.
pub fn is_file_start(data: &[u8]) -> IsFormat {
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

/// Like [`io::read`][lox::io::read], but always reads the file as STL file
/// (vertices are being unified).
///
/// If you need more control about how and what to read, take a look at
/// [`Reader`].
///
///
/// # Example
///
/// ```no_run
/// use lox::{
///     ds::FaceDelegateMesh,
///     fat::MiniMesh,
///     io::stl,
/// };
///
/// let mesh: MiniMesh<FaceDelegateMesh> = stl::read("foo.stl")?;
///
/// // This also tries to read the file as STL (and will return an error, since
/// // the file is probably not a valid STL file).
/// let mesh: MiniMesh<FaceDelegateMesh> = stl::read("foo.other-format")?;
/// # Ok::<_, lox::io::Error>(())
/// ```
pub fn read<T: Empty + MemSink, P: AsRef<Path>>(path: P) -> Result<T, Error> {
    T::create_from(Reader::open(path)?)
}

/// Like [`io::write`][lox::io::write], but always writes the file as binary
/// STL file.
///
/// If you need more control about how and what to write, take a look at
/// [`Writer`].
///
///
/// # Example
///
/// ```no_run
/// use lox::{
///     prelude::*,
///     ds::FaceDelegateMesh,
///     fat::MiniMesh,
///     io::stl,
/// };
///
/// let mesh = MiniMesh::<FaceDelegateMesh>::empty(); // dummy mesh
/// stl::write("foo.stl", &mesh)?;
///
/// // This also writes the mesh as STL file, regardless of the file name. No
/// // idea why you would do such a thing though.
/// stl::write("foo.other-format", &mesh)?;
/// # Ok::<_, lox::io::Error>(())
/// ```
pub fn write<T: MemSource, P: AsRef<Path>>(path: P, src: &T) -> Result<(), Error> {
    let file = io::BufWriter::new(File::create(path)?);

    Config::binary()
        .into_writer(file)
        .transfer_from(src)
}


/// The two different encodings of STL files.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Encoding {
    /// Everything is ASCII and very space inefficient.
    Ascii,

    /// Everything is stored as binary, in little endian encoding.
    Binary,
}

impl TryFrom<FileEncoding> for Encoding {
    type Error = EncodingNotSupported;
    fn try_from(src: FileEncoding) -> Result<Self, Self::Error> {
        match src {
            FileEncoding::Ascii => Ok(Encoding::Ascii),
            FileEncoding::BinaryLittleEndian => Ok(Encoding::Binary),
            FileEncoding::BinaryBigEndian => Err(EncodingNotSupported),
        }
    }
}

impl From<Encoding> for FileEncoding {
    fn from(src: Encoding) -> Self {
        match src {
            Encoding::Ascii => FileEncoding::Ascii,
            Encoding::Binary => FileEncoding::BinaryLittleEndian,
        }
    }
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
