//! The STL file format.
//!
//! STL is a pretty old and inflexible file format. It simply stores a list of
//! triangles where each triangle specifies its normal and the position of its
//! three vertices (plus a strange *attribute byte count* in binary encoding).
//! The normal and positions are always three 32 bit IEEE floats (`f32`). Due
//! to just storing a list of triangles, many vertices are stored multiple
//! times, making the format fairly space inefficient.
//!
//! Furthermore, for most uses of the mesh from an STL file, you need to unify
//! the vertices. The [`Reader`][stl::Reader] in this module can do this for you, but it's a
//! time consuming task, being around 5 times slower than reading a file
//! without unifying the vertices.
//!
//! Despite its many flaws, the file format is still used a lot, in particular
//! for 3D printing. The only real advantage is its simplicity.
//!
//! ### Links:
//! - ["Specifications"](http://www.fabbers.com/tech/STL_Format)
//! - [Wikipedia](https://en.wikipedia.org/wiki/STL_(file_format))
//!
//!
//! # Reading
//!
//! Reading an STL file is done via the [`Reader`][stl::Reader]. You can create
//! one via [`Reader::open`][stl::Reader::open] or
//! [`Reader::new`][stl::Reader::new].
//!
//! TODO
//!
//!
//! # Writing
//!
//! To write an STL file, you need a [`Writer`][stl::Writer]. To obtain one,
//! create a [`Config`][stl::Config] first and call `into_writer` on it.
//! Then you can use the writer via [`MeshWriter`][crate::io::MeshWriter].
//!
//!
//! ```no_run
//! #![feature(proc_macro_hygiene)]
//! use cgmath::{Point3, Vector3};
//! use lox::{
//!     mesh,
//!     prelude::*,
//!     ds::SharedVertexMesh,
//!     io::stl::Config,
//! };
//!
//!
//! let (mesh, positions, face_normals) = mesh! {
//!     type: SharedVertexMesh,
//!     vertices: [
//!         v0: (Point3::new(0.0f32, 0.0, 0.0)),
//!         v1: (Point3::new(0.0, 1.0, 0.0)),
//!         v2: (Point3::new(1.0, 0.0, 0.0)),
//!     ],
//!     faces: [
//!         [v0, v1, v2]: (Vector3::new(0.0f32, 0.0, -1.0)),
//!     ],
//! };
//!
//! // TODO remove unwrap once ? in doctest is stable
//! Config::binary()
//!     .into_writer(&mesh, &positions)
//!     .with_face_normals(&face_normals) // <-- this is optional
//!     .write_to_file("mesh.stl").unwrap();
//! ```

use std::{
    convert::TryFrom,
    io,
};

use failure::Fail;

use crate::{
    io::{FileEncoding, EncodingNotSupported, parse},
};


mod read;
mod write;

#[cfg(test)]
mod tests;

pub use self::read::{FnRawSink, Reader, RawSink, RawTriangle, RawResult};
pub use self::write::{Config, Writer, Sink};


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

/// Errors that can potentially happen when reading or writing an STL file.
#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "Parsing error: {}", _0)]
    Parse(parse::Error),

    #[fail(display = "IO error: {}", _0)]
    Io(io::Error),
}

impl From<io::Error> for Error {
    fn from(src: io::Error) -> Self {
        Error::Io(src)
    }
}

impl From<parse::Error> for Error {
    fn from(src: parse::Error) -> Self {
        Error::Parse(src)
    }
}
