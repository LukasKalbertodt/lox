//! The STL file format.
//!
//! STL is a pretty old and inflexible file format. It simply stores a list of
//! triangles where each triangle specifies its normal and the position of its
//! three vertices (plus a strange *attribute byte count*). The normal and
//! positions are always three 32 bit IEEE floats (`f32`). Due to just storing
//! a list of triangles, many vertices are stored multiple times, making the
//! format fairly space inefficient.
//!
//! Furthermore, for most uses of the mesh from an STL file, you need to unify
//! the vertices. The [`Reader`][stl::Reader] in this module can do this for you, but it's a
//! time consuming task, being around 5 times slower than reading a file
//! without unifying the vertices.
//!
//! Despite its many flaws, the file format is still used a lot, in particular
//! for 3D printing. The only real advantage is its simplicity.
//!
//! Links:
//! - ["Specifications"](http://www.fabbers.com/tech/STL_Format)
//! - [Wikipedia](https://en.wikipedia.org/wiki/STL_(file_format))
//!
//!
//! # Reading
//!
//! Reading an STL file is done via the [`Reader`][stl::Reader]. You can create
//! one via [`Reader::open`][stl::Reader::open] or
//! [`Reader::new`][stl::Reader::new]. Afterwards you need to use one of its
//! `read*` methods.
//!
//! ```no_run
//! use lox::{
//!     prelude::*,
//!     ds::SharedVertexMesh,
//!     io::stl::{Reader, ReadOptions},
//! };
//!
//! // TODO: remove `unwrap()`s once `?` in doctests is implemented
//! let results = Reader::open("mesh.stl").unwrap()
//!     .read::<SharedVertexMesh>(ReadOptions::default()).unwrap();
//! println!("{:?}", results);
//! ```
//!
//!
//! # Writing
//!
//! TODO
//!

use std::io;

use failure::Fail;



mod read;
mod write;

#[cfg(test)]
mod tests;

pub use self::read::{CountingSink, Reader, Sink, Triangle, RawResult, ReadResults, ReadOptions};
pub use self::write::{Serializer, Writer};


/// The two different formats of STL files.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Format {
    /// Everything is ASCII and very space inefficient.
    Ascii,

    /// Everything is stored as binary, in little endian encoding.
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
