use std::{
    // collections::HashSet,
    // fmt,
    io,
};

use failure::Fail;

use fev_core::{MeshElement};
use crate::{
    ser::{PrimitiveType, TypedLabel},
};


mod write;

pub use self::write::PlyWriter;

// use byteorder::{WriteBytesExt, BigEndian, LittleEndian};

// use crate::{
//     TriMesh, Pos3Like, Vec3Like,
//     handle::{DefaultIndex, FaceHandle, VertexHandle},
//     map::{FaceMap, VertexMap},
//     io::{
//         IntoMeshWriter, PropSetSerialize, MeshWriter, PropSerialize, PropSerializer,
//         PrimitiveType, PropLabel, PropSetSerializer, PropType, LabeledPropSet,
//         NameLabel, PropLabeler, StandardLabel,
//     },
// };



#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Format {
    Ascii,
    BinaryBigEndian,
    BinaryLittleEndian,
}


#[derive(Clone, Copy, Debug)]
pub struct Ply {
    format: Format,
}

impl Ply {
    pub fn ascii() -> Self {
        Self::new(Format::Ascii)
    }

    pub fn binary() -> Self {
        Self::new(Format::BinaryBigEndian)
    }

    pub fn new(format: Format) -> Self {
        Self { format }
    }
}

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "type '{}' is not supported by PLY", _0)]
    PrimitiveTypeNotSupported(PrimitiveType),

    #[fail(display =
        "attempt to add {} property `{:?}` to PLY file which uses the field name '{}', but a \
         property with that name has already been added ({:?})",
        element,
        new_label,
        name,
        old_label,
    )]
    NameAlreadyInUse {
        name: String,
        element: MeshElement,
        old_label: TypedLabel,
        new_label: TypedLabel,
    },

    #[fail(display = "IO error: {}", _0)]
    Io(io::Error),
}

impl From<io::Error> for Error {
    fn from(src: io::Error) -> Self {
        Error::Io(src)
    }
}
