#![allow(unused_imports)] // TODO

use std::{
    fmt,
    fs::File,
    io::{self, BufWriter, Cursor, Write},
    path::Path,
};

use cgmath::Point3;
use failure::Fail;

use crate::{
    Mesh, TriVerticesOfFace,
    handle::{VertexHandle, FaceHandle, DefaultInt},
    map::VertexPropMap,
    math::{Pos3Like, PrimitiveNum},
    sealed::Sealed,
    util::MeshSizeHint,
};
use self::parse::ParseError;

pub mod parse;
pub mod ply;
pub mod stl;

// /// Types that can be transformed into a [`MeshWriter`].
// pub trait IntoMeshWriter<'a, MeshT, PosM>
// where
//     MeshT: 'a + Mesh + TriVerticesOfFace,
//     PosM: 'a + VertexPropMap,
//     PosM::Target: Pos3Like,
// {
//     type Writer: MeshWriter;
//     fn into_writer(self, mesh: &'a MeshT, vertex_positions: &'a PosM) -> Self::Writer;
// }

// /// Types that can serialize a mesh with vertex positions and potentially
// /// additional properties. The mesh and properties are already stored within
// /// the type.
// ///
// /// The main method of this trait is `write_to` which writes the mesh to a
// /// given `io::Write` destination. There are some other provided methods for
// /// easily writing to a file, to stdout and to memory.
// pub trait MeshWriter {
//     type Error: From<io::Error>;

//     /// Writes the mesh and all mesh properties into the given `Write`
//     /// instance.
//     fn write_to(&self, writer: impl Write) -> Result<(), Self::Error>;

//     /// Writes the mesh to the file given by the filename. Overwrites the file
//     /// if it already exists.
//     fn write_to_file(&self, path: impl AsRef<Path>) -> Result<(), Self::Error> {
//         self.write_to(BufWriter::new(File::create(path)?))
//     }

//     /// Writes the mesh to stdout. Locks stdout for the time the mesh is being
//     /// written.
//     fn write_to_stdout(&self) -> Result<(), Self::Error> {
//         let stdout = io::stdout();
//         let lock = stdout.lock();
//         self.write_to(lock)
//     }

//     /// Writes the mesh into a `Vec<u8>` which is returned on success.
//     fn write_to_memory(&self) -> Result<Vec<u8>, Self::Error> {
//         let mut w = Cursor::new(Vec::new());
//         self.write_to(&mut w)?;
//         Ok(w.into_inner())
//     }
// }

/// Represents one of the supported file formats.
///
/// New file formats may be added with only minor version bumps, so you cannot
/// match this enum exhaustively.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum FileFormat {
    Ply,
    Stl,
}

impl FileFormat {
    /// Tries to guess the file format from the file extension.
    ///
    /// Returns `None` if:
    /// - the path/file has no extension in its name, or
    /// - the extension is no valid UTF8, or
    /// - the file extension is not known.
    pub fn from_extension(path: impl AsRef<Path>) -> Option<Self> {
        path.as_ref()
            .extension()
            .and_then(|ext| ext.to_str())
            .and_then(|ext| {
                match ext {
                    "ply" => Some(FileFormat::Ply),
                    "stl" => Some(FileFormat::Stl),
                    _ => None,
                }
            })
    }
}

impl fmt::Display for FileFormat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FileFormat::Ply => "PLY",
            FileFormat::Stl => "STL",
        }.fmt(f)
    }
}

/// Describes the encoding of the main data of a mesh file.
///
/// Not every format has to support all of these encodings (in fact, many
/// formats only support one encoding).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileEncoding {
    /// Everything is stored as an ASCII string. Generally, ASCII encodings are
    /// fairly space-inefficient.
    Ascii,

    /// Binary encoding where all numeric types are stored in big endian
    /// layout.
    BinaryBigEndian,

    /// Binary encoding where all numeric types are stored in little endian
    /// layout.
    BinaryLittleEndian,
}

impl FileEncoding {
    pub fn binary_native() -> Self {
        #[cfg(target_endian = "big")]
        { FileEncoding::BinaryBigEndian }

        #[cfg(target_endian = "little")]
        { FileEncoding::BinaryLittleEndian }
    }
}

/// A simple unit-like error type that is used for `TryFrom<FileEncoding>`
/// impls of format specific `Encoding` types.
#[derive(Debug, Clone, Copy)]
pub struct EncodingNotSupported;

/// The error type for reading or writing a mesh.
#[derive(Debug, Fail)]
pub enum Error {
    /// An IO error.
    ///
    /// Can be caused by all kinds of failures. For example, if the underlying
    /// writer or reader returns an error or a file cannot be opened, this
    /// error variant is returned.
    #[fail(display = "IO error: {}", _0)]
    Io(io::Error),

    /// An error while parsing input data.
    ///
    /// Whenever a file (or generally, a stream) is parsed as a specific format
    /// and the file isn't valid, this error is returned. See [`parse::Error`]
    /// for more information.
    #[fail(display = "Parsing error: {}", _0)]
    Parse(ParseError),
}

impl From<io::Error> for Error {
    fn from(src: io::Error) -> Self {
        Error::Io(src)
    }
}

impl From<ParseError> for Error {
    fn from(src: ParseError) -> Self {
        Error::Parse(src)
    }
}



// ==========================================================================
// ===== Primitives
// ==========================================================================

/// Represents the type of an IO primitive.
///
/// This is closely related to [`PrimitiveValue`] (which represents an IO
/// primitive value) and [`Primitive`] (which is a trait abstracting over the
/// closed set of IO primitive types).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    Uint8,
    Int8,
    Uint16,
    Int16,
    Uint32,
    Int32,
    Float32,
    Float64,
}

/// Represents an IO primitive value.
///
/// This is closely related to [`PrimitiveType`] (which represents only the
/// type of IO primitives) and [`Primitive`] (which is a trait abstracting over
/// the closed set of IO primitive types).
#[derive(Debug, Clone, Copy)]
pub enum PrimitiveValue {
    Uint8(u8),
    Int8(i8),
    Uint16(u16),
    Int16(i16),
    Uint32(u32),
    Int32(i32),
    Float32(f32),
    Float64(f64),
}

macro_rules! make_convert_method {
    ($name:ident, $ty:ident, $variant:ident) => {
        make_convert_method!($name, $ty, $variant, stringify!($ty), stringify!($variant));
    };
    ($name:ident, $ty:ident, $variant:ident, $ty_str:expr, $variant_str:expr) => {
        /// Returns this value as `
        #[doc = $ty_str]
        /// ` if `self` is `
        #[doc = $variant_str]
        /// `. Otherwise, `None` is returned. This function does not cast
        /// between different number types!
        pub fn $name(&self) -> Option<$ty> {
            match self {
                PrimitiveValue::$variant(x) => Some(*x),
                _ => None,
            }
        }
    };
}

impl PrimitiveValue {
    make_convert_method!(as_u8, u8, Uint8);
    make_convert_method!(as_i8, i8, Int8);
    make_convert_method!(as_u16, u16, Uint16);
    make_convert_method!(as_i16, i16, Int16);
    make_convert_method!(as_u32, u32, Uint32);
    make_convert_method!(as_i32, i32, Int32);
    make_convert_method!(as_f32, f32, Float32);
    make_convert_method!(as_f64, f64, Float64);

    /// Returns the type of this value.
    pub fn ty(&self) -> PrimitiveType {
        match self {
            PrimitiveValue::Uint8(_) => PrimitiveType::Uint8,
            PrimitiveValue::Int8(_) => PrimitiveType::Int8,
            PrimitiveValue::Uint16(_) => PrimitiveType::Uint16,
            PrimitiveValue::Int16(_) => PrimitiveType::Int16,
            PrimitiveValue::Uint32(_) => PrimitiveType::Uint32,
            PrimitiveValue::Int32(_) => PrimitiveType::Int32,
            PrimitiveValue::Float32(_) => PrimitiveType::Float32,
            PrimitiveValue::Float64(_) => PrimitiveType::Float64,
        }
    }
}


/// Abstracts over all IO primitive types.
///
/// Note that this trait is exactly implemented for the types that are included
/// in [`PrimitiveType`] and [`PrimitiveValue`]. Thus, this is a closed set of
/// implementing types (unusual for a trait). As a consequence, you are not
/// supposed to implement this trait for your own types! That's why this trait
/// has a supertrait called `Sealed`. Said supertrait is crate-private, so you
/// can't implement it for your types.
pub trait Primitive: PrimitiveNum + Sealed {
    /// The type represented as this [`PrimitiveType`] value.
    const TY: PrimitiveType;

    /// Returns the primitive as a [`PrimitiveValue`] (basically dynamic
    /// typing).
    ///
    /// The implementation of this method always returns a value with the same
    /// type as specified in `Self::TY`. In other words: `T::TY ==
    /// t.to_primitive_value().ty()` is always true for all primitives `t` with
    /// type `T`.
    fn to_primitive_value(&self) -> PrimitiveValue;

    /// Returns `Some(self)` if `T == Self`, or `None` otherwise.
    ///
    /// This method is useful to convert a generic value into a value of a
    /// specific type.
    fn downcast_as<T: Primitive>(self) -> Option<T> {
        DowncastAs::downcast_as(self)
    }
}

macro_rules! impl_primitive {
    ($ty:ident, $variant:ident) => {
        impl Sealed for $ty {}
        impl Primitive for $ty {
            const TY: PrimitiveType = PrimitiveType::$variant;
            fn to_primitive_value(&self) -> PrimitiveValue {
                PrimitiveValue::$variant(*self)
            }
        }

        impl DowncastAs<$ty> for $ty {
            fn downcast_as(self) -> Option<$ty> {
                Some(self)
            }
        }
    }
}

impl_primitive!(u8,  Uint8);
impl_primitive!(i8,  Int8);
impl_primitive!(u16, Uint16);
impl_primitive!(i16, Int16);
impl_primitive!(u32, Uint32);
impl_primitive!(i32, Int32);
impl_primitive!(f32, Float32);
impl_primitive!(f64, Float64);

/// A helper trait to implement [`Primitive::downcast_as`].
trait DowncastAs<Target: Primitive> {
    fn downcast_as(self) -> Option<Target>;
}

// The default impl returns `None`. Specific impls will overwrite that.
impl<Source: Primitive, Target: Primitive> DowncastAs<Target> for Source {
    default fn downcast_as(self) -> Option<Target> {
        None
    }
}


// ==========================================================================
// ===== {Streaming/Mem}-Sinks and Sources
// ==========================================================================

pub trait StreamSource {
    fn transfer_to<S: MemSink>(self, sink: &mut S) -> Result<(), Error>;
}

/// A type that can receive and store mesh data in any order.
///
/// This trait is mostly used to transfer mesh data from a [`StreamSource`]
/// into another type. In this kind of transfer, the `StreamSource` determines
/// the order of the data, while the `MemSink` has to be able to store the data
/// in any order. There are a few exceptions – those are explained below.
///
/// In general, if the source provides data that the sink cannot store, that
/// data is ignored/discarded and does not lead to errors.
///
///
/// # Kinds of methods on this trait
///
/// There are three kinds of methods:
/// - *Mesh connectivity*: `add_vertex` and `add_face`. These are the only
///   required methods.
/// - *`size_hint`*: empty implementation provided.
/// - *Mesh properties*: `prepare_*` and `set_*` methods: empty implementations
///   provided.
///
/// There are some rules for the last kind of methods: for each property (e.g.
/// `vertex_position` or `face_normal`), the `prepare_*` method has to be
/// called by the source before the `set_*` method can be called. Additionally,
/// the `N` type parameter must be the same for all calls of `prepare_*` and
/// `set_*` of one property. The sink can rely on these rules.
///
/// The handles passed to `set_` methods have to be handles returned
/// by `add_vertex` or `add_face`.
///
/// The `count` parameter of the `prepare_` methods is just an optimization and
/// represents a lower bound of the number of properties will be added via
/// `set_*`. Therefore, it's always valid for the source to pass 0 as `count`.
pub trait MemSink {
    fn add_vertex(&mut self) -> VertexHandle;
    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle;

    /// Might be called by the source to indicate how many vertices and faces
    /// are to be expected.
    ///
    /// This is just an optimization as it allows the sink to reserve memory.
    /// The provided implementation simply does nothing, which is absolutely
    /// valid.
    ///
    /// This method might not be called by the source at all.
    fn size_hint(&mut self, _hint: MeshSizeHint) {}


    /// Informs the sink that the source will provide at least `count` vertex
    /// positions with the scalar type `N`.
    fn prepare_vertex_positions<N: Primitive>(&mut self, _count: DefaultInt) -> Result<(), Error> {
        Ok(())
    }

    /// Sets the position (with scalar type `N`) of the vertex `v`.
    fn set_vertex_position<N: Primitive>(
        &mut self,
        _v: VertexHandle,
        _position: Point3<N>,
    ) {}
}

pub trait StreamSink {
    fn transfer_from<S: MemSource>(self, src: &S) -> Result<(), Error>;
}

// TODO: probably use mesh traits as supertrait instead of repeating many of
// the relevant methods here.
pub trait MemSource {
    fn vertices(&self) -> Box<dyn Iterator<Item = VertexHandle> + '_>;
    fn faces(&self) -> Box<dyn Iterator<Item = FaceHandle> + '_>;

    fn num_vertices(&self) -> DefaultInt;
    fn num_faces(&self) -> DefaultInt;

    fn vertices_of_face(&self, f: FaceHandle) -> [VertexHandle; 3];

    fn vertex_position_type(&self) -> Option<PrimitiveType> {
        None
    }
    fn vertex_position<T: Primitive>(&self, _: VertexHandle) -> Point3<T> {
        panic!("requested non-existent vertex position from `MemSource`");
    }
}
