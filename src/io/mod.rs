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
};


pub mod parse;
pub mod ply;
pub mod stl;

/// Types that can be transformed into a [`MeshWriter`].
pub trait IntoMeshWriter<'a, MeshT, PosM>
where
    MeshT: 'a + Mesh + TriVerticesOfFace,
    PosM: 'a + VertexPropMap,
    PosM::Target: Pos3Like,
{
    type Writer: MeshWriter;
    fn into_writer(self, mesh: &'a MeshT, vertex_positions: &'a PosM) -> Self::Writer;
}

/// Types that can serialize a mesh with vertex positions and potentially
/// additional properties. The mesh and properties are already stored within
/// the type.
///
/// The main method of this trait is `write_to` which writes the mesh to a
/// given `io::Write` destination. There are some other provided methods for
/// easily writing to a file, to stdout and to memory.
pub trait MeshWriter {
    type Error: From<io::Error>;

    /// Writes the mesh and all mesh properties into the given `Write`
    /// instance.
    fn write_to(&self, writer: impl Write) -> Result<(), Self::Error>;

    /// Writes the mesh to the file given by the filename. Overwrites the file
    /// if it already exists.
    fn write_to_file(&self, path: impl AsRef<Path>) -> Result<(), Self::Error> {
        self.write_to(BufWriter::new(File::create(path)?))
    }

    /// Writes the mesh to stdout. Locks stdout for the time the mesh is being
    /// written.
    fn write_to_stdout(&self) -> Result<(), Self::Error> {
        let stdout = io::stdout();
        let lock = stdout.lock();
        self.write_to(lock)
    }

    /// Writes the mesh into a `Vec<u8>` which is returned on success.
    fn write_to_memory(&self) -> Result<Vec<u8>, Self::Error> {
        let mut w = Cursor::new(Vec::new());
        self.write_to(&mut w)?;
        Ok(w.into_inner())
    }
}

/// Represents one of the supported file formats.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

mod internal {
    pub trait DoNotImplement {}
}

/// Abstracts over all IO primitive types.
///
/// Note that this trait is exactly implemented for the types that are included
/// in [`PrimitiveType`] and [`PrimitiveValue`]. Thus, this is a closed set of
/// implementing types (unusual for a trait). As a consequence, you are not
/// supposed to implement this trait for your own types! That's why this trait
/// has a supertrait called `DoNotImplement`. Said supertrait is crate-private,
/// so you can't implement it for other types.
pub trait Primitive: PrimitiveNum + internal::DoNotImplement {
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
        AsPrimitive::as_primitive(self)
    }
}

macro_rules! impl_primitive {
    ($ty:ident, $variant:ident) => {
        impl internal::DoNotImplement for $ty {}
        impl Primitive for $ty {
            const TY: PrimitiveType = PrimitiveType::$variant;
            fn to_primitive_value(&self) -> PrimitiveValue {
                PrimitiveValue::$variant(*self)
            }
        }

        impl AsPrimitive<$ty> for $ty {
            fn as_primitive(self) -> Option<$ty> {
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

/// A helper trait to implement [`Primitive::downcast_as`]. Ignore this.
pub trait AsPrimitive<Target: Primitive> {
    fn as_primitive(self) -> Option<Target>;
}

impl<Source: Primitive, Target: Primitive> AsPrimitive<Target> for Source {
    default fn as_primitive(self) -> Option<Target> {
        None
    }
}


// ==========================================================================
// ===== {Streaming/Mem}-Sinks and Sources
// ==========================================================================

pub trait StreamingSource {
    type Error: Fail;
    fn transfer_to<S: MemSink>(self, sink: &mut S) -> Result<(), Self::Error>;
}

pub trait MemSink {
    fn add_vertex(&mut self) -> VertexHandle;
    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle;

    fn set_vertex_position<N: Primitive>(
        &mut self,
        _: VertexHandle,
        _position: Point3<N>,
    ) {}
}

pub trait StreamingSink {
    type Error: Fail;
    fn transfer_from<S: MemSource>(self, src: &S) -> Result<(), Self::Error>;
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
