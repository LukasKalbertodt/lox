use std::{
    fs::File,
    io::{self, BufWriter, Cursor, Write},
    path::Path,
};

use cgmath::Point3;
use failure::Fail;

use crate::{
    Mesh, MeshUnsorted, ExplicitFace,
    handle::{VertexHandle, FaceHandle},
    map::VertexPropMap,
    math::{Pos3Like, PrimitiveNum},
};


pub mod parse;
pub mod ply;
pub mod stl;

/// Types that can be transformed into a [`MeshWriter`].
pub trait IntoMeshWriter<'a, MeshT, PosM>
where
    MeshT: 'a + Mesh + MeshUnsorted + ExplicitFace,
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

#[derive(Debug, Clone, Copy)]
pub enum FileFormat {
    Ply,
    Stl,
}

impl FileFormat {
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
        pub fn $name(&self) -> Option<$ty> {
            match self {
                PrimitiveValue::$variant(x) => Some(*x),
                _ => None,
            }
        }
    }
}

impl PrimitiveValue {
    make_convert_method!(to_u8, u8, Uint8);
    make_convert_method!(to_i8, i8, Int8);
    make_convert_method!(to_u16, u16, Uint16);
    make_convert_method!(to_i16, i16, Int16);
    make_convert_method!(to_u32, u32, Uint32);
    make_convert_method!(to_i32, i32, Int32);
    make_convert_method!(to_f32, f32, Float32);
    make_convert_method!(to_f64, f64, Float64);
}

pub trait Primitive: PrimitiveNum {
    const TY: PrimitiveType;
    fn value(&self) -> PrimitiveValue;
}

macro_rules! impl_primitive {
    ($ty:ident, $variant:ident) => {
        impl Primitive for $ty {
            const TY: PrimitiveType = PrimitiveType::$variant;
            fn value(&self) -> PrimitiveValue {
                PrimitiveValue::$variant(*self)
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


pub trait StreamingSource {
    type Error: Fail;
    fn transfer_to<S: MemSink>(self, sink: &mut S) -> Result<(), Self::Error>;
}

pub trait MemSink {
    fn add_vertex(&mut self) -> VertexHandle;
    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle;

    fn set_vertex_position<N: PrimitiveNum>(
        &mut self,
        _: VertexHandle,
        _position: Point3<N>,
    ) {}
}
