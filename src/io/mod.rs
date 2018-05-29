use std::{
    fmt::Display,
    io::{self, Write},
};



mod ply;

pub use self::ply::Ply;


// pub trait MeshSerializer {
//     type Err: Error;


//     // TODO: is `Display` really appropriate here?
//     fn serialize<M, W>(&self, w: &mut W, mesh: &M) -> Result<(), Self::Err>
//         where M: TriMesh,
//               M::Idx: PrimitiveSerialize,
//               W: Write;
// }

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrimitiveType {
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Int8,
    Int16,
    Int32,
    Int64,
    Float32,
    Float64,
}

pub trait PrimitiveSerialize: Display {
    fn ty() -> PrimitiveType where Self: Sized;
    fn write_binary_be(&self, w: &mut Write) -> io::Result<()>;
    fn write_binary_le(&self, w: &mut Write) -> io::Result<()>;

    fn write_ascii(&self, w: &mut Write) -> io::Result<()> {
        write!(w, "{}", self)
    }
}

macro_rules! impl_primitive_serialize {
    ($name:ident, $variant:ident) => {
        impl PrimitiveSerialize for $name {
            fn ty() -> PrimitiveType {
                PrimitiveType::$variant
            }
            fn write_binary_be(&self, _w: &mut Write) -> io::Result<()> {
                // TODO: Use `byteorder` crate
                unimplemented!()
            }
            fn write_binary_le(&self, _w: &mut Write) -> io::Result<()> {
                // TODO: Use `byteorder` crate
                unimplemented!()
            }
        }
    }
}

impl_primitive_serialize!(u8, Uint8);
impl_primitive_serialize!(i8, Int8);
impl_primitive_serialize!(u16, Uint16);
impl_primitive_serialize!(i16, Int16);
impl_primitive_serialize!(u32, Uint32);
impl_primitive_serialize!(i32, Int32);
impl_primitive_serialize!(u64, Uint64);
impl_primitive_serialize!(i64, Int64);
impl_primitive_serialize!(f32, Float32);
impl_primitive_serialize!(f64, Float64);


// fn write_ply<Idx: HandleIndex>(mesh: FvTriMesh<Idx>) {
//     for (vH, v) in mesh.vertices() {
//         for attr in attributes[vH] {
//             write!(file, "{} ", attr);
//         }
//     }
// }
