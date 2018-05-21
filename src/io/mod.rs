use std::{
    error::Error,
    fmt::Display,
    io::{self, Write},
};

use TriMesh;


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

pub trait PrimitiveSerialize: Display {
    fn write_binary_be<W: Write>(&self, w: &mut W) -> io::Result<()>;
    fn write_binary_le<W: Write>(&self, w: &mut W) -> io::Result<()>;

    fn write_ascii<W: Write>(&self, w: &mut W) -> io::Result<()> {
        write!(w, "{}", self)
    }
}

macro_rules! impl_primitive_serialize_ints {
    ($name:ident) => {
        impl PrimitiveSerialize for $name {
            fn write_binary_be<W: Write>(&self, _w: &mut W) -> io::Result<()> {
                // TODO: Use `byteorder` crate
                unimplemented!()
            }
            fn write_binary_le<W: Write>(&self, _w: &mut W) -> io::Result<()> {
                // TODO: Use `byteorder` crate
                unimplemented!()
            }
        }
    }
}

impl_primitive_serialize_ints!(u8);
impl_primitive_serialize_ints!(u16);
impl_primitive_serialize_ints!(u32);
impl_primitive_serialize_ints!(u64);
impl_primitive_serialize_ints!(usize);


// fn write_ply<Idx: HandleIndex>(mesh: FvTriMesh<Idx>) {
//     for (vH, v) in mesh.vertices() {
//         for attr in attributes[vH] {
//             write!(file, "{} ", attr);
//         }
//     }
// }
