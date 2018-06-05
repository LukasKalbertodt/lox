use std::{
    io::Write,
};

use crate::{
    Pos3Like,
    Vec3Like,
    TriMesh,
};

// mod ply;
mod ply2;

pub use self::ply2::Ply;


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

pub trait PrimitiveSerialize {
    fn ty() -> PrimitiveType;

    fn serialize<S: PrimitiveSerializer>(&self, serializer: S) -> Result<S::Ok, S::Error>;
}

macro_rules! impl_primitive_serialize {
    ($name:ident, $func:ident, $ty:ident) => {
        impl PrimitiveSerialize for $name {
            fn ty() -> PrimitiveType {
                PrimitiveType::$ty
            }

            fn serialize<S: PrimitiveSerializer>(
                &self,
                serializer: S,
            ) -> Result<S::Ok, S::Error> {
                serializer.$func(*self)
            }
        }
    }
}

impl_primitive_serialize!(u8,  serialize_u8,  Uint8);
impl_primitive_serialize!(u16, serialize_u16, Uint16);
impl_primitive_serialize!(u32, serialize_u32, Uint32);
impl_primitive_serialize!(u64, serialize_u64, Uint64);
impl_primitive_serialize!(i8,  serialize_i8,  Int8);
impl_primitive_serialize!(i16, serialize_i16, Int16);
impl_primitive_serialize!(i32, serialize_i32, Int32);
impl_primitive_serialize!(i64, serialize_i64, Int64);
impl_primitive_serialize!(f32, serialize_f32, Float32);
impl_primitive_serialize!(f64, serialize_f64, Float64);


#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PropKind {
    Position {
        scalar_ty: PrimitiveType,
    },
    Normal {
        scalar_ty: PrimitiveType,
    },
    Primitive {
        name: String,
        ty: PrimitiveType,
    },
}


pub trait PropSerialize {
    fn kind() -> PropKind;

    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: PropSerializer;
}

pub trait PrimitiveSerializer {
    type Ok;
    type Error;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error>;
    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error>;
    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error>;
    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error>;
    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error>;
    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error>;
    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error>;
    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error>;
    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error>;
    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error>;
    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error>;

    // TODO: more primitives?
}

pub trait PropSerializer {
    type Ok;
    type Error;

    fn serialize_position<PosT>(&mut self, v: &PosT) -> Result<Self::Ok, Self::Error>
    where
        PosT: Pos3Like,
        PosT::Scalar: PrimitiveSerialize;

    fn serialize_normal<NormalT>(&mut self, v: &NormalT) -> Result<Self::Ok, Self::Error>
    where
        NormalT: Vec3Like,
        NormalT::Scalar: PrimitiveSerialize;

    fn serialize_primitive(
        &mut self,
        name: &str,
        v: &impl PrimitiveSerialize,
    ) -> Result<Self::Ok, Self::Error>;
}


pub trait MeshSerializer<'a> {
    type Error;

    fn add_vertex_prop<PropT: PropSerialize>(
        &mut self,
        prop: &PropT,
    ) -> Result<&mut Self, Self::Error>;

    fn write<MeshT>(&mut self, mesh: &'a MeshT, writer: impl Write) -> Result<(), Self::Error>
    where
        MeshT: TriMesh,
        MeshT::VertexProp: PropSerialize;
}
