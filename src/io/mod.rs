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

    fn serialize<S: PrimitiveSerializer>(&self, serializer: S) -> Result<(), S::Error>;
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
            ) -> Result<(), S::Error> {
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


/// Defines *what* a property represents semantically as well as the type of
/// the property data.
///
/// This is closely related to the trait `LabeledPropSet` and is mainly used
/// for serialization.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PropLabel {
    Position {
        scalar_ty: PrimitiveType,
    },
    Normal {
        scalar_ty: PrimitiveType,
    },
    Named {
        name: String,
        ty: PrimitiveType,
    },
}

/// A set of labeled properties.
pub trait LabeledPropSet {
    /// The labels for all properties in this set.
    ///
    /// The order of these labels has to match the order in which the
    /// properties are serialized in `serialize()`!
    const LABELS: &'static [PropLabel];

    /// Serializes all properties in this set with the given serializer.
    fn serialize<S>(&self, serializer: S) -> Result<(), S::Error>
    where
        S: PropSerializer;
}

pub trait PrimitiveSerializer {
    type Error;

    fn serialize_bool(self, v: bool) -> Result<(), Self::Error>;
    fn serialize_i8(self, v: i8) -> Result<(), Self::Error>;
    fn serialize_i16(self, v: i16) -> Result<(), Self::Error>;
    fn serialize_i32(self, v: i32) -> Result<(), Self::Error>;
    fn serialize_i64(self, v: i64) -> Result<(), Self::Error>;
    fn serialize_u8(self, v: u8) -> Result<(), Self::Error>;
    fn serialize_u16(self, v: u16) -> Result<(), Self::Error>;
    fn serialize_u32(self, v: u32) -> Result<(), Self::Error>;
    fn serialize_u64(self, v: u64) -> Result<(), Self::Error>;
    fn serialize_f32(self, v: f32) -> Result<(), Self::Error>;
    fn serialize_f64(self, v: f64) -> Result<(), Self::Error>;

    // TODO: more primitives?
}

pub trait PropSerializer {
    type Error;

    fn serialize_position<PosT>(&mut self, v: &PosT) -> Result<(), Self::Error>
    where
        PosT: Pos3Like,
        PosT::Scalar: PrimitiveSerialize;

    fn serialize_normal<NormalT>(&mut self, v: &NormalT) -> Result<(), Self::Error>
    where
        NormalT: Vec3Like,
        NormalT::Scalar: PrimitiveSerialize;

    fn serialize_named(
        &mut self,
        name: &str,
        v: &impl PrimitiveSerialize,
    ) -> Result<(), Self::Error>;
}


// TODO: Make better with GATs
pub trait IntoMeshWriter<'a, MeshT>
where
    MeshT: TriMesh + 'a,
    MeshT::VertexProp: LabeledPropSet,
{
    type Error;
    type Writer: MeshWriter<'a, Error = Self::Error>;

    fn serialize(self, mesh: &'a MeshT) -> Result<Self::Writer, Self::Error>;
}

pub trait MeshWriter<'a> {
    type Error;

    // fn add_vertex_prop<PropT: PropSerialize>(
    //     &mut self,
    //     prop: &PropT,
    // ) -> Result<&mut Self, Self::Error>;

    fn write(&mut self, writer: impl Write) -> Result<(), Self::Error>;
}
