use std::{
    fmt,
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
    Bool,
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

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            PrimitiveType::Bool => "bool",
            PrimitiveType::Uint8 => "u8",
            PrimitiveType::Uint16 => "u16",
            PrimitiveType::Uint32 => "u32",
            PrimitiveType::Uint64 => "u64",
            PrimitiveType::Int8 => "i8",
            PrimitiveType::Int16 => "i16",
            PrimitiveType::Int32 => "i32",
            PrimitiveType::Int64 => "i64",
            PrimitiveType::Float32 => "f32",
            PrimitiveType::Float64 => "f64",
        }.fmt(f)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PropType {
    // A single value of a primitive type.
    Single(PrimitiveType),

    /// Multiple values of a primitive type.
    ///
    /// The number of values can vary between different property values, even
    /// if the two values belong to the same mesh. If the length of all values
    /// is the same for one mesh, use `FixedLen` instead.
    VariableLen(PrimitiveType),

    /// Multiple values of a primitive type with a fixed number of values,
    /// specified by `len`.
    FixedLen {
        ty: PrimitiveType,
        len: u64,
    },
}

impl PropType {
    pub fn primitive_type(&self) -> PrimitiveType {
        match *self {
            PropType::Single(ty) => ty,
            PropType::VariableLen(ty) => ty,
            PropType::FixedLen { ty, .. } => ty,
        }
    }
}

impl fmt::Display for PropType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            PropType::Single(ty) => ty.fmt(f),
            PropType::VariableLen(ty) => write!(f, "[{}]", ty),
            PropType::FixedLen { ty, len } => write!(f, "[{}; {}]", ty, len),
        }
    }
}

pub trait PrimitiveProp: PropSerialize {
    const PRIMITIVE_TY: PrimitiveType;
}

pub trait PropSerialize {
    // TODO: make assoc const?
    fn ty() -> PropType;
    fn serialize<S: PropSerializer>(&self, serializer: S) -> Result<(), S::Error>;
}

macro_rules! impl_for_primitives {
    ($name:ident, $func:ident, $ty:ident) => {
        impl PropSerialize for $name {
            fn ty() -> PropType {
                PropType::Single(PrimitiveType::$ty)
            }

            fn serialize<S: PropSerializer>(&self, serializer: S) -> Result<(), S::Error> {
                serializer.$func(*self)
            }
        }

        impl PrimitiveProp for $name {
            const PRIMITIVE_TY: PrimitiveType = PrimitiveType::$ty;
        }
    }
}

impl_for_primitives!(u8,  serialize_u8,  Uint8);
impl_for_primitives!(u16, serialize_u16, Uint16);
impl_for_primitives!(u32, serialize_u32, Uint32);
impl_for_primitives!(u64, serialize_u64, Uint64);
impl_for_primitives!(i8,  serialize_i8,  Int8);
impl_for_primitives!(i16, serialize_i16, Int16);
impl_for_primitives!(i32, serialize_i32, Int32);
impl_for_primitives!(i64, serialize_i64, Int64);
impl_for_primitives!(f32, serialize_f32, Float32);
impl_for_primitives!(f64, serialize_f64, Float64);

macro_rules! impl_prop_serialize_for_arrays {
    ($n:expr) => {
        impl<T: PrimitiveProp> PropSerialize for [T; $n] {
            fn ty() -> PropType {
                PropType::FixedLen {
                    ty: T::PRIMITIVE_TY,
                    len: $n,
                }
            }

            fn serialize<S: PropSerializer>(&self, serializer: S) -> Result<(), S::Error> {
                serializer.serialize_fixed_len_seq(self)
            }
        }
    }
}

impl_prop_serialize_for_arrays!(1);
impl_prop_serialize_for_arrays!(2);
impl_prop_serialize_for_arrays!(3);
impl_prop_serialize_for_arrays!(4);
impl_prop_serialize_for_arrays!(5);
impl_prop_serialize_for_arrays!(6);


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
        ty: PropType,
    },
}

/// A set of labeled properties.
pub trait LabeledPropSet {
    /// The labels for all properties in this set.
    ///
    /// The order of these labels has to match the order in which the
    /// properties are serialized in `serialize()`!
    const LABELS: &'static [PropLabel];
}

pub trait PropSetSerialize {
    /// Serializes all properties in this set with the given serializer.
    fn serialize<S>(&self, serializer: S) -> Result<(), S::Error>
    where
        S: PropSetSerializer;
}

pub trait PropSerializer {
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

    fn serialize_fixed_len_seq<T: PropSerialize>(self, v: &[T]) -> Result<(), Self::Error>;

    // TODO: more primitives?
}

pub trait PropSetSerializer {
    type Error;

    fn serialize_position<PosT>(&mut self, v: &PosT) -> Result<(), Self::Error>
    where
        PosT: Pos3Like,
        PosT::Scalar: PropSerialize;

    fn serialize_normal<NormalT>(&mut self, v: &NormalT) -> Result<(), Self::Error>
    where
        NormalT: Vec3Like,
        NormalT::Scalar: PropSerialize;

    fn serialize_named(
        &mut self,
        name: &str,
        v: &impl PropSerialize,
    ) -> Result<(), Self::Error>;
}


// TODO: Make better with GATs
pub trait IntoMeshWriter<'a, MeshT>
where
    MeshT: TriMesh + 'a,
    MeshT::VertexProp: LabeledPropSet + PropSetSerialize,
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

pub trait PropLabeler<T> {
    type Serialize: PropSetSerialize;

    fn labels(&self) -> PropLabel;
    fn wrap(&self, v: T) -> Self::Serialize;
}

pub struct NamedLabel<S: Into<String> + Clone>(S);


impl<T, S> PropLabeler<T> for NamedLabel<S>
where
    S: Into<String> + Clone,
    T: PropSerialize,
{
    type Serialize = WithNamedLabel<T>;

    fn labels(&self) -> PropLabel {
        PropLabel::Named {
            name: self.0.clone().into(),
            ty: T::ty(),
        }
    }
    fn wrap(&self, v: T) -> Self::Serialize {
        WithNamedLabel {
            wrapped: v,
            name: self.0.clone().into(),
        }
    }
}

pub struct WithNamedLabel<T> {
    wrapped: T,
    name: String,
}

impl<T: PropSerialize> PropSetSerialize for WithNamedLabel<T> {
    fn serialize<S>(&self, mut serializer: S) -> Result<(), S::Error>
    where
        S: PropSetSerializer,
    {
        serializer.serialize_named(&self.name, &self.wrapped)
    }
}
