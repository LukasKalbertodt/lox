use std::{
    fmt,
};

use auto_impl::auto_impl;
use fev_core::{
    handle::{EdgeHandle, FaceHandle, Handle, VertexHandle},
    prop::{LabeledPropList, PropLabel},
};


/// The primitive types of the `fev-io` data model.
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

/// The types of the `fev-io` data model.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DataType {
    /// A single value of a primitive type.
    Single(PrimitiveType),

    /// Multiple values of a primitive type.
    ///
    /// The number of values can vary between different property values, even
    /// if the two values belong to the same thing (e.g. mesh). If the length
    /// of all values is the same for one mesh, use `FixedLen` instead.
    VariableLen(PrimitiveType),

    /// Multiple values of a primitive type with a fixed number of values,
    /// specified by `len`.
    ///
    /// `len` is expected to be a fairly small number.
    FixedLen {
        ty: PrimitiveType,
        len: u64,
    },
}

impl DataType {
    /// Returns the primitive type of this data type.
    pub fn primitive_type(&self) -> PrimitiveType {
        match *self {
            DataType::Single(t) => t,
            DataType::VariableLen(t) => t,
            DataType::FixedLen { ty, .. } => ty,
        }
    }
}

/// A type that can be represented as a data type from the `fev-io` data model.
///
/// The mapping to the data type has to be known at compile time.
#[auto_impl(&)]
pub trait Serialize {
    /// The data type representing this type.
    const DATA_TYPE: DataType;

    /// Serialize this value into the given serializer.
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<(), S::Error>;
}

/// A type serializable as a single primitive type from the `fev-io` data
/// model.
///
/// When this trait is implemented, `<Self as Serialize>::DATA_TYPE` should be
/// equal to `DataType::Single(Self::PRIMITIVE_TYPE)`.
#[auto_impl(&)]
pub trait SinglePrimitive: Serialize {
    /// The primitive type representing this type.
    const PRIMITIVE_TYPE: PrimitiveType;

    /// Serialize this value into the given single-primitive-serializer.
    fn serialize_single<S: SinglePrimitiveSerializer>(
        &self,
        serializer: S,
    ) -> Result<(), S::Error>;
}


macro_rules! impl_for_primitives {
    ($name:ident, $func:ident, $ty:ident) => {
        impl Serialize for $name {
            const DATA_TYPE: DataType = DataType::Single(PrimitiveType::$ty);

            fn serialize<S: Serializer>(&self, serializer: S) -> Result<(), S::Error> {
                self.serialize_single(serializer)
            }
        }

        impl SinglePrimitive for $name {
            const PRIMITIVE_TYPE: PrimitiveType = PrimitiveType::$ty;

            fn serialize_single<S: SinglePrimitiveSerializer>(
                &self,
                serializer: S,
            ) -> Result<(), S::Error> {
                serializer.$func(*self)
            }
        }
    }
}

impl_for_primitives!(bool, serialize_bool, Bool);
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

macro_rules! impl_serialize_for_array {
    ($n:expr) => {
        impl<T: SinglePrimitive> Serialize for [T; $n] {
            const DATA_TYPE: DataType = DataType::FixedLen {
                ty: T::PRIMITIVE_TYPE,
                len: $n,
            };

            fn serialize<S: Serializer>(&self, serializer: S) -> Result<(), S::Error> {
                serializer.serialize_fixed_len_seq(self)
            }
        }
    }
}

impl_serialize_for_array!(1);
impl_serialize_for_array!(2);
impl_serialize_for_array!(3);
impl_serialize_for_array!(4);
impl_serialize_for_array!(5);
impl_serialize_for_array!(6);

impl<'a, T: 'a + SinglePrimitive> Serialize for &'a [T] {
    const DATA_TYPE: DataType = DataType::VariableLen(T::PRIMITIVE_TYPE);

    fn serialize<S: Serializer>(&self, serializer: S) -> Result<(), S::Error> {
        serializer.serialize_variable_len_seq(self)
    }
}

macro_rules! impl_serialize_for_tuple {
    ($len:expr, $T:ident, $type:ty => $($binding:ident),+) => {
        impl<$T: SinglePrimitive + Copy> Serialize for $type {
            const DATA_TYPE: DataType = DataType::FixedLen {
                ty: $T::PRIMITIVE_TYPE,
                len: $len,
            };

            fn serialize<S: Serializer>(&self, serializer: S) -> Result<(), S::Error> {
                let ($($binding ,)+) = *self;
                serializer.serialize_fixed_len_seq(&[$($binding),+])
            }
        }
    }
}

impl_serialize_for_tuple!(1, T, (T,) => a);
impl_serialize_for_tuple!(2, T, (T, T) => a, b);
impl_serialize_for_tuple!(3, T, (T, T, T) => a, b, c);
impl_serialize_for_tuple!(4, T, (T, T, T, T) => a, b, c, d);
impl_serialize_for_tuple!(5, T, (T, T, T, T, T) => a, b, c, d, e);
impl_serialize_for_tuple!(6, T, (T, T, T, T, T, T) => a, b, c, d, e, f);

macro_rules! impl_serialize_for_handle {
    ($name:ident) => {
        impl Serialize for $name {
            // TODO: change if index has a different type.
            const DATA_TYPE: DataType = DataType::Single(PrimitiveType::Uint32);

            fn serialize<S: Serializer>(&self, serializer: S) -> Result<(), S::Error> {
                self.serialize_single(serializer)
            }
        }

        impl SinglePrimitive for $name {
            const PRIMITIVE_TYPE: PrimitiveType = PrimitiveType::Uint32;

            fn serialize_single<S: SinglePrimitiveSerializer>(
                &self,
                serializer: S,
            ) -> Result<(), S::Error> {
                serializer.serialize_u32(self.id())
            }
        }
    }
}

impl_serialize_for_handle!(EdgeHandle);
impl_serialize_for_handle!(FaceHandle);
impl_serialize_for_handle!(VertexHandle);


/// A type that can serialize (potentially only a subset of) the types from
/// the `fev-io` data model.
///
/// This trait is fairly similar to `serde::Serializer`, with two differences:
/// - This serializer cannot serialize as many combined types (no maps, ...).
/// - All methods return `Result<(), _>`, so there is no `Ok` data. Instead,
///   the serializer is expected to change some state (it usually means that
///   the serializer contains a writer in which the serialized data is
///   written).
///
/// The data model of `fev-io` is way simpler than the Serde data model,
/// because usually, mesh formats don't support a wide variety of types and
/// type constructs. Our data model contains the following types:
/// - `bool`
/// - Integers (`u8`, `i8`, ..., `u64`, `i64`)
/// - Floats (`f32`, `f64`)
/// - Sequences with fixed length (like Rust's arrays `[T; N]`)
/// - Sequences with variable length (like Rust's slices `[T]`)
///
/// The serializer functionality is split into two traits: The
/// [`SinglePrimitiveSerializer`] (which can only serialize single primitives)
/// and this trait which additionally can serialize sequences.
///
/// Types that implement this trait don't necessarily need to be able to
/// serialize all types from our data model. In that case an error can be
/// returned. Note that the documentation of implementing types should clearly
/// state if some operations are not supported.
///
/// However, if a method can be implemented in a way that won't result in bad
/// surprises, it should be. For example, some formats don't support the `bool`
/// type, but integer types. In that case a `false` can simply be serialized as
/// `0` and a `true` as `1`. Similarly, if a format doesn't support fixed
/// length sequences, but variable length sequences, the former can simply
/// encoded as the latter.
pub trait Serializer: SinglePrimitiveSerializer {

    // TODO: We might want to change those to return another serializer instead
    // of taking a slice.
    fn serialize_fixed_len_seq<T: Serialize>(self, v: &[T]) -> Result<(), Self::Error>;
    fn serialize_variable_len_seq<T: Serialize>(self, v: &[T]) -> Result<(), Self::Error>;
}

/// A type that can serialize (potentially only a subset of) the single
/// primitive types from the `fev-io` data model.
///
/// The primitive types from the `fev_io` data model are:
/// - `bool`
/// - Integers (`u8`, `i8`, ..., `u64`, `i64`)
/// - Floats (`f32`, `f64`)
///
/// See the [`Serializer`] documentation for more information.
pub trait SinglePrimitiveSerializer {
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
}

/// A property list where all properties in the list can be serialized.
///
/// If `PropListSerialize` and `LabeledPropList` are implemented, the order of
/// properties has to match! In fact, this trait is pretty useless on its own
/// and is always used together a given list of labels. This list is either
/// provided directly or via the `LabeledPropList` trait. This label list also
/// defines the number of properties in this list.
pub trait PropListSerialize {
    /// Returns the data type of the property with the given index.
    fn data_type_of(prop_index: usize) -> DataType;

    /// Serializes the property with the given index with the given serializer.
    fn serialize_at<S: Serializer>(
        &self,
        prop_index: usize,
        serializer: S,
    ) -> Result<(), S::Error>;

    fn typed_labels() -> Vec<TypedLabel>
    where
        Self: LabeledPropList,
    {
        (0..Self::num_props()).map(|i| {
            TypedLabel {
                label: Self::label_of(i),
                data_type: Self::data_type_of(i)
            }
        }).collect()
    }
}

impl<'a, T: 'a + PropListSerialize> PropListSerialize for &'a T {
    fn data_type_of(prop_index: usize) -> DataType {
        T::data_type_of(prop_index)
    }

    fn serialize_at<S: Serializer>(
        &self,
        prop_index: usize,
        serializer: S,
    ) -> Result<(), S::Error> {
        (*self).serialize_at(prop_index, serializer)
    }
}

impl PropListSerialize for () {
    fn data_type_of(_: usize) -> DataType {
        panic!() // TODO
    }

    fn serialize_at<S: Serializer>(&self, _: usize, _: S) -> Result<(), S::Error> {
        panic!() // TODO
    }
}

// impl<T: Serialize> PropListSerialize for T {
//     fn data_type_of(prop_index: usize) -> DataType {
//         match prop_index {
//             0 => T::DATA_TYPE,
//             _ => unreachable!(),
//         }
//     }

//     fn serialize_at<S: Serializer>(
//         &self,
//         prop_index: usize,
//         serializer: S,
//     ) -> Result<(), S::Error> {
//         match prop_index {
//             0 => self.serialize(serializer),
//             _ => unreachable!(),
//         }
//     }
// }


#[derive(Clone)]
pub struct TypedLabel {
    pub label: PropLabel,
    pub data_type: DataType,
}

impl fmt::Debug for TypedLabel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}: {:?}", self.label, self.data_type)
    }
}


pub struct SingleProp<T: Serialize>(pub T);

impl<T: Serialize> PropListSerialize for SingleProp<T> {
    fn data_type_of(prop_index: usize) -> DataType {
        match prop_index {
            0 => T::DATA_TYPE,
            _ => unreachable!(),
        }
    }

    fn serialize_at<S: Serializer>(
        &self,
        prop_index: usize,
        serializer: S,
    ) -> Result<(), S::Error> {
        match prop_index {
            0 => self.0.serialize(serializer),
            _ => unreachable!(),
        }
    }
}
