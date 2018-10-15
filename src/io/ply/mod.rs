use std::{
    io,
};

use failure::Fail;


mod write;

#[cfg(test)]
mod tests;


pub use self::write::{Serializer, Writer};


/// The format of a PLY file.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Format {
    Ascii,
    BinaryBigEndian,
    BinaryLittleEndian,
}

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "IO error: {}", _0)]
    Io(io::Error),
}

impl From<io::Error> for Error {
    fn from(src: io::Error) -> Self {
        Error::Io(src)
    }
}

/// A writer which can serialize any PLY type into itself.
///
/// This trait is implemented internally once for each format: ASCII, binary
/// little endian and binary big endian. You are not really supposed to
/// implement this trait yourself; it doesn't really make sense.
pub trait PropSerializer {
    type Error;

    fn serialize_u8(self, v: u8) -> Result<(), Self::Error>;
    fn serialize_u16(self, v: u16) -> Result<(), Self::Error>;
    fn serialize_u32(self, v: u32) -> Result<(), Self::Error>;
    fn serialize_i8(self, v: i8) -> Result<(), Self::Error>;
    fn serialize_i16(self, v: i16) -> Result<(), Self::Error>;
    fn serialize_i32(self, v: i32) -> Result<(), Self::Error>;
    fn serialize_f32(self, v: f32) -> Result<(), Self::Error>;
    fn serialize_f64(self, v: f64) -> Result<(), Self::Error>;

    fn serialize_fixed_len_seq<'a, I, E>(self, values: I) -> Result<(), Self::Error>
    where
        I: IntoIterator<Item = &'a E>,
        E: 'a + SingleSerialize;

    fn serialize_dyn_len_seq<'a, I, E>(self, values: I) -> Result<(), Self::Error>
    where
        I: IntoIterator<Item = &'a E>,
        I::IntoIter: ExactSizeIterator,
        E: 'a + SingleSerialize;
}

/// A type that can be serialized into a PLY file.
///
/// You might implement this trait for your own types, but you usually don't
/// have to. There alrady exist implementations for Rust's primitive types,
/// arrays and slices.
pub trait Serialize {
    /// The PLY type. Can be a single primitive type or an array/slice. This
    /// constant is used to generate the header field.
    const TYPE: PropType;

    /// Serializes itself with the given serializer.
    fn serialize<S: PropSerializer>(&self, ser: S) -> Result<(), S::Error>;
}

/// A `Serialize` type which is not a sequence, but a single primitive type.
/// The `TYPE` constant of the `Serialize` impl has to be
/// `PropType::Single(Self::SINGLE_TYPE)`.
pub trait SingleSerialize: Serialize {
    const SINGLE_TYPE: SinglePropType;
}

/// A primitive PLY type. There are 8 in total, 2 floating point types, 3
/// signed and 3 unsigned integers.
#[derive(Debug)]
pub enum SinglePropType {
    Uchar,
    Ushort,
    Uint,
    Char,
    Short,
    Int,
    Float,
    Double,
}

impl SinglePropType {
    /// Returns the type name used in the header (e.g. `short` for `i16`). This
    /// is simply the variant name in lowercase.
    pub fn ply_type_name(&self) -> &'static str {
        match *self {
            SinglePropType::Uchar => "uchar",
            SinglePropType::Ushort => "ushort",
            SinglePropType::Uint => "uint",
            SinglePropType::Char => "char",
            SinglePropType::Short => "short",
            SinglePropType::Int => "int",
            SinglePropType::Float => "float",
            SinglePropType::Double => "double",
        }
    }
}

/// Represents a possibly compound PLY data type.
#[derive(Debug)]
pub enum PropType {
    /// Just a single value of the given type.
    Single(SinglePropType),

    /// A sequence which length is not known and/or varies between different
    /// property values.
    DynLenSeq(SinglePropType),

    /// A sequence which length is known up front and which does not vary.
    FixedLenSeq {
        len: usize,
        ty: SinglePropType,
    }
}


// ==============================================================================================
// ===== Implementations of `Serialize` and `SingleSerialize`
// ==============================================================================================

macro_rules! impl_primitive_type {
    ($name:ident, $ser_fn:ident, $ty:ident) => {
        impl Serialize for $name {
            const TYPE: PropType = PropType::Single(SinglePropType::$ty);

            fn serialize<S: PropSerializer>(&self, ser: S) -> Result<(), S::Error> {
                ser.$ser_fn(*self)
            }
        }

        impl SingleSerialize for $name {
            const SINGLE_TYPE: SinglePropType = SinglePropType::$ty;
        }
    }
}

impl_primitive_type!(u8,  serialize_u8,  Uchar);
impl_primitive_type!(u16, serialize_u16, Ushort);
impl_primitive_type!(u32, serialize_u32, Uint);
impl_primitive_type!(i8,  serialize_i8,  Char);
impl_primitive_type!(i16, serialize_i16, Short);
impl_primitive_type!(i32, serialize_i32, Int);
impl_primitive_type!(f32, serialize_f32, Float);
impl_primitive_type!(f64, serialize_f64, Double);


macro_rules! impl_for_array {
    ($len:expr) => {
        impl<T: SingleSerialize> Serialize for [T; $len] {
            const TYPE: PropType = PropType::FixedLenSeq { len: $len, ty: T::SINGLE_TYPE };

            fn serialize<S: PropSerializer>(&self, ser: S) -> Result<(), S::Error> {
                ser.serialize_fixed_len_seq(self)
            }
        }
    }
}

impl_for_array!(1);
impl_for_array!(2);
impl_for_array!(3);

impl<T: SingleSerialize> Serialize for &[T] {
    const TYPE: PropType = PropType::DynLenSeq(T::SINGLE_TYPE);

    fn serialize<S: PropSerializer>(&self, ser: S) -> Result<(), S::Error> {
        ser.serialize_dyn_len_seq(*self)
    }
}
