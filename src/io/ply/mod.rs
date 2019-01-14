//! Reading from and writing to PLY files (Polygon File Format).
//!
//! TODO: explain everything.

// TODO PLY things:
// - Figure out colors (write and read)
// - How to name the `Serializer`? Maybe `WriterBuilder`?

use std::{
    io,
    path::Path,
};

use failure::Fail;

use crate::{
    Empty,
    io::{FileEncoding, StreamingSource, MemSink, parse},
};

mod read;
mod write;

#[cfg(test)]
mod tests;


pub use self::read::{
    ElementDef, Property, PropertyDef, PropertyType, PropIndex, RawElement,
    RawElementGroup, RawElementIter, RawResult, RawSink, Reader, ScalarType,
    ScalarTypeParseError, ListLenType, RawOffset,
};
pub use self::write::{Config, Writer};


/// Reads the PLY file with the given filename into an empty instance of `T`
/// and returns that instance.
///
/// If you need more control about how and what to read, take a look at
/// [`Reader`].
///
/// TODO: Example
pub fn read<T: Empty + MemSink, P: AsRef<Path>>(path: P) -> Result<T, Error> {
    let reader = Reader::open(path)?;
    let mut out = T::empty();
    reader.transfer_to(&mut out)?;
    Ok(out)
}


/// The encoding of a PLY file.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Encoding {
    /// Everything is stored as an ASCII string. You should usually not use
    /// this as this encoding is very space-inefficient.
    Ascii,

    /// Binary encoding where all numeric types are stored in big endian
    /// layout. The header is still ASCII.
    BinaryBigEndian,

    /// Binary encoding where all numeric types are stored in little endian
    /// layout. The header is still ASCII.
    BinaryLittleEndian,
}

impl From<FileEncoding> for Encoding {
    fn from(src: FileEncoding) -> Self {
        match src {
            FileEncoding::Ascii => Encoding::Ascii,
            FileEncoding::BinaryBigEndian => Encoding::BinaryBigEndian,
            FileEncoding::BinaryLittleEndian => Encoding::BinaryLittleEndian,
        }
    }
}

impl From<Encoding> for FileEncoding {
    fn from(src: Encoding) -> Self {
        match src {
            Encoding::Ascii => FileEncoding::Ascii,
            Encoding::BinaryBigEndian => FileEncoding::BinaryBigEndian,
            Encoding::BinaryLittleEndian => FileEncoding::BinaryLittleEndian,
        }
    }
}

/// Everything that can go wrong when writing or reading PLY files.
#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "Parsing error: {}", _0)]
    Parse(parse::Error),

    #[fail(display = "IO error: {}", _0)]
    Io(io::Error),
}

impl From<io::Error> for Error {
    fn from(src: io::Error) -> Self {
        Error::Io(src)
    }
}

impl From<parse::Error> for Error {
    fn from(src: parse::Error) -> Self {
        Error::Parse(src)
    }
}


/// A writer which can serialize any PLY type into itself.
///
/// This trait is implemented internally once for each format: ASCII, binary
/// little endian and binary big endian. You are not really supposed to
/// implement this trait yourself; it doesn't really make sense.
pub trait PropSerializer {
    fn serialize_u8(self, v: u8) -> Result<(), Error>;
    fn serialize_u16(self, v: u16) -> Result<(), Error>;
    fn serialize_u32(self, v: u32) -> Result<(), Error>;
    fn serialize_i8(self, v: i8) -> Result<(), Error>;
    fn serialize_i16(self, v: i16) -> Result<(), Error>;
    fn serialize_i32(self, v: i32) -> Result<(), Error>;
    fn serialize_f32(self, v: f32) -> Result<(), Error>;
    fn serialize_f64(self, v: f64) -> Result<(), Error>;

    /// Serialize a sequence of values where the length of the sequence was
    /// known beforehand.
    ///
    /// In particular, the sequence length must not vary in between calls of
    /// this method.
    fn serialize_fixed_len_seq<'a, I, E>(self, values: I) -> Result<(), Error>
    where
        I: IntoIterator<Item = &'a E>,
        E: 'a + SingleSerialize;

    /// Serialize a sequence of values with varying length.
    ///
    /// The length of each individual sequence is stored in the file.
    fn serialize_dyn_len_seq<'a, I, E>(self, values: I) -> Result<(), Error>
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
    fn serialize<S: PropSerializer>(&self, ser: S) -> Result<(), Error>;
}

/// A `Serialize` type which is not a sequence, but a single primitive type.
/// The `TYPE` constant of the `Serialize` impl has to be
/// `PropType::Single(Self::SINGLE_TYPE)`.
pub trait SingleSerialize: Serialize {
    const SINGLE_TYPE: SinglePropType;
}

/// A primitive PLY type. There are 8 in total: 2 floating point types, 3
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

/// Represents a (possibly compound) PLY data type.
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

            fn serialize<S: PropSerializer>(&self, ser: S) -> Result<(), Error> {
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

            fn serialize<S: PropSerializer>(&self, ser: S) -> Result<(), Error> {
                ser.serialize_fixed_len_seq(self)
            }
        }
    }
}

impl_for_array!(1);
impl_for_array!(2);
impl_for_array!(3);
impl_for_array!(4);
impl_for_array!(5);
impl_for_array!(6);
impl_for_array!(7);
impl_for_array!(8);
impl_for_array!(16);

impl<T: SingleSerialize> Serialize for &[T] {
    const TYPE: PropType = PropType::DynLenSeq(T::SINGLE_TYPE);

    fn serialize<S: PropSerializer>(&self, ser: S) -> Result<(), Error> {
        ser.serialize_dyn_len_seq(*self)
    }
}
