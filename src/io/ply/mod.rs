use std::{
    io,
};

use failure::Fail;


mod write;

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
}

pub trait Serialize {
    const HEADER_TYPE_NAME: &'static str;
    fn serialize<S: PropSerializer>(&self, ser: S) -> Result<(), S::Error>;
}

macro_rules! impl_primitive_type {
    ($name:ident, $ser_fn:ident, $header_name:expr) => {
        impl Serialize for $name {
            const HEADER_TYPE_NAME: &'static str = $header_name;

            fn serialize<S: PropSerializer>(&self, ser: S) -> Result<(), S::Error> {
                ser.$ser_fn(*self)
            }
        }
    }
}

impl_primitive_type!(u8,  serialize_u8,  "uchar");
impl_primitive_type!(u16, serialize_u16, "ushort");
impl_primitive_type!(u32, serialize_u32, "uint");
impl_primitive_type!(i8,  serialize_i8,  "char");
impl_primitive_type!(i16, serialize_i16, "short");
impl_primitive_type!(i32, serialize_i32, "int");
impl_primitive_type!(f32, serialize_f32, "float");
impl_primitive_type!(f64, serialize_f64, "double");
