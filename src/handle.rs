use std::fmt;

use crate::{
    io::{PropType, PrimitiveType, PropSerialize, PropSerializer, PrimitiveProp},
};

pub type DefaultIndex = u32;

// TODO: It would be really nice to let the user choose the index to use.
// However this complicates the API at several places, making some things even
// impossible without future Rust features (like GATs). So for now we will
// simply use `u32` as index everywhere.
pub trait DefaultIndexExt: Copy {
    const NUM_BYTES: u8;
    fn from_usize(raw: usize) -> Self;
    fn to_usize(&self) -> usize;
    fn next(&self) -> Self;
}

macro_rules! impl_handle_index {
    ($name:ident) => {
        impl DefaultIndexExt for $name {
            const NUM_BYTES: u8 = ::std::mem::size_of::<$name>() as u8;

            fn from_usize(raw: usize) -> Self {
                assert!(raw <= Self::max_value() as usize);
                raw as Self
            }
            fn to_usize(&self) -> usize {
                *self as usize
            }
            fn next(&self) -> Self {
                self + 1
            }
        }
    }
}

// impl_handle_index!(u8);
// impl_handle_index!(u16);
impl_handle_index!(u32);
// impl_handle_index!(u64);
// impl_handle_index!(usize);


pub trait Handle: Copy {
    fn from_idx(idx: u32) -> Self;
    fn idx(&self) -> u32;

    fn from_usize(raw: usize) -> Self {
        Self::from_idx(raw as u32)
    }
    fn to_usize(&self) -> usize {
        self.idx() as usize
    }
}

macro_rules! make_handle_type {
    ($name:ident, $short:expr) => {
        #[derive(Clone, Copy, PartialEq, Eq)]
        pub struct $name(DefaultIndex);

        impl Handle for $name {
            fn from_idx(idx: u32) -> Self {
                $name(idx)
            }
            fn idx(&self) -> u32 {
                self.0
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                $short.fmt(f)?;
                self.idx().fmt(f)
            }
        }

        // TODO: this should be derived
        impl PropSerialize for $name {
            fn ty() -> PropType {
                PropType::Single(PrimitiveType::Uint32)
            }

            fn serialize<S: PropSerializer>(&self, serializer: S) -> Result<(), S::Error> {
                serializer.serialize_u32(self.0)
            }
        }

        impl PrimitiveProp for $name {
            const PRIMITIVE_TY: PrimitiveType = PrimitiveType::Uint32;
        }
    }
}

make_handle_type!(FaceHandle, "F");
make_handle_type!(EdgeHandle, "E");
make_handle_type!(VertexHandle, "V");
