

pub type DefaultIndex = u32;

pub trait HandleIndex: Copy {
    fn from_usize(raw: usize) -> Self;
}

macro_rules! impl_handle_index {
    ($name:ident) => {
        impl HandleIndex for $name {
            fn from_usize(raw: usize) -> Self {
                assert!(raw <= Self::max_value() as usize);
                raw as Self
            }
        }
    }
}

impl_handle_index!(u8);
impl_handle_index!(u16);
impl_handle_index!(u32);
impl_handle_index!(u64);
impl_handle_index!(usize);


macro_rules! make_handle_type {
    ($name:ident, $short:expr) => {
        #[derive(Clone, Copy, PartialEq, Eq)]
        pub struct $name<Idx: HandleIndex>(Idx);

        impl<Idx: HandleIndex> $name<Idx> {
            pub fn idx(&self) -> Idx {
                self.0
            }

            pub(crate) fn from_usize(idx: usize) -> Self {
                $name(Idx::from_usize(idx))
            }
        }

        impl<Idx: HandleIndex + ::std::fmt::Debug> ::std::fmt::Debug for $name<Idx> {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                $short.fmt(f)?;
                self.idx().fmt(f)
            }
        }
    }
}

make_handle_type!(FaceHandle, "F");
make_handle_type!(EdgeHandle, "E");
make_handle_type!(VertexHandle, "V");
