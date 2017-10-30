

pub type DefaultIndex = u32;

pub trait HandleIndex: Copy {
    fn num_bytes() -> u8;
    fn from_usize(raw: usize) -> Self;
    fn to_usize(&self) -> usize;
    fn next(&self) -> Self;
}

macro_rules! impl_handle_index {
    ($name:ident) => {
        impl HandleIndex for $name {
            fn num_bytes() -> u8 {
                ::std::mem::size_of::<$name>() as u8
            }
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

impl_handle_index!(u8);
impl_handle_index!(u16);
impl_handle_index!(u32);
impl_handle_index!(u64);
impl_handle_index!(usize);


pub trait Handle: Copy + From<usize> {
    type Idx: HandleIndex;
    fn from_idx(idx: Self::Idx) -> Self;
    fn idx(&self) -> Self::Idx;

    fn from_usize(raw: usize) -> Self {
        let idx = Self::Idx::from_usize(raw);
        Self::from_idx(idx)
    }
    fn to_usize(&self) -> usize {
        self.idx().to_usize()
    }
}

macro_rules! make_handle_type {
    ($name:ident, $short:expr) => {
        #[derive(Clone, Copy, PartialEq, Eq)]
        pub struct $name<Idx: HandleIndex>(Idx);

        impl<Idx: HandleIndex> From<usize> for $name<Idx> {
            fn from(raw: usize) -> Self {
                $name(Idx::from_usize(raw))
            }
        }

        impl<Idx: HandleIndex> Handle for $name<Idx> {
            type Idx = Idx;
            fn from_idx(idx: Self::Idx) -> Self {
                $name(idx)
            }
            fn idx(&self) -> Self::Idx {
                self.0
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
