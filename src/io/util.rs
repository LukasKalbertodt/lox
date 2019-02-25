//! Small utility items used in the `io` module.

use std::{
    marker::PhantomData,
};

use crate::{
    io::Primitive,
    math::{PrimitiveFloat},
};


/// Specifies preferred types for floating point numbers and integers.
///
/// This is used by [`MemSink`][super::MemSink] to signal preferred types to
/// the source. This is used in situations where the source does not have a
/// specific type but has a choice. If, for example, the source reads an ASCII
/// file in which positions are specified in standard `3.14` notation, it's not
/// immediately clear how the source should parse those numbers: parsing as
/// `f32` could loose precision; parsing as `f64` could be useless overhead if
/// the sink converts it back to `f32`. Similarly, if the source generates
/// values (e.g. a shape description), the same is true: it would be great if
/// the source would know the preferred type.
///
/// This trait is only implemented by [`WishFor<F, I>`][WishFor] where `F` is
/// the float type and `I` is the integer type. I don't think implementing this
/// trait for your own types makes any sense. The default for most things is
/// `WishFor<f32, i32>`, see [`DefaultTypeWishes`].
pub trait TypeWish {
    /// The specific type that should be used, if floating point numbers are
    /// available.
    type Float: PrimitiveFloat + Primitive;

    /// The specific type that should be used, if integers are available.
    type Integer: Primitive;
}

/// The default type wish: `f32` as float type, `i32` as integer type.
pub type DefaultTypeWishes = WishFor<f32, i32>;

/// Implements [`TypeWish`] with the float type `F` and the integer type `I`.
///
/// This type is only used at the type level and cannot be created nor used at
/// runtime.
#[allow(unreachable_code)] // TODO: see #38885
#[derive(Debug)]
pub struct WishFor<F: PrimitiveFloat + Primitive, I: Primitive>(!, PhantomData<F>, PhantomData<I>);

impl<F: PrimitiveFloat + Primitive, I: Primitive> TypeWish for WishFor<F, I> {
    type Float = F;
    type Integer = I;
}

/// Type to only overwrite the wish for the float type and keep the default
/// integer type.
pub type WishFloat<F> = WishFor<F, <DefaultTypeWishes as TypeWish>::Integer>;

/// Type to only overwrite the wish for the integer type and keep the default
/// float type.
pub type WishInteger<I> = WishFor<<DefaultTypeWishes as TypeWish>::Float, I>;


/// A type wish which overwrites either `Float` or `Integer` (depending on `P`)
/// and keeps the other type as default.
///
/// If `P` is an integer, the `Integer` type is overwritten, otherwise the
/// `Float` type is overwritten.
#[allow(unreachable_code)] // TODO: see #38885
#[derive(Debug)]
pub struct OverwriteFor<P: Primitive>(!, PhantomData<P>);

macro_rules! impl_overwrite {
    ($($ty:ident => ($f:ty, $i:ty),)*) => {
        $(
            impl TypeWish for OverwriteFor<$ty> {
                type Float = $f;
                type Integer = $i;
            }
        )*
    }
}

impl_overwrite!(
    u8 => (<DefaultTypeWishes as TypeWish>::Float, u8),
    i8 => (<DefaultTypeWishes as TypeWish>::Float, i8),
    u16 => (<DefaultTypeWishes as TypeWish>::Float, u16),
    i16 => (<DefaultTypeWishes as TypeWish>::Float, i16),
    u32 => (<DefaultTypeWishes as TypeWish>::Float, u32),
    i32 => (<DefaultTypeWishes as TypeWish>::Float, i32),
    f32 => (f32, <DefaultTypeWishes as TypeWish>::Integer),
    f64 => (f64, <DefaultTypeWishes as TypeWish>::Integer),
);


/// The result of inspecting the start of the file to check if it's a file of a
/// specific format.
///
/// This is returned by the `is_file_start` functions in each file format
/// module.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IsFormat {
    /// The file is very likely a file of the specified format.
    ///
    /// This should be returned when there are strong indicators of the
    /// specified format (e.g. the magic number is found). The `is_file_start`
    /// function is not required to already try parsing the file and properly
    /// check for errors. Instead, quick and easy indicators should be used.
    Probably,

    /// The file could be a file of the specified format, but there is no clear
    /// indicator that it is.
    ///
    /// This should be returned as rarely as possible. It's only necessary when
    /// a file format does not have a magic number or something like that.
    Maybe,

    /// The file is definitely not valid in the specified format (e.g. a magic
    /// number is not found).
    No,
}
