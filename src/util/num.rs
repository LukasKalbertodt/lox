//! Abstractions over numerical types and vector-like types.


use num_traits::{Float, FloatConst, NumAssign, NumCast};

use crate::{
    cast::{self, CastFrom, CastInto},
};


/// Primitive numerical types, like `f64` and `u32`.
///
/// This trait is automatically implemented for all types that satisfy the
/// super-trait constraints.
pub trait PrimitiveNum:
    'static + lina::Scalar + PartialOrd + NumAssign + NumCast + PrimitiveCast
{}

impl<T> PrimitiveNum for T
where
    T: 'static + lina::Scalar + PartialOrd + NumAssign + NumCast + PrimitiveCast,
{}

/// Primitive floating point types: `f32` and `f64`.
///
/// This trait is automatically implemented for all types that satisfy the
/// super-trait constraints.
pub trait PrimitiveFloat:
    PrimitiveNum
        + Float
        + FloatConst
        + lina::Float
        + CastFrom<f32, Fidelity = cast::Lossless>
        + CastFrom<u8, Fidelity = cast::Lossless>
        + CastFrom<i8, Fidelity = cast::Lossless>
        + CastFrom<u16, Fidelity = cast::Lossless>
        + CastFrom<i16, Fidelity = cast::Lossless>
{
    /// Creates `Self` from the given `f32`. Uses `cast::lossless` internally.
    fn from_f32(v: f32) -> Self {
        cast::lossless(v)
    }
}

impl<T> PrimitiveFloat for T
where
    T: PrimitiveNum
        + Float
        + FloatConst
        + lina::Float
        + CastFrom<f32, Fidelity = cast::Lossless>
        + CastFrom<u8, Fidelity = cast::Lossless>
        + CastFrom<i8, Fidelity = cast::Lossless>
        + CastFrom<u16, Fidelity = cast::Lossless>
        + CastFrom<i16, Fidelity = cast::Lossless>
{}


/// Types that can be casted from and into all primitive types.
///
/// This is basically a trait-bound alias that is automatically implemented for
/// all types that satisfy the supertrait bounds.
pub trait PrimitiveCast: CastFromPrimitive + CastIntoPrimitive {}

impl<T> PrimitiveCast for T where T: CastFromPrimitive + CastIntoPrimitive {}

/// Types that can be cast from all primitive types.
pub trait CastFromPrimitive:
    CastFrom<u8> + CastFrom<u16> + CastFrom<u32> + CastFrom<u64> + CastFrom<u128>
        + CastFrom<i8> + CastFrom<i16> + CastFrom<i32> + CastFrom<i64> + CastFrom<i128>
        + CastFrom<f32> + CastFrom<f64>
{}

impl<T> CastFromPrimitive for T
where
    T: CastFrom<u8> + CastFrom<u16> + CastFrom<u32> + CastFrom<u64> + CastFrom<u128>
        + CastFrom<i8> + CastFrom<i16> + CastFrom<i32> + CastFrom<i64> + CastFrom<i128>
        + CastFrom<f32> + CastFrom<f64>
{}

/// Types that can be cast into all primitive types.
pub trait CastIntoPrimitive:
    CastInto<u8> + CastInto<u16> + CastInto<u32> + CastInto<u64> + CastInto<u128>
        + CastInto<i8> + CastInto<i16> + CastInto<i32> + CastInto<i64> + CastInto<i128>
        + CastInto<f32> + CastInto<f64>
{}

impl<T> CastIntoPrimitive for T
where
    T: CastInto<u8> + CastInto<u16> + CastInto<u32> + CastInto<u64> + CastInto<u128>
        + CastInto<i8> + CastInto<i16> + CastInto<i32> + CastInto<i64> + CastInto<i128>
        + CastInto<f32> + CastInto<f64>
{}
