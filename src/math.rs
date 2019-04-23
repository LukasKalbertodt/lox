//! Abstractions over numerical types and vector-like types.


use std::{
    fmt::Debug,
};

use cgmath::BaseFloat;
use num_traits::{Float, FloatConst, Num, NumAssign, NumCast};

use crate::{
    cast::{self, CastFromIntegers, LosslessCastFrom, PrimitiveCast},
};


/// Primitive numerical types, like `f64` and `u32`.
///
/// This trait is automatically implemented for all types that satisfy the
/// super-trait constraints.
///
/// Note that this is very similar to `cgmath::BaseNum`. Right now, the only
/// difference is the additional `'static` bound and the `PrimitiveCast` bound.
pub trait PrimitiveNum:
    'static + Copy + Debug + Num + PartialOrd + NumAssign + NumCast + PrimitiveCast<cast::Lossy>
{}

impl<T> PrimitiveNum for T
where
    T: 'static
        + Copy
        + Debug
        + Num
        + PartialOrd
        + NumAssign
        + NumCast
        + PrimitiveCast<cast::Lossy>,
{}

/// Primitive floating point types: `f32` and `f64`.
///
/// This trait is automatically implemented for all types that satisfy the
/// super-trait constraints.
pub trait PrimitiveFloat:
    PrimitiveNum
    + Float
    + FloatConst
    + BaseFloat
    + LosslessCastFrom<f32>
    + CastFromIntegers<cast::AllowRounding>
{}

impl<T> PrimitiveFloat for T
where
    T: PrimitiveNum
        + Float
        + FloatConst
        + BaseFloat
        + LosslessCastFrom<f32>
        + CastFromIntegers<cast::AllowRounding>,
{}
