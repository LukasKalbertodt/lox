//! Casting between different number types.
//!
//! This module offers functions and traits for casting between numerical
//! types. The goal is to allow the user to choose how exactly numbers are
//! casted, e.g. whether rounding is allowed or not.
//!
//! A notable difference to similar solutions is that this module never looks
//! at the actual concrete number value to decide whether or not casting is
//! possible (of course, the value is used when performing the actual cast).
//! Instead, it is decided purely on the compile-time type. This has the
//! obvious disadvantage of being more restrictive (`u16` -> `u8` cast is not
//! allowed via `lossless`, even if your `u16` values never exceed 255). The
//! important advantage is speed: since everything can be decided at compile
//! time, the decision has no overhead. Furthermore, you can get compile errors
//! by using trait bounds from this module.
//!
//!
//! # Cast Rigors
//!
//! A cast rigor describes how much we "allow" when casting. We decide between
//! two things that one might want to avoid: clamping and rounding. The former
//! describes the process of changing a number to fit in a smaller range (e.g.
//! `500u16` to `u8`). "Rounding" means that the input number is not outside
//! the range of the destination type, but can not be exactly represented; a
//! reasonable close number of the destiniation type is choosen (e.g. float to
//! int).
//!
//! Two binary choices lead to four different rigors:
//! - [`Lossless`]: neither clamping nor rounding allowed.
//! - [`AllowClamping`]: clamping allowed, rounding not allowed.
//! - [`AllowRounding`]: rounding allowed, clamping not allowed.
//! - [`Lossy`]: both, clamping and rounding, allowed.
//!
//! In the following table you can see what can happen during the conversions
//! between primitive number types. Here, '×' stands for "clamping", '○' stands
//! for "rounding" and '⊗' stands for both. Empty cells mean that this cast is
//! always lossless.
//!
//! | ↱          |`u8`|`u16`|`u32`|`u64`|`u128`|`i8`|`i16`|`i32`|`i64`|`i128`|`f32`|`f64`|
//! | -          |----|-----|-----|-----|------|----|-----|-----|-----|------|-----|-----|
//! | **`u8`**   |    |     |     |     |      |  × |     |     |     |      |     |     |
//! | **`u16`**  |  × |     |     |     |      |  × |   × |     |     |      |     |     |
//! | **`u32`**  |  × |   × |     |     |      |  × |   × |   × |     |      |   ○ |     |
//! | **`u64`**  |  × |   × |   × |     |      |  × |   × |   × |   × |      |   ○ |   ○ |
//! | **`u128`** |  × |   × |   × |   × |      |  × |   × |   × |   × |    × |   ○ |   ○ |
//! |            |    |     |     |     |      |    |     |     |     |      |     |     |
//! | **`i8`**   |  × |   × |   × |   × |    × |    |     |     |     |      |     |     |
//! | **`i16`**  |  × |   × |   × |   × |    × |  × |     |     |     |      |     |     |
//! | **`i32`**  |  × |   × |   × |   × |    × |  × |   × |     |     |      |   ○ |     |
//! | **`i64`**  |  × |   × |   × |   × |    × |  × |   × |   × |     |      |   ○ |   ○ |
//! | **`i128`** |  × |   × |   × |   × |    × |  × |   × |   × |   × |      |   ○ |   ○ |
//! |            |    |     |     |     |      |    |     |     |     |      |     |     |
//! | **`f32`**  |  ⊗ |   ⊗ |  ⊗ |   ⊗ |   ⊗ |  ⊗ |   ⊗ |  ⊗ |   ⊗ |   ○  |     |     |
//! | **`f64`**  |  ⊗ |   ⊗ |  ⊗ |   ⊗ |   ⊗ |  ⊗ |   ⊗ |  ⊗ |   ⊗ |   ⊗ |   ⊗ |     |
//!
//!
//! # Casting functions
//!
//! For each cast rigor, there are two corresponding functions called
//! `{{rigor}}` and `try_{{rigor}}`, e.g. [`lossless`] and
//! [`try_lossless`].
//!
//! The functions without `try_` prefix work with classical trait bounds: if
//! you attempt to cast betwene two types that are not castable with the
//! selected rigor, you will get a compile error.
//!
//! The functions *with* `try_` prefix work differently: instead of resulting
//! in a compiler error, `None` is returned. Remember: those functions do *not*
//! look at the value to decide whether `Some` or `None` is returned! This is
//! useful in a couple of situations, but you should first try to use the
//! functions without `try_` prefix as a compiler error is better than a
//! runtime error.
//!
//! Additionally, there are two functions generic over the cast rigor: [`cast`]
//! and [`try_cast`]. You usually don't want to use them manually, but they are
//! useful in generic contexts.
//!
//! If you need to find out if a cast is possible without providing a value,
//! [`is_cast_possible`] is what you are looking for.
//!
//! ## Example
//!
//! ```
//! use lox::cast;
//!
//! // This works without a problem as the cast is always lossless.
//! assert_eq!(cast::lossless::<u8, u16>(27), 27);
//! assert_eq!(cast::try_lossless::<u8, u16>(27), Some(27));
//!
//! // The other way around, we can't always cast without loosing information.
//! // Thus, the first line here would lead to a compiler error. With the
//! // `try_*` version you can get `None` instead of a compiler error.
//! //assert_eq!(cast::lossless::<u16, u8>(27), 27);
//! assert_eq!(cast::try_lossless::<u16, u8>(10), None);
//! assert_eq!(cast::try_lossless::<u16, u8>(300), None);
//!
//! // When we allow clamping, we can cast.
//! assert_eq!(cast::clamping::<u16, u8>(300), 255);
//! assert_eq!(cast::try_clamping::<u16, u8>(300), Some(255));
//! ```
//!
//! # Casting traits
//!
//! The traits are mainly used to implement the functions of this module and
//! usually don't need to be used directly. However, you can use some traits as
//! trait bounds for your own functions.
//!
//! The `CastFrom` trait is generic over the cast rigor. This trait shouldn't
//! be implemented directly. Instead, there are four rigor specific traits that
//! have proper supertrait bounds. `CastFrom` is automatically implemented via
//! blanket implementations for types that implement the rigor specific traits.
//!
//! The traits are implemented for all combination of primitive Rust types
//! (unsigned integers, signed integers and floating point types).
//!
//! The `TryCastFrom` trait is just a helper to implement `try_*` functions, so
//! it's probably not useful to you.
//!
// TODO: should we use the `conv` crate instead?

use crate::{
    sealed::Sealed,
};


// ===========================================================================
// ===== Casting functions
// ===========================================================================

/// Cast `src` from type `Src` to the type `Dst`, with the cast rigor `R`.
///
/// Instead of using this generic function, there is a specific function for
/// each cast rigor that you can use. Usually, that's easier.
#[inline(always)]
pub fn cast<R, Src, Dst>(src: Src) -> Dst
where
    R: CastRigor,
    Src: CastInto<R, Dst>,
{
    src.cast_into()
}

/// Cast `src` from type `Src` to the type `Dst`, without loosing information.
#[inline(always)]
pub fn lossless<Src, Dst>(src: Src) -> Dst
where
    Src: CastInto<Lossless, Dst>,
{
    src.cast_into()
}

/// Cast `src` from type `Src` to the type `Dst`, with clamping being allowed.
#[inline(always)]
pub fn clamping<Src, Dst>(src: Src) -> Dst
where
    Src: CastInto<AllowClamping, Dst>,
{
    src.cast_into()
}

/// Cast `src` from type `Src` to the type `Dst`, with rounding being allowed.
#[inline(always)]
pub fn rounding<Src, Dst>(src: Src) -> Dst
where
    Src: CastInto<AllowRounding, Dst>,
{
    src.cast_into()
}

/// Cast `src` from type `Src` to the type `Dst`, with clamping and rounding
/// being allowed.
#[inline(always)]
pub fn lossy<Src, Dst>(src: Src) -> Dst
where
    Src: CastInto<Lossy, Dst>,
{
    src.cast_into()
}

/// Determines whether casting from `Src` to `Dst` with the specific cast rigor
/// is possible.
///
/// If this function returns `false`, `try_from` will always return `None` for
/// the same type arguments. Similarly, it will always return `Some` if this
/// function returns `true`.
#[inline(always)]
pub fn is_cast_possible<R, Src, Dst>() -> bool
where
    R: CastRigor,
    Dst: TryCastFrom<R, Src>,
{
    Dst::CAST_POSSIBLE
}

/// Cast `src` from type `Src` to the type `Dst`, with the cast rigor `R`, or
/// return `None` if the types cannot be casted with the specified rigor.
///
/// This is like [`cast`] but returns `None` if the two types cannot be casted
/// with the specified rigor. Note that the decision whether the types can be
/// casted only depends on the types and not the value. Thus, whether `Some` or
/// `None` is returned is known at compile time.
#[inline(always)]
pub fn try_cast<R, Src, Dst>(src: Src) -> Option<Dst>
where
    R: CastRigor,
    Dst: TryCastFrom<R, Src>,
{
    Dst::try_cast_from(src)
}

/// [`try_cast`] with `NoCast` rigor. See that documentation for more info.
/// This function is pretty useless in non-generic situations.
#[inline(always)]
pub fn try_no_cast<Src, Dst>(src: Src) -> Option<Dst>
where
    Dst: TryCastFrom<NoCast, Src>,
{
    Dst::try_cast_from(src)
}

/// [`try_cast`] with `Lossless` rigor. See that documentation for more info.
#[inline(always)]
pub fn try_lossless<Src, Dst>(src: Src) -> Option<Dst>
where
    Dst: TryCastFrom<Lossless, Src>,
{
    Dst::try_cast_from(src)
}

/// [`try_cast`] with `AllowClamping` rigor. See that documentation for more
/// info.
#[inline(always)]
pub fn try_clamping<Src, Dst>(src: Src) -> Option<Dst>
where
    Dst: TryCastFrom<AllowClamping, Src>,
{
    Dst::try_cast_from(src)
}

/// [`try_cast`] with `AllowRounding` rigor. See that documentation for more
/// info.
#[inline(always)]
pub fn try_rounding<Src, Dst>(src: Src) -> Option<Dst>
where
    Dst: TryCastFrom<AllowRounding, Src>,
{
    Dst::try_cast_from(src)
}

/// [`try_cast`] with `Lossy` rigor. See that documentation for more info.
#[inline(always)]
pub fn try_lossy<Src, Dst>(src: Src) -> Option<Dst>
where
    Dst: TryCastFrom<Lossy, Src>,
{
    Dst::try_cast_from(src)
}

// ===========================================================================
// ===== Casting rigors
// ===========================================================================

/// Describes how rigorous a cast shall be. See module level documentation for
/// more information.
///
/// This trait is only implemented for the four different rigors defined in
/// this module and cannot be implemented for own types.
pub trait CastRigor: Sealed {}

/// Cast rigor: no casting allowed at all.
///
/// This is purely used at type level and it's impossible to construct.
#[derive(Debug)]
pub enum NoCast {}
impl Sealed for NoCast {}
impl CastRigor for NoCast {}

/// Cast rigor: neither clamping nor rounding is allowed.
///
/// This is purely used at type level and it's impossible to construct.
#[derive(Debug)]
pub enum Lossless {}
impl Sealed for Lossless {}
impl CastRigor for Lossless {}

/// Cast rigor: clamping allowed, rounding not allowed.
///
/// This is purely used at type level and it's impossible to construct.
#[derive(Debug)]
pub enum AllowClamping {}
impl Sealed for AllowClamping {}
impl CastRigor for AllowClamping {}

/// Cast rigor: rounding allowed, clamping not allowed.
///
/// This is purely used at type level and it's impossible to construct.
#[derive(Debug)]
pub enum AllowRounding {}
impl Sealed for AllowRounding {}
impl CastRigor for AllowRounding {}

/// Cast rigor: both, clamping and rounding, allowed.
///
/// This is purely used at type level and it's impossible to construct.
#[derive(Debug)]
pub enum Lossy {}
impl Sealed for Lossy {}
impl CastRigor for Lossy {}


// ===========================================================================
// ===== Casting traits
// ===========================================================================

/// Ability to be casted from the type `Src` with the given cast rigor.
///
/// You shouldn't implement this trait directly (in fact, you can't) but rather
/// implement one or multiple of [`LosslessCastFrom`], [`ClampingCastFrom`],
/// [`RoundingCastFrom`] and [`LossyCastFrom`]. Implementing those will also
/// implement this trait thanks to several blanket impls.
pub trait CastFrom<R: CastRigor, Src> {
    fn cast_from(src: Src) -> Self;
}

/// Types that can always be casted from `Src` without loosing any information.
/// See [`Lossless`] for more information.
pub trait LosslessCastFrom<Src>: ClampingCastFrom<Src> + RoundingCastFrom<Src> {
    fn lossless_cast_from(src: Src) -> Self;
}

/// Types that can always be casted from `Src` if clamping values is allowed.
/// See [`AllowClamping`] for more information.
pub trait ClampingCastFrom<Src>: LossyCastFrom<Src> {
    fn clamping_cast_from(src: Src) -> Self;
}

/// Types that can always be casted from `Src` if rounding values is allowed.
/// See [`AllowRounding`] for more information.
pub trait RoundingCastFrom<Src>: LossyCastFrom<Src> {
    fn rounding_cast_from(src: Src) -> Self;
}

/// Types that can always be casted from `Src` if clamping and rounding values
/// is allowed. See [`Lossy`] for more information.
pub trait LossyCastFrom<Src> {
    fn lossy_cast_from(src: Src) -> Self;
}

// Here we implement `CastFrom` with specific rigors for all types that
// implement the trait specific to that rigor.
impl<Src, Dst> CastFrom<Lossless, Src> for Dst
where
    Dst: LosslessCastFrom<Src>,
{
    fn cast_from(src: Src) -> Self {
        Dst::lossless_cast_from(src)
    }
}

impl<Src, Dst> CastFrom<AllowClamping, Src> for Dst
where
    Dst: ClampingCastFrom<Src>,
{
    fn cast_from(src: Src) -> Self {
        Dst::clamping_cast_from(src)
    }
}

impl<Src, Dst> CastFrom<AllowRounding, Src> for Dst
where
    Dst: RoundingCastFrom<Src>,
{
    fn cast_from(src: Src) -> Self {
        Dst::rounding_cast_from(src)
    }
}

impl<Src, Dst> CastFrom<Lossy, Src> for Dst
where
    Dst: LossyCastFrom<Src>,
{
    fn cast_from(src: Src) -> Self {
        Dst::lossy_cast_from(src)
    }
}

impl<T> CastFrom<NoCast, T> for T {
    fn cast_from(src: T) -> Self {
        src
    }
}

/// Helper trait for `try_*` functions.
///
/// This trait is implemented for all types and this default implementation
/// always returns `None`. For all rigor, source and destination type
/// combinations for which `CastFrom` is implemented, this default
/// implementation is overwritten to return `Some` with the casted value.
pub trait TryCastFrom<R: CastRigor, Src>: Sized {
    const CAST_POSSIBLE: bool;
    fn try_cast_from(src: Src) -> Option<Self>;
}

impl<R: CastRigor, Src, Dst> TryCastFrom<R, Src> for Dst {
    default const CAST_POSSIBLE: bool = false;
    default fn try_cast_from(_: Src) -> Option<Self> {
        None
    }
}

impl<R: CastRigor, Src, Dst> TryCastFrom<R, Src> for Dst
where
    Dst: CastFrom<R, Src>,
{
    const CAST_POSSIBLE: bool = true;
    fn try_cast_from(src: Src) -> Option<Self> {
        Some(Dst::cast_from(src))
    }
}

/// A helper trait that works in the opposite direction than `CastFrom`.
///
/// There is a blanket implementation that implements this for all types for
/// which `CastFrom` is implemented. So you usually don't have to implement
/// this trait directly.
pub trait CastInto<R: CastRigor, Dst> {
    fn cast_into(self) -> Dst;
}

impl<R, Src, Dst> CastInto<R, Dst> for Src
where
    R: CastRigor,
    Dst: CastFrom<R, Src>,
{
    fn cast_into(self) -> Dst {
        Dst::cast_from(self)
    }
}

// ===========================================================================
// ===== Bound aliases
// ===========================================================================
/// Convenience trait implemented for all types that can be cast from all
/// primitive integer types with the rigor `R`.
pub trait CastFromIntegers<R: CastRigor>:
    CastFrom<R, u8>
    + CastFrom<R, i8>
    + CastFrom<R, u16>
    + CastFrom<R, i16>
    + CastFrom<R, u32>
    + CastFrom<R, i32>
    + CastFrom<R, u64>
    + CastFrom<R, i64>
    + CastFrom<R, u128>
    + CastFrom<R, i128>
{}

impl<T, R: CastRigor> CastFromIntegers<R> for T
where
    T: CastFrom<R, u8>
        + CastFrom<R, i8>
        + CastFrom<R, u16>
        + CastFrom<R, i16>
        + CastFrom<R, u32>
        + CastFrom<R, i32>
        + CastFrom<R, u64>
        + CastFrom<R, i64>
        + CastFrom<R, u128>
        + CastFrom<R, i128>
{}

/// Convenience trait implemented for all types that can be cast into all
/// primitive integer types with the rigor `R`.
pub trait CastIntoIntegers<R: CastRigor>:
    CastInto<R, u8>
    + CastInto<R, i8>
    + CastInto<R, u16>
    + CastInto<R, i16>
    + CastInto<R, u32>
    + CastInto<R, i32>
    + CastInto<R, u64>
    + CastInto<R, i64>
    + CastInto<R, u128>
    + CastInto<R, i128>
{}

impl<T, R: CastRigor> CastIntoIntegers<R> for T
where
    T: CastInto<R, u8>
        + CastInto<R, i8>
        + CastInto<R, u16>
        + CastInto<R, i16>
        + CastInto<R, u32>
        + CastInto<R, i32>
        + CastInto<R, u64>
        + CastInto<R, i64>
        + CastInto<R, u128>
        + CastInto<R, i128>
{}


/// Convenience trait implemented for all types that can be cast from all
/// primitive float types (`f32` and `f64`) with the rigor `R`.
pub trait CastFromFloats<R: CastRigor>: CastFrom<R, f32> + CastFrom<R, f64> {}

impl<T, R: CastRigor> CastFromFloats<R> for T
where
    T: CastFrom<R, f32> + CastFrom<R, f64>,
{}

/// Convenience trait implemented for all types that can be cast into all
/// primitive float types (`f32` and `f64`) with the rigor `R`.
pub trait CastIntoFloats<R: CastRigor>: CastInto<R, f32> + CastInto<R, f64> {}

impl<T, R: CastRigor> CastIntoFloats<R> for T
where
    T: CastInto<R, f32> + CastInto<R, f64>,
{}


/// Types that can be casted from and into all primitive types with rigor `R`.
///
/// This is basically a trait-bound alias that is automatically implemented for
/// all types that satisfy the supertrait bounds.
pub trait PrimitiveCast<R: CastRigor>:
    CastFromIntegers<R>
    + CastFromFloats<R>
    + CastIntoIntegers<R>
    + CastIntoFloats<R>
{}

impl<T, R: CastRigor> PrimitiveCast<R> for T
where
    T: CastFromIntegers<R> + CastFromFloats<R> + CastIntoIntegers<R> + CastIntoFloats<R>,
{}

// ===========================================================================
// ===== Implementations for primitive types
// ===========================================================================

// ----- Lossless ------------------------------------------------------------
macro_rules! impl_lossless {
    ($($src:ty => $dst:ty ;)*) => {
        $(
            impl LosslessCastFrom<$src> for $dst {
                #[inline(always)]
                fn lossless_cast_from(src: $src) -> Self {
                    src.into()
                }
            }

            impl RoundingCastFrom<$src> for $dst {
                #[inline(always)]
                fn rounding_cast_from(src: $src) -> Self {
                    src.into()
                }
            }

            impl ClampingCastFrom<$src> for $dst {
                #[inline(always)]
                fn clamping_cast_from(src: $src) -> Self {
                    src.into()
                }
            }

            impl LossyCastFrom<$src> for $dst {
                #[inline(always)]
                fn lossy_cast_from(src: $src) -> Self {
                    src.into()
                }
            }
        )*
    }
}

impl_lossless!(
    // Unsigned to unsigned
    u8 => u8;
    u8 => u16;
    u8 => u32;
    u8 => u64;
    u8 => u128;

    u16 => u16;
    u16 => u32;
    u16 => u64;
    u16 => u128;

    u32 => u32;
    u32 => u64;
    u32 => u128;

    u64 => u64;
    u64 => u128;

    u128 => u128;

    // Unsigned to signed
    u8 => i16;
    u8 => i32;
    u8 => i64;
    u8 => i128;

    u16 => i32;
    u16 => i64;
    u16 => i128;

    u32 => i64;
    u32 => i128;

    u64 => i128;

    // Signed to signed
    i8 => i8;
    i8 => i16;
    i8 => i32;
    i8 => i64;
    i8 => i128;

    i16 => i16;
    i16 => i32;
    i16 => i64;
    i16 => i128;

    i32 => i32;
    i32 => i64;
    i32 => i128;

    i64 => i64;
    i64 => i128;

    i128 => i128;

    // Unsigned to float
    u8 => f32;
    u8 => f64;

    u16 => f32;
    u16 => f64;

    u32 => f64;

    // Signed to float
    i8 => f32;
    i8 => f64;

    i16 => f32;
    i16 => f64;

    i32 => f64;

    // Float to float
    f32 => f32;
    f32 => f64;
    f64 => f64;
);


// ----- Clamping ------------------------------------------------------------
macro_rules! impl_clamping {
    ($($cast_method:ident: $src:ident => $dst:ident ;)*) => {
        $(
            impl ClampingCastFrom<$src> for $dst {
                fn clamping_cast_from(src: $src) -> Self {
                    impl_clamping!(@do_cast $cast_method: src, $src => $dst)
                }
            }

            impl LossyCastFrom<$src> for $dst {
                fn lossy_cast_from(src: $src) -> Self {
                    <$dst as ClampingCastFrom<$src>>::clamping_cast_from(src)
                }
            }
        )*
    };
    // `$src::max` > `$dst::max` and `$src::min` >= `$dst::min`.
    (@do_cast top: $v:expr, $src:ident => $dst:ident) => {
        if $v > $dst::max_value() as $src {
            $dst::max_value()
        } else {
            $v as $dst
        }
    };
    // `$src::max` <= `$dst::max` and `$src::min` < `$dst::min`.
    (@do_cast neg: $v:expr, $src:ident => $dst:ident) => {
        if $v < $dst::min_value() as $src {
            $dst::min_value()
        } else {
            $v as $dst
        }
    };
    // `$src::max` <= `$dst::max` and `$src::min` >= `$dst::min`.
    (@do_cast both: $v:expr, $src:ident => $dst:ident) => {
        if $v > $dst::max_value() as $src {
            $dst::max_value()
        } else if $v < $dst::min_value() as $src {
            $dst::min_value()
        } else {
            $v as $dst
        }
    };
}

impl_clamping!(
    // Unsigned to unsigned
    top: u16 => u8;

    top: u32 => u8;
    top: u32 => u16;

    top: u64 => u8;
    top: u64 => u16;
    top: u64 => u32;

    top: u128 => u8;
    top: u128 => u16;
    top: u128 => u32;
    top: u128 => u64;

    // Unsigned to signed
    top: u8 => i8;

    top: u16 => i8;
    top: u16 => i16;

    top: u32 => i8;
    top: u32 => i16;
    top: u32 => i32;

    top: u64 => i8;
    top: u64 => i16;
    top: u64 => i32;
    top: u64 => i64;

    top: u128 => i8;
    top: u128 => i16;
    top: u128 => i32;
    top: u128 => i64;
    top: u128 => i128;

    // Signed to unsigned
    neg: i8 => u8;
    neg: i8 => u16;
    neg: i8 => u32;
    neg: i8 => u64;
    neg: i8 => u128;

    both: i16 => u8;
    neg: i16 => u16;
    neg: i16 => u32;
    neg: i16 => u64;
    neg: i16 => u128;

    both: i32 => u8;
    both: i32 => u16;
    neg: i32 => u32;
    neg: i32 => u64;
    neg: i32 => u128;

    both: i64 => u8;
    both: i64 => u16;
    both: i64 => u32;
    neg: i64 => u64;
    neg: i64 => u128;

    both: i128 => u8;
    both: i128 => u16;
    both: i128 => u32;
    both: i128 => u64;
    neg: i128 => u128;

    // Signed to signed
    both: i16 => i8;

    both: i32 => i8;
    both: i32 => i16;

    both: i64 => i8;
    both: i64 => i16;
    both: i64 => i32;

    both: i128 => i8;
    both: i128 => i16;
    both: i128 => i32;
    both: i128 => i64;
);


// ----- Rounding ------------------------------------------------------------
macro_rules! impl_rounding {
    ($($src:ty => $dst:ty ;)*) => {
        $(
            impl RoundingCastFrom<$src> for $dst {
                #[inline(always)]
                fn rounding_cast_from(src: $src) -> Self {
                    src as $dst
                }
            }

            impl LossyCastFrom<$src> for $dst {
                #[inline(always)]
                fn lossy_cast_from(src: $src) -> Self {
                    <$dst as RoundingCastFrom<$src>>::rounding_cast_from(src)
                }
            }
        )*
    }
}

impl_rounding!(
    // Unsigned to float
    u32 => f32;

    u64 => f32;
    u64 => f64;

    u128 => f32;
    u128 => f64;

    // Signed to float
    i32 => f32;

    i64 => f32;
    i64 => f64;

    i128 => f32;
    i128 => f64;

    // Float to signed
    f32 => i128;
);


// ----- Lossy ---------------------------------------------------------------
impl LossyCastFrom<f64> for f32 {
    #[inline(always)]
    fn lossy_cast_from(src: f64) -> Self {
        // This is safe. See https://github.com/rust-lang/rust/issues/15536
        src as f32
    }
}

macro_rules! impl_lossy_float_to_int {
    ($($src:ident => $dst:ident ;)*) => {
        $(
            impl LossyCastFrom<$src> for $dst {
                fn lossy_cast_from(src: $src) -> Self {
                    // We just have to take care of saturating here. If it's in
                    // range, we can use `as` to round.
                    //
                    // TODO: Maybe optimize this?
                    // TODO: Replace with `as` once it's not UB anymore. See
                    //       https://github.com/rust-lang/rust/issues/10184
                    if src > std::$dst::MAX as $src {
                        std::$dst::MAX
                    } else if src < std::$dst::MIN as $src {
                        std::$dst::MIN
                    } else {
                        src as $dst
                    }
                }
            }
        )*
    }
}

impl_lossy_float_to_int!(
    f32 => u8;
    f32 => u16;
    f32 => u32;
    f32 => u64;
    f32 => u128;
    f32 => i8;
    f32 => i16;
    f32 => i32;
    f32 => i64;

    f64 => u8;
    f64 => u16;
    f64 => u32;
    f64 => u64;
    f64 => u128;
    f64 => i8;
    f64 => i16;
    f64 => i32;
    f64 => i64;
    f64 => i128;
);


// ===========================================================================
// ===== Test
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // This test suite uses a lot of macros to avoid duplicate code. But just
    // using macros is a really bad idea because it increases compile times a
    // lot. That's why we define real functions, too, which are then called by
    // the macro.
    macro_rules! gen_fn {
        ($assert_name:ident, $fn_name:ident) => {
            #[inline(never)]
            fn $assert_name<R, SrcT, DstT>(
                src: SrcT,
                dst: DstT,
                should_succeed: bool,
                rigor_str: &str,
                src_str: &str,
                dst_str: &str,
            )
            where
                R: CastRigor,
                SrcT: Copy,
                DstT: PartialEq + Copy,
            {
                let (expected_val, expected_str, actual_str) = if should_succeed {
                    (Some(dst), "succeed", "failed")
                } else {
                    (None, "fail", "succeeded")
                };


                if $fn_name::<SrcT, DstT>(src) != expected_val {
                    panic!(
                        "expected {} -> {} `{}` cast to {}, but it {}",
                        src_str,
                        dst_str,
                        stringify!($fn_name),
                        expected_str,
                        actual_str,
                    );
                }

                // Test generic `try_cast`
                if try_cast::<R, SrcT, DstT>(src) != expected_val {
                    panic!(
                        "expected {} -> {} `try_cast` to {}, but it {}",
                        src_str,
                        dst_str,
                        expected_str,
                        actual_str,
                    );
                }

                // Test `is_cast_possible`
                if is_cast_possible::<R, SrcT, DstT>() != should_succeed {
                    panic!(
                        "expected `is_cast_possible<{}, {}, {}>()` to be `{}`, but \
                            it returned `{}`",
                        rigor_str,
                        src_str,
                        dst_str,
                        should_succeed,
                        !should_succeed,
                    );
                }
            }
        }
    }

    gen_fn!(assert_try_no_cast, try_no_cast);
    gen_fn!(assert_try_lossless, try_lossless);
    gen_fn!(assert_try_clamping, try_clamping);
    gen_fn!(assert_try_rounding, try_rounding);
    gen_fn!(assert_try_lossy, try_lossy);

    macro_rules! util {
        (@assert_opt $fun:ident, $rigor:ident: $src:ident as $dst:ident => $outcome:tt) => {
            util!(@inner $fun ::<$rigor, $src, $dst>(
                util!(@lit $src),
                util!(@lit $dst),
                util!(@to_bool $outcome),
                stringify!($rigor),
                stringify!($src),
                stringify!($dst),
            ))
        };
        (@inner no_cast $($t:tt)*) => { assert_try_no_cast $($t)* };
        (@inner lossless $($t:tt)*) => { assert_try_lossless $($t)* };
        (@inner clamping $($t:tt)*) => { assert_try_clamping $($t)* };
        (@inner rounding $($t:tt)*) => { assert_try_rounding $($t)* };
        (@inner lossy $($t:tt)*) => { assert_try_lossy $($t)* };
        (@to_bool n) => { false };
        (@to_bool y) => { true };
        (@lit f32) => { 3.0 };
        (@lit f64) => { 3.0 };
        (@lit $integer_ty:ident) => { 3 };
    }

    macro_rules! test {
        (
            $fun:ident, $rigor:ident: $ty:ident =>
            $u8:tt $u16:tt $u32:tt $u64:tt $u128:tt
            $i8:tt $i16:tt $i32:tt $i64:tt $i128:tt
            $f32:tt $f64:tt
        ) => {{
            util!(@assert_opt $fun, $rigor: $ty as u8 => $u8);
            util!(@assert_opt $fun, $rigor: $ty as u16 => $u16);
            util!(@assert_opt $fun, $rigor: $ty as u32 => $u32);
            util!(@assert_opt $fun, $rigor: $ty as u64 => $u64);
            util!(@assert_opt $fun, $rigor: $ty as u128 => $u128);

            util!(@assert_opt $fun, $rigor: $ty as i8 => $i8);
            util!(@assert_opt $fun, $rigor: $ty as i16 => $i16);
            util!(@assert_opt $fun, $rigor: $ty as i32 => $i32);
            util!(@assert_opt $fun, $rigor: $ty as i64 => $i64);
            util!(@assert_opt $fun, $rigor: $ty as i128 => $i128);

            util!(@assert_opt $fun, $rigor: $ty as f32 => $f32);
            util!(@assert_opt $fun, $rigor: $ty as f64 => $f64);
        }};
    }

    #[test]
    fn cast_try_no_cast() {
        //                             u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64
        test!(no_cast, NoCast: u8   => y  n   n   n   n    n  n   n   n   n    n   n);
        test!(no_cast, NoCast: u16  => n  y   n   n   n    n  n   n   n   n    n   n);
        test!(no_cast, NoCast: u32  => n  n   y   n   n    n  n   n   n   n    n   n);
        test!(no_cast, NoCast: u64  => n  n   n   y   n    n  n   n   n   n    n   n);
        test!(no_cast, NoCast: u128 => n  n   n   n   y    n  n   n   n   n    n   n);

        test!(no_cast, NoCast: i8   => n  n   n   n   n    y  n   n   n   n    n   n);
        test!(no_cast, NoCast: i16  => n  n   n   n   n    n  y   n   n   n    n   n);
        test!(no_cast, NoCast: i32  => n  n   n   n   n    n  n   y   n   n    n   n);
        test!(no_cast, NoCast: i64  => n  n   n   n   n    n  n   n   y   n    n   n);
        test!(no_cast, NoCast: i128 => n  n   n   n   n    n  n   n   n   y    n   n);

        test!(no_cast, NoCast: f32  => n  n   n   n   n    n  n   n   n   n    y   n);
        test!(no_cast, NoCast: f64  => n  n   n   n   n    n  n   n   n   n    n   y);
    }

    #[test]
    fn cast_try_lossless() {
        //                                u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64
        test!(lossless, Lossless: u8   => y  y   y   y   y    n  y   y   y   y    y   y);
        test!(lossless, Lossless: u16  => n  y   y   y   y    n  n   y   y   y    y   y);
        test!(lossless, Lossless: u32  => n  n   y   y   y    n  n   n   y   y    n   y);
        test!(lossless, Lossless: u64  => n  n   n   y   y    n  n   n   n   y    n   n);
        test!(lossless, Lossless: u128 => n  n   n   n   y    n  n   n   n   n    n   n);

        test!(lossless, Lossless: i8   => n  n   n   n   n    y  y   y   y   y    y   y);
        test!(lossless, Lossless: i16  => n  n   n   n   n    n  y   y   y   y    y   y);
        test!(lossless, Lossless: i32  => n  n   n   n   n    n  n   y   y   y    n   y);
        test!(lossless, Lossless: i64  => n  n   n   n   n    n  n   n   y   y    n   n);
        test!(lossless, Lossless: i128 => n  n   n   n   n    n  n   n   n   y    n   n);

        test!(lossless, Lossless: f32  => n  n   n   n   n    n  n   n   n   n    y   y);
        test!(lossless, Lossless: f64  => n  n   n   n   n    n  n   n   n   n    n   y);
    }

    #[test]
    fn cast_try_clamping() {
        //                                     u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64
        test!(clamping, AllowClamping: u8   => y  y   y   y   y    y  y   y   y   y    y   y);
        test!(clamping, AllowClamping: u16  => y  y   y   y   y    y  y   y   y   y    y   y);
        test!(clamping, AllowClamping: u32  => y  y   y   y   y    y  y   y   y   y    n   y);
        test!(clamping, AllowClamping: u64  => y  y   y   y   y    y  y   y   y   y    n   n);
        test!(clamping, AllowClamping: u128 => y  y   y   y   y    y  y   y   y   y    n   n);

        test!(clamping, AllowClamping: i8   => y  y   y   y   y    y  y   y   y   y    y   y);
        test!(clamping, AllowClamping: i16  => y  y   y   y   y    y  y   y   y   y    y   y);
        test!(clamping, AllowClamping: i32  => y  y   y   y   y    y  y   y   y   y    n   y);
        test!(clamping, AllowClamping: i64  => y  y   y   y   y    y  y   y   y   y    n   n);
        test!(clamping, AllowClamping: i128 => y  y   y   y   y    y  y   y   y   y    n   n);

        test!(clamping, AllowClamping: f32  => n  n   n   n   n    n  n   n   n   n    y   y);
        test!(clamping, AllowClamping: f64  => n  n   n   n   n    n  n   n   n   n    n   y);
    }

    #[test]
    fn cast_try_rounding() {
        //                                     u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64
        test!(rounding, AllowRounding: u8   => y  y   y   y   y    n  y   y   y   y    y   y);
        test!(rounding, AllowRounding: u16  => n  y   y   y   y    n  n   y   y   y    y   y);
        test!(rounding, AllowRounding: u32  => n  n   y   y   y    n  n   n   y   y    y   y);
        test!(rounding, AllowRounding: u64  => n  n   n   y   y    n  n   n   n   y    y   y);
        test!(rounding, AllowRounding: u128 => n  n   n   n   y    n  n   n   n   n    y   y);

        test!(rounding, AllowRounding: i8   => n  n   n   n   n    y  y   y   y   y    y   y);
        test!(rounding, AllowRounding: i16  => n  n   n   n   n    n  y   y   y   y    y   y);
        test!(rounding, AllowRounding: i32  => n  n   n   n   n    n  n   y   y   y    y   y);
        test!(rounding, AllowRounding: i64  => n  n   n   n   n    n  n   n   y   y    y   y);
        test!(rounding, AllowRounding: i128 => n  n   n   n   n    n  n   n   n   y    y   y);

        test!(rounding, AllowRounding: f32  => n  n   n   n   n    n  n   n   n   y    y   y);
        test!(rounding, AllowRounding: f64  => n  n   n   n   n    n  n   n   n   n    n   y);
    }

    #[test]
    fn cast_try_lossy() {
        //                          u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64
        test!(lossy, Lossy: u8   => y  y   y   y   y    y  y   y   y   y    y   y);
        test!(lossy, Lossy: u16  => y  y   y   y   y    y  y   y   y   y    y   y);
        test!(lossy, Lossy: u32  => y  y   y   y   y    y  y   y   y   y    y   y);
        test!(lossy, Lossy: u64  => y  y   y   y   y    y  y   y   y   y    y   y);
        test!(lossy, Lossy: u128 => y  y   y   y   y    y  y   y   y   y    y   y);

        test!(lossy, Lossy: i8   => y  y   y   y   y    y  y   y   y   y    y   y);
        test!(lossy, Lossy: i16  => y  y   y   y   y    y  y   y   y   y    y   y);
        test!(lossy, Lossy: i32  => y  y   y   y   y    y  y   y   y   y    y   y);
        test!(lossy, Lossy: i64  => y  y   y   y   y    y  y   y   y   y    y   y);
        test!(lossy, Lossy: i128 => y  y   y   y   y    y  y   y   y   y    y   y);

        test!(lossy, Lossy: f32  => y  y   y   y   y    y  y   y   y   y    y   y);
        test!(lossy, Lossy: f64  => y  y   y   y   y    y  y   y   y   y    y   y);
    }

    #[test]
    fn cast_clamping() {
        assert_eq!(clamping::<u16, u8>(255), 255);
        assert_eq!(clamping::<u16, u8>(256), 255);
        assert_eq!(clamping::<u16, u8>(20_000), 255);

        assert_eq!(clamping::<i16, u8>(255), 255);
        assert_eq!(clamping::<i16, u8>(256), 255);
        assert_eq!(clamping::<i16, u8>(20_000), 255);
        assert_eq!(clamping::<i16, u8>(0), 0);
        assert_eq!(clamping::<i16, u8>(-1), 0);
        assert_eq!(clamping::<i16, u8>(-10_000), 0);

        assert_eq!(clamping::<u16, i8>(127), 127);
        assert_eq!(clamping::<u16, i8>(128), 127);
        assert_eq!(clamping::<u16, i8>(20_000), 127);
        assert_eq!(clamping::<u8, i8>(127), 127);
        assert_eq!(clamping::<u8, i8>(128), 127);

        assert_eq!(clamping::<i16, i8>(127), 127);
        assert_eq!(clamping::<i16, i8>(128), 127);
        assert_eq!(clamping::<i16, i8>(20_000), 127);
        assert_eq!(clamping::<i16, i8>(-128), -128);
        assert_eq!(clamping::<i16, i8>(-129), -128);
        assert_eq!(clamping::<i16, i8>(-20_000), -128);
    }

    #[test]
    fn cast_lossy() {
        assert_eq!(lossy::<f32, i8>(0.0), 0);
        assert_eq!(lossy::<f32, i8>(1.0), 1);
        assert_eq!(lossy::<f32, i8>(-1.0), -1);

        assert!(lossy::<f32, i8>(1.5) == 1 || lossy::<f32, i8>(1.5) == 2);
        assert!(lossy::<f32, i8>(-1.5) == -1 || lossy::<f32, i8>(-1.5) == -2);

        // It's not really easy to test most of this as the rounding mode is
        // not specified.
    }
}
