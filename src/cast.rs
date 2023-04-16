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
//! # Cast Fidelity
//!
//! A cast fidelity describes how "good" a cast is or in other words: what
//! losses we have to expect. There are two things that one might want to
//! avoid: clamping and rounding. The former describes the process of changing
//! a number to fit in a smaller range (e.g. `500u16` to `u8`). "Rounding"
//! means that the input number is inside the range of the destination type,
//! but can not be exactly represented; a reasonable close number of the
//! destiniation type is choosen (e.g. float to int).
//!
//! Two binary choices lead to four different fidelities plus one extra one to
//! disallow any type change:
//! - [`SameType`]: No cast is performed, source and target type are the same.
//! - [`Lossless`]: Number can be represented losslessly in the target type.
//! - [`Clamping`]: For some values, clamping is necessary to represent it in
//!   the target type.
//! - [`Rounding`]: For some values, rounding is necessary to represent it in
//!   the target type.
//! - [`Lossy`]: Clamping and rounding is necessary.
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
//! | **`u128`** |  × |   × |   × |   × |      |  × |   × |   × |   × |    × |   ⊗ |   ○ |
//! |            |    |     |     |     |      |    |     |     |     |      |     |     |
//! | **`i8`**   |  × |   × |   × |   × |    × |    |     |     |     |      |     |     |
//! | **`i16`**  |  × |   × |   × |   × |    × |  × |     |     |     |      |     |     |
//! | **`i32`**  |  × |   × |   × |   × |    × |  × |   × |     |     |      |   ○ |     |
//! | **`i64`**  |  × |   × |   × |   × |    × |  × |   × |   × |     |      |   ○ |   ○ |
//! | **`i128`** |  × |   × |   × |   × |    × |  × |   × |   × |   × |      |   ⊗ |   ○ |
//! |            |    |     |     |     |      |    |     |     |     |      |     |     |
//! | **`f32`**  |  ⊗ |   ⊗ |  ⊗ |   ⊗ |   ⊗ |  ⊗ |   ⊗ |  ⊗ |   ⊗ |   ○  |     |     |
//! | **`f64`**  |  ⊗ |   ⊗ |  ⊗ |   ⊗ |   ⊗ |  ⊗ |   ⊗ |  ⊗ |   ⊗ |   ⊗ |   ⊗ |     |
//!
//! **Note**: unlike the `as` cast, which truncates when casting from larger to
//! smaller integers (e.g. `257u16 as u8 == 1u8`), here integers are clamped
//! into the target range (e.g. `clamping::<_, u8>(257u16) == 255u8`). This
//! makes more sense when considering these are numeric values and approximating
//! during cast.
//!
//!
//! # Casting functions
//!
//! The function [`cast`] is generic over the "required fidelity". There are
//! also convenient functions for each fidelity: [`lossless`], [`clamping`],
//! [`rounding`], [`lossy`]. All these functions fail to compile if the
//! specified fidelity cannot be met.
//!
//! Additionally, there is [`try_cast`] which returns `None` if the specified
//! fidelity cannot be met. Remember: this function does *not* look at the
//! value to decide whether `Some` or `None` is returned, but just at the types!
//!
//!
//! ## Example
//!
//! ```
//! use lox::cast;
//!
//! // This works without a problem as the cast is always lossless.
//! assert_eq!(cast::lossless::<u8, u16>(27), 27);
//! assert_eq!(cast::try_cast::<cast::Lossless, u8, u16>(27), Some(27));
//!
//! // The other way around, we can't always cast without loosing information.
//! // Thus, the first line here would lead to a compiler error. With the
//! // `try_*` version you can get `None` instead of a compiler error.
//! //assert_eq!(cast::lossless::<u16, u8>(27), 27);
//! assert_eq!(cast::try_cast::<cast::Lossless, u16, u8>(10), None);
//! assert_eq!(cast::try_cast::<cast::Lossless, u16, u8>(300), None);
//!
//! // When we allow clamping, we can cast.
//! assert_eq!(cast::clamping::<u16, u8>(300), 255);
//! assert_eq!(cast::try_cast::<cast::Clamping, u16, u8>(300), Some(255));
//! ```
//!
//! # Casting traits
//!
//! The traits are mainly used to implement the functions of this module and
//! usually don't need to be used directly. However, you can use some traits as
//! trait bounds for your own functions.
//!
//! `CastFrom` is implemented for all combination of primitive Rust types
//! (unsigned integers, signed integers and floating point types).
//!

use crate::{
    sealed::Sealed, util::{Bool, True, False},
};


// ===========================================================================
// ===== Casting functions
// ===========================================================================

/// Cast `src` from type `Src` to the type `Dst`, with at least fidelity `F`.
///
/// If the cast is not possible while meeting fidelity `F`, this won't compile.
///
/// Instead of using this generic function, there is a specific function for
/// each cast rigor that you can use. Usually, that's easier.
#[inline(always)]
pub fn cast<F, Src, Dst>(src: Src) -> Dst
where
    F: Fidelity,
    Src: CastInto<Dst>,
    Src::Fidelity: SufficientFor<F>,
{
    src.cast_into()
}

/// Cast `src` from type `Src` to the type `Dst`, without losing information.
#[inline(always)]
pub fn lossless<Src, Dst>(src: Src) -> Dst
where
    Src: CastInto<Dst>,
    Src::Fidelity: SufficientFor<Lossless>,
{
    src.cast_into()
}

/// Cast `src` from type `Src` to the type `Dst`, with clamping being allowed.
#[inline(always)]
pub fn clamping<Src, Dst>(src: Src) -> Dst
where
    Src: CastInto<Dst>,
    Src::Fidelity: SufficientFor<Clamping>,
{
    src.cast_into()
}

/// Cast `src` from type `Src` to the type `Dst`, with rounding being allowed.
#[inline(always)]
pub fn rounding<Src, Dst>(src: Src) -> Dst
where
    Src: CastInto<Dst>,
    Src::Fidelity: SufficientFor<Rounding>,
{
    src.cast_into()
}

/// Cast `src` from type `Src` to the type `Dst`, with clamping and rounding
/// being allowed.
#[inline(always)]
pub fn lossy<Src, Dst>(src: Src) -> Dst
where
    Src: CastInto<Dst>,
{
    src.cast_into()
}

/// Determines whether casting from `Src` to `Dst` is possible while meeting the
/// specified fidelity.
///
/// If this function returns `false`, `try_*` will always return `None` for
/// the same type arguments. Similarly, it will always return `Some` if this
/// function returns `true`.
#[inline(always)]
pub fn is_cast_possible<F, Src, Dst>() -> bool
where
    F: Fidelity,
    Src: CastInto<Dst>,
    Src::Fidelity: GreaterOrEqual<F>,
{
    <Src::Fidelity as GreaterOrEqual<F>>::Out::VALUE
}

/// Casts `src` from type `Src` to the type `Dst`, with at least fidelty `F`, or
/// returns `None` if the types cannot be cast with said fidelity.
///
/// This is like [`cast`] but returns `None` if the two types cannot be cast
/// with at least the specified fidelity, instead of refusing to compile. Note
/// that the decision whether the types can be cast only depends on the types
/// and not the value. Thus, whether `Some` or `None` is returned is known at
/// compile time.
#[inline(always)]
pub fn try_cast<F, Src, Dst>(src: Src) -> Option<Dst>
where
    F: Fidelity,
    Src: CastInto<Dst>,
    Src::Fidelity: GreaterOrEqual<F>,
{
    if is_cast_possible::<F, Src, Dst>() {
        Some(src.cast_into())
    } else {
        None
    }
}


// ===========================================================================
// ===== Casting fidelities
// ===========================================================================

/// Describes the fidelity of a cast.
///
/// This trait is only implemented for the five different fidelities defined in
/// this module and cannot be implemented for own types.
pub trait Fidelity:
    Sealed
    + GreaterOrEqual<SameType>
    + GreaterOrEqual<Lossless>
    + GreaterOrEqual<Rounding>
    + GreaterOrEqual<Clamping>
    + GreaterOrEqual<Lossy>
{}

/// Cast fidelity: source and destinition type are the same, no casting takes
/// place.
#[derive(Debug)]
pub enum SameType {}
impl Sealed for SameType {}
impl Fidelity for SameType {}

/// Cast fidelity: source value can be represented losslessly by target type.
#[derive(Debug)]
pub enum Lossless {}
impl Sealed for Lossless {}
impl Fidelity for Lossless {}

/// Cast fidelity: clamping is required for some values when casting.
#[derive(Debug)]
pub enum Clamping {}
impl Sealed for Clamping {}
impl Fidelity for Clamping {}

/// Cast fidelity: rounding is required for some values when casting.
#[derive(Debug)]
pub enum Rounding {}
impl Sealed for Rounding {}
impl Fidelity for Rounding {}

/// Cast fidelity: round and clamping is required for some values when casting.
#[derive(Debug)]
pub enum Lossy {}
impl Sealed for Lossy {}
impl Fidelity for Lossy {}

/// Implemented if `Self` represents the same or better fidelity than `Req`.
pub trait SufficientFor<Req: Fidelity>: Fidelity {}

impl<L: Fidelity, R: Fidelity> SufficientFor<R> for L
where
    L: GreaterOrEqual<R, Out = True>,
{}

/// Defines a relationship between fidelities, specifically whether `Self` is
/// the same or a greater fidelity than `Rhs`.
///
/// Implemented for all fidelity combinations.
pub trait GreaterOrEqual<Rhs: Fidelity> {
    type Out: Bool;
}

impl GreaterOrEqual<SameType> for SameType { type Out = True; }
impl GreaterOrEqual<Lossless> for SameType { type Out = True; }
impl GreaterOrEqual<Rounding> for SameType { type Out = True; }
impl GreaterOrEqual<Clamping> for SameType { type Out = True; }
impl GreaterOrEqual<Lossy> for SameType { type Out = True; }

impl GreaterOrEqual<SameType> for Lossless { type Out = False; }
impl GreaterOrEqual<Lossless> for Lossless { type Out = True; }
impl GreaterOrEqual<Rounding> for Lossless { type Out = True; }
impl GreaterOrEqual<Clamping> for Lossless { type Out = True; }
impl GreaterOrEqual<Lossy> for Lossless { type Out = True; }

impl GreaterOrEqual<SameType> for Rounding { type Out = False; }
impl GreaterOrEqual<Lossless> for Rounding { type Out = False; }
impl GreaterOrEqual<Rounding> for Rounding { type Out = True; }
impl GreaterOrEqual<Clamping> for Rounding { type Out = False; }
impl GreaterOrEqual<Lossy> for Rounding { type Out = True; }

impl GreaterOrEqual<SameType> for Clamping { type Out = False; }
impl GreaterOrEqual<Lossless> for Clamping { type Out = False; }
impl GreaterOrEqual<Rounding> for Clamping { type Out = False; }
impl GreaterOrEqual<Clamping> for Clamping { type Out = True; }
impl GreaterOrEqual<Lossy> for Clamping { type Out = True; }

impl GreaterOrEqual<SameType> for Lossy { type Out = False; }
impl GreaterOrEqual<Lossless> for Lossy { type Out = False; }
impl GreaterOrEqual<Rounding> for Lossy { type Out = False; }
impl GreaterOrEqual<Clamping> for Lossy { type Out = False; }
impl GreaterOrEqual<Lossy> for Lossy { type Out = True; }


// ===========================================================================
// ===== Casting traits
// ===========================================================================

/// Ability to be casted from the type `Src`.
///
/// This is the core trait of this module. It is implemented for all
/// combinations of primitive types (12 x 12 = 144 impls).
pub trait CastFrom<Src> {
    /// Fidelity with which this cast is performed.
    type Fidelity: Fidelity;

    /// Performs the cast.
    ///
    /// This mostly has the same behavior as the `as` cast, except for casting
    /// larger integers to smaller ones. In the latter case, the value is
    /// clamped into the representable range, i.e. `1000u16 -> 255u8`. The
    /// exact cast semantics of `as` are described [here][spec].
    ///
    /// [spec]: https://doc.rust-lang.org/reference/expressions/operator-expr.html#semantics
    fn cast_from(src: Src) -> Self;
}

impl<T> CastFrom<T> for T {
    type Fidelity = SameType;
    fn cast_from(src: T) -> Self {
        src
    }
}

/// Symmetric counterpart of [`CastFrom`].
///
/// There is a blanket implementation that implements this for all types for
/// which [`CastFrom`] is implemented. So you usually don't have to implement
/// this trait directly.
pub trait CastInto<Dst> {
    type Fidelity: Fidelity;
    fn cast_into(self) -> Dst;
}

impl<Src, Dst: CastFrom<Src>> CastInto<Dst> for Src {
    type Fidelity = Dst::Fidelity;
    fn cast_into(self) -> Dst {
        Dst::cast_from(self)
    }
}

// ===========================================================================
// ===== Implementations for primitive types
// ===========================================================================

macro_rules! impl_cast {
    ($($src:ident -> $dst:ident : $fidelity:ident $(. $direction:ident)? ,)*) => {
        $(
            impl CastFrom<$src> for $dst {
                type Fidelity = $fidelity;
                fn cast_from(src: $src) -> Self {
                    impl_cast!(@imp $src -> $dst : $fidelity $(. $direction)?; src)
                }
            }
        )*
    };

    // The actual implementations on how it is cast. Compare the reference:
    // https://doc.rust-lang.org/reference/expressions/operator-expr.html#semantics

    // Lossless casts are available via the `Into` trait.
    (@imp $src:ident -> $dst:ident : Lossless; $v:ident) => { $v.into() };

    // This only concerns int -> float and f32 -> i128. All these cases work as
    // expected with the `as` cast operator: only rounding takes place.
    (@imp $src:ident -> $dst:ident : Rounding; $v:ident) => { $v as _ };

    // This only concerns float -> int, f64 -> f32 and i128/u128 -> f32. All
    // these cases work as expected with the `as` cast operator.
    (@imp $src:ident -> $dst:ident : Lossy; $v:ident) => { $v as _ };

    // Only clamping operations which only concern int -> int casts are a bit
    // more tricky. The `as` operator truncates instead of clamps. This might
    // make sense generally but not in the context of numeric operations. So we
    // have to add checks ourselves. To improve efficiency, we differentiate
    // three different cases: pos, neg and both. Which just say at what ends
    // the clamping might occur.
    (@imp $src:ident -> $dst:ident : Clamping.pos; $v:ident) => {
        if $v > $dst::max_value() as $src {
            $dst::max_value()
        } else {
            $v as $dst
        }
    };
    (@imp $src:ident -> $dst:ident : Clamping.neg; $v:ident) => {
        if $v < $dst::min_value() as $src {
            $dst::min_value()
        } else {
            $v as $dst
        }
    };
    (@imp $src:ident -> $dst:ident : Clamping.both; $v:ident) => {
        if $v > $dst::max_value() as $src {
            $dst::max_value()
        } else if $v < $dst::min_value() as $src {
            $dst::min_value()
        } else {
            $v as $dst
        }
    };
}

// `SameType` casts are not part of this as they are implemented above in a
// blanket impl.
impl_cast! {
    // ----- Unsigned integers -----------------------------------------------
    u8 ->  u16: Lossless,
    u8 ->  u32: Lossless,
    u8 ->  u64: Lossless,
    u8 -> u128: Lossless,
    u8 ->   i8: Clamping.pos,
    u8 ->  i16: Lossless,
    u8 ->  i32: Lossless,
    u8 ->  i64: Lossless,
    u8 -> i128: Lossless,
    u8 ->  f32: Lossless,
    u8 ->  f64: Lossless,

    u16 ->   u8: Clamping.pos,
    u16 ->  u32: Lossless,
    u16 ->  u64: Lossless,
    u16 -> u128: Lossless,
    u16 ->   i8: Clamping.pos,
    u16 ->  i16: Clamping.pos,
    u16 ->  i32: Lossless,
    u16 ->  i64: Lossless,
    u16 -> i128: Lossless,
    u16 ->  f32: Lossless,
    u16 ->  f64: Lossless,

    u32 ->   u8: Clamping.pos,
    u32 ->  u16: Clamping.pos,
    u32 ->  u64: Lossless,
    u32 -> u128: Lossless,
    u32 ->   i8: Clamping.pos,
    u32 ->  i16: Clamping.pos,
    u32 ->  i32: Clamping.pos,
    u32 ->  i64: Lossless,
    u32 -> i128: Lossless,
    u32 ->  f32: Rounding,
    u32 ->  f64: Lossless,

    u64 ->   u8: Clamping.pos,
    u64 ->  u16: Clamping.pos,
    u64 ->  u32: Clamping.pos,
    u64 -> u128: Lossless,
    u64 ->   i8: Clamping.pos,
    u64 ->  i16: Clamping.pos,
    u64 ->  i32: Clamping.pos,
    u64 ->  i64: Clamping.pos,
    u64 -> i128: Lossless,
    u64 ->  f32: Rounding,
    u64 ->  f64: Rounding,

    u128 ->   u8: Clamping.pos,
    u128 ->  u16: Clamping.pos,
    u128 ->  u32: Clamping.pos,
    u128 ->  u64: Clamping.pos,
    u128 ->   i8: Clamping.pos,
    u128 ->  i16: Clamping.pos,
    u128 ->  i32: Clamping.pos,
    u128 ->  i64: Clamping.pos,
    u128 -> i128: Clamping.pos,
    u128 ->  f32: Lossy,
    u128 ->  f64: Rounding,

    // ----- Signed integers -------------------------------------------------
    i8 ->   u8: Clamping.neg,
    i8 ->  u16: Clamping.neg,
    i8 ->  u32: Clamping.neg,
    i8 ->  u64: Clamping.neg,
    i8 -> u128: Clamping.neg,
    i8 ->  i16: Lossless,
    i8 ->  i32: Lossless,
    i8 ->  i64: Lossless,
    i8 -> i128: Lossless,
    i8 ->  f32: Lossless,
    i8 ->  f64: Lossless,

    i16 ->   u8: Clamping.both,
    i16 ->  u16: Clamping.neg,
    i16 ->  u32: Clamping.neg,
    i16 ->  u64: Clamping.neg,
    i16 -> u128: Clamping.neg,
    i16 ->   i8: Clamping.both,
    i16 ->  i32: Lossless,
    i16 ->  i64: Lossless,
    i16 -> i128: Lossless,
    i16 ->  f32: Lossless,
    i16 ->  f64: Lossless,

    i32 ->   u8: Clamping.both,
    i32 ->  u16: Clamping.both,
    i32 ->  u32: Clamping.neg,
    i32 ->  u64: Clamping.neg,
    i32 -> u128: Clamping.neg,
    i32 ->   i8: Clamping.both,
    i32 ->  i16: Clamping.both,
    i32 ->  i64: Lossless,
    i32 -> i128: Lossless,
    i32 ->  f32: Rounding,
    i32 ->  f64: Lossless,

    i64 ->   u8: Clamping.both,
    i64 ->  u16: Clamping.both,
    i64 ->  u32: Clamping.both,
    i64 ->  u64: Clamping.neg,
    i64 -> u128: Clamping.neg,
    i64 ->   i8: Clamping.both,
    i64 ->  i16: Clamping.both,
    i64 ->  i32: Clamping.both,
    i64 -> i128: Lossless,
    i64 ->  f32: Rounding,
    i64 ->  f64: Rounding,

    i128 ->   u8: Clamping.both,
    i128 ->  u16: Clamping.both,
    i128 ->  u32: Clamping.both,
    i128 ->  u64: Clamping.both,
    i128 -> u128: Clamping.neg,
    i128 ->   i8: Clamping.both,
    i128 ->  i16: Clamping.both,
    i128 ->  i32: Clamping.both,
    i128 ->  i64: Clamping.both,
    i128 ->  f32: Lossy,
    i128 ->  f64: Rounding,

    // ----- Floats ----------------------------------------------------------
    f32 ->   u8: Lossy,
    f32 ->  u16: Lossy,
    f32 ->  u32: Lossy,
    f32 ->  u64: Lossy,
    f32 -> u128: Lossy,
    f32 ->   i8: Lossy,
    f32 ->  i16: Lossy,
    f32 ->  i32: Lossy,
    f32 ->  i64: Lossy,
    f32 -> i128: Rounding,
    f32 ->  f64: Lossless,

    f64 ->   u8: Lossy,
    f64 ->  u16: Lossy,
    f64 ->  u32: Lossy,
    f64 ->  u64: Lossy,
    f64 -> u128: Lossy,
    f64 ->   i8: Lossy,
    f64 ->  i16: Lossy,
    f64 ->  i32: Lossy,
    f64 ->  i64: Lossy,
    f64 -> i128: Lossy,
    f64 ->  f32: Lossy,
}


// ===========================================================================
// ===== Test
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[inline(never)]
    fn check<F, SrcT, DstT>(
        src: SrcT,
        dst: DstT,
        should_succeed: bool,
        rigor_str: &str,
        src_str: &str,
        dst_str: &str,
    )
    where
        F: Fidelity,
        SrcT: Copy,
        DstT: PartialEq + Copy + CastFrom<SrcT>,
        DstT::Fidelity: GreaterOrEqual<F>,
    {
        let (expected_val, expected_str, actual_str) = if should_succeed {
            (Some(dst), "succeed", "failed")
        } else {
            (None, "fail", "succeeded")
        };


        // Test generic `try_cast`
        if try_cast::<F, SrcT, DstT>(src) != expected_val {
            panic!(
                "expected {} -> {} `try_cast` to {}, but it {}",
                src_str,
                dst_str,
                expected_str,
                actual_str,
            );
        }

        // Test `is_cast_possible`
        if is_cast_possible::<F, SrcT, DstT>() != should_succeed {
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

    macro_rules! test {
        (
            $fun:ident, $rigor:ident: $ty:ident =>
            $u8:tt $u16:tt $u32:tt $u64:tt $u128:tt
            $i8:tt $i16:tt $i32:tt $i64:tt $i128:tt
            $f32:tt $f64:tt
        ) => {{
            test!(@check $rigor: $ty as u8 => $u8);
            test!(@check $rigor: $ty as u16 => $u16);
            test!(@check $rigor: $ty as u32 => $u32);
            test!(@check $rigor: $ty as u64 => $u64);
            test!(@check $rigor: $ty as u128 => $u128);

            test!(@check $rigor: $ty as i8 => $i8);
            test!(@check $rigor: $ty as i16 => $i16);
            test!(@check $rigor: $ty as i32 => $i32);
            test!(@check $rigor: $ty as i64 => $i64);
            test!(@check $rigor: $ty as i128 => $i128);

            test!(@check $rigor: $ty as f32 => $f32);
            test!(@check $rigor: $ty as f64 => $f64);
        }};

        (@check $rigor:ident: $src:ident as $dst:ident => $outcome:tt) => {
            check::<$rigor, $src, $dst>(
                test!(@lit $src),
                test!(@lit $dst),
                test!(@to_bool $outcome),
                stringify!($rigor),
                stringify!($src),
                stringify!($dst),
            )
        };
        (@to_bool n) => { false };
        (@to_bool y) => { true };
        (@lit f32) => { 3.0 };
        (@lit f64) => { 3.0 };
        (@lit $integer_ty:ident) => { 3 };
    }

    #[test]
    fn cast_try_no_cast() {
        //                               u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64
        test!(no_cast, SameType: u8   => y  n   n   n   n    n  n   n   n   n    n   n);
        test!(no_cast, SameType: u16  => n  y   n   n   n    n  n   n   n   n    n   n);
        test!(no_cast, SameType: u32  => n  n   y   n   n    n  n   n   n   n    n   n);
        test!(no_cast, SameType: u64  => n  n   n   y   n    n  n   n   n   n    n   n);
        test!(no_cast, SameType: u128 => n  n   n   n   y    n  n   n   n   n    n   n);

        test!(no_cast, SameType: i8   => n  n   n   n   n    y  n   n   n   n    n   n);
        test!(no_cast, SameType: i16  => n  n   n   n   n    n  y   n   n   n    n   n);
        test!(no_cast, SameType: i32  => n  n   n   n   n    n  n   y   n   n    n   n);
        test!(no_cast, SameType: i64  => n  n   n   n   n    n  n   n   y   n    n   n);
        test!(no_cast, SameType: i128 => n  n   n   n   n    n  n   n   n   y    n   n);

        test!(no_cast, SameType: f32  => n  n   n   n   n    n  n   n   n   n    y   n);
        test!(no_cast, SameType: f64  => n  n   n   n   n    n  n   n   n   n    n   y);
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
        //                                u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64
        test!(clamping, Clamping: u8   => y  y   y   y   y    y  y   y   y   y    y   y);
        test!(clamping, Clamping: u16  => y  y   y   y   y    y  y   y   y   y    y   y);
        test!(clamping, Clamping: u32  => y  y   y   y   y    y  y   y   y   y    n   y);
        test!(clamping, Clamping: u64  => y  y   y   y   y    y  y   y   y   y    n   n);
        test!(clamping, Clamping: u128 => y  y   y   y   y    y  y   y   y   y    n   n);

        test!(clamping, Clamping: i8   => y  y   y   y   y    y  y   y   y   y    y   y);
        test!(clamping, Clamping: i16  => y  y   y   y   y    y  y   y   y   y    y   y);
        test!(clamping, Clamping: i32  => y  y   y   y   y    y  y   y   y   y    n   y);
        test!(clamping, Clamping: i64  => y  y   y   y   y    y  y   y   y   y    n   n);
        test!(clamping, Clamping: i128 => y  y   y   y   y    y  y   y   y   y    n   n);

        test!(clamping, Clamping: f32  => n  n   n   n   n    n  n   n   n   n    y   y);
        test!(clamping, Clamping: f64  => n  n   n   n   n    n  n   n   n   n    n   y);
    }

    #[test]
    fn cast_try_rounding() {
        //                                u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64
        test!(rounding, Rounding: u8   => y  y   y   y   y    n  y   y   y   y    y   y);
        test!(rounding, Rounding: u16  => n  y   y   y   y    n  n   y   y   y    y   y);
        test!(rounding, Rounding: u32  => n  n   y   y   y    n  n   n   y   y    y   y);
        test!(rounding, Rounding: u64  => n  n   n   y   y    n  n   n   n   y    y   y);
        test!(rounding, Rounding: u128 => n  n   n   n   y    n  n   n   n   n    n   y);

        test!(rounding, Rounding: i8   => n  n   n   n   n    y  y   y   y   y    y   y);
        test!(rounding, Rounding: i16  => n  n   n   n   n    n  y   y   y   y    y   y);
        test!(rounding, Rounding: i32  => n  n   n   n   n    n  n   y   y   y    y   y);
        test!(rounding, Rounding: i64  => n  n   n   n   n    n  n   n   y   y    y   y);
        test!(rounding, Rounding: i128 => n  n   n   n   n    n  n   n   n   y    n   y);

        test!(rounding, Rounding: f32  => n  n   n   n   n    n  n   n   n   y    y   y);
        test!(rounding, Rounding: f64  => n  n   n   n   n    n  n   n   n   n    n   y);
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
        // TODO: rounding mode is now specified in the specs, add tests!
    }
}
