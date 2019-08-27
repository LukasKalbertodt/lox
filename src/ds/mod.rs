//! Polygon mesh **d**ata **s**tructures.
//!
//! This module contains the different implementations of the mesh traits in
//! this libary.
//!
//! # TODO
//! - list all implementations
//! - explain advantages and disadvantages of data structures

use std::fmt;

use crate::{
    traits::marker::{Bool, True, False},
};

#[cfg(test)]
#[macro_use]
mod tests;

mod checked;
pub mod directed_edge;
// mod face_delegate;
pub mod half_edge;
mod shared_vertex;

pub use self::{
    directed_edge::DirectedEdgeMesh,
    // face_delegate::FaceDelegateMesh,
    half_edge::HalfEdgeMesh,
    shared_vertex::SharedVertexMesh,
};

pub(crate) use self::checked::Checked;


// ===== Utilities for optional fields in mesh data structures -----------------------------------

/// This is a helper for `TypeOpt`.
///
/// With this, we build a type system function from a Boolean and a type `T` to
/// a type. That output type is either `()` or `T`. Unfortunately, we have to
/// use a few hacks to actually make this work.
trait TypeOrVoid<T: Copy + fmt::Debug>: Bool {
    /// The output type. `T` for `True`, and `()` for `False`.
    type Output: Copy + fmt::Debug;

    /// Convert `T` to the output type. Either the value is just returned or
    /// discarded.
    fn new(t: T) -> Self::Output;

    /// Depending on the Boolean, we return `None` or `Some<T>`. This does look
    /// like we move compile-time decisions to the runtime. This is true, but
    /// the compiler can always optimize this away. It's just more convenient
    /// this way.
    fn into_option(v: Self::Output) -> Option<T>;
}

impl<B: Bool, T: Copy + fmt::Debug> TypeOrVoid<T> for B {
    // Unreachable. The compiler doesn't know that `True` and `False` are the
    // only types implementing `Bool` (because well, we could add new ones). So
    // the compiler doesn't know that this impl will never be active.
    default type Output = !;
    default fn new(_: T) -> Self::Output {
        unreachable!()
    }
    default fn into_option(_: Self::Output) -> Option<T> {
        unreachable!()
    }
}

// Specialization for `True`
impl<T: Copy + fmt::Debug> TypeOrVoid<T> for True {
    type Output = T;
    fn new(t: T) -> Self::Output {
        t
    }
    fn into_option(v: Self::Output) -> Option<T> {
        Some(v)
    }
}

// Specialization for `False`
impl<T: Copy + fmt::Debug> TypeOrVoid<T> for False {
    type Output = ();
    fn new(_: T) -> Self::Output {
        ()
    }
    fn into_option(_: Self::Output) -> Option<T> {
        None
    }
}

/// An optional value of type `T` which presence is determined by the compile
/// time Boolean `B`.
///
/// If `B` is `False`, this type is always zero sized, otherwise it has the
/// same size as `T` and contains an instance of that type.
///
/// Due to limitations of the Rust compiler we already have to specify `Copy`
/// and `Debug` bounds here although they have nothing to do with the `TypeOpt`
/// type itself. That's because we need those bounds later where we use
/// `TypeOpt`.
struct TypeOpt<T: Copy + fmt::Debug, B: Bool>(<B as TypeOrVoid<T>>::Output);

impl<T: Copy + fmt::Debug, B: Bool> TypeOpt<T, B> {
    /// Create a new instance of this optional. If `B` is `False`, the value
    /// `v` is just discarded. You can also use the `From<T>` impl.
    fn new(v: T) -> Self {
        Self(<B as TypeOrVoid<T>>::new(v))
    }

    /// Converts this optional in a standard `Option`. While this seems like we
    /// lose the compile-time advantage here, the compiler can easily optimize
    /// this. Depending on `B`, either `None` or `Some` is returned without
    /// depending on any runtime value.
    fn into_option(self) -> Option<T> {
        <B as TypeOrVoid<T>>::into_option(self.0)
    }
}

impl<T: Copy + fmt::Debug, B: Bool> From<T> for TypeOpt<T, B> {
    fn from(t: T) -> Self {
        Self::new(t)
    }
}

impl<T: Copy + fmt::Debug, B: Bool> Copy for TypeOpt<T, B> {}
impl<T: Copy + fmt::Debug, B: Bool> Clone for TypeOpt<T, B> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}
