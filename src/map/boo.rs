//! Functionality as a workaround for the lack of GATs for lifetimes.
//!
//! This module allows a way to abstract over borrowed and owned values without
//! introducing runtime checks (which is what `Cow` would do). This allows
//! `PropMap` to either return owned or borrowed values. This is the classical
//! *streaming iterator* problem. To solve this, we would make the associated
//! type `Item` generic over a lifetime, but this is not yet possible.
//!
//! A workaround is to bring the lifetime parameter into the trait directly.
//! But this leads to a long list of other problems and many many `for<'s>`
//! everywhere. So this is a slightly better workaround. It still requires some
//! boilerplate code here and there, but is not restricting due to strange
//! lifetimes.
//!
//! This module is called `boo` for two reasons:
//!
//! - It's an acronym for **b**orrowed **o**r **o**wned
//! - Ghosts say boo. This is kinda creepy and ugly and should be avoided. But
//!   it stays until GATs are here.
//!
//! For more information, see this [blog
//! post](http://tiny.cc/streaming-iterator-gats).

use std::ops;

/// Types which denote if a type is borrowed or owned.
///
/// This trait is only implemented by two types, [`Owned`] and [`Borrowed`].
/// You **shouldn't** implement this trait for your own types!
pub trait Marker {
    /// For internal use: by setting this to `!` (never), we can make sure that
    /// the compiler removes some branches.
    type RefDummy;

    /// For internal use: by setting this to `!` (never), we can make sure that
    /// the compiler removes some branches.
    type OwnedDummy;
}

/// An owned type T. This type cannot be constructed and can only be used as a
/// marker.
pub enum Owned {}

impl Marker for Owned {
    type RefDummy = !;
    type OwnedDummy = ();
}

/// A borrowed type T. This type cannot be constructed and can only be used as a
/// marker.
pub enum Borrowed {}

impl Marker for Borrowed {
    type RefDummy = ();
    type OwnedDummy = !;
}

/// Holds either an owned or a borrowed value. The borrowed/owned state is
/// fully determined by the type parameter `M`!
///
/// This type can only be used in two ways: `Wrap<T, Borrowed>` and
/// `Wrap<T, Owned>`. To create an instance of this type use the appropriate
/// `From` impls (usually via `.into()`).
///
/// This type implements `Deref` which allows you to get a reference to the
/// underlying data. If you want extract the exact value from this type, use
/// the `.into_inner()` method.
pub struct Wrap<'a, T: 'a, M: Marker>(WrapInner<'a, T, M>);


/// The actual `Wrap` implementation. But we don't want to make the fields
/// public, so we wrap it in a struct.
enum WrapInner<'a, T: 'a, M: Marker> {
    Borrowed(&'a T, M::RefDummy),
    Owned(T, M::OwnedDummy),
}

impl<T, M: Marker> ops::Deref for Wrap<'_, T, M> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        match &self.0 {
            WrapInner::Borrowed(b,_ ) => b,
            WrapInner::Owned(v, _) => v,
        }
    }
}

impl<'a, T> From<T> for Wrap<'a, T, Owned> {
    fn from(src: T) -> Self {
        Wrap(WrapInner::Owned(src, ()))
    }
}

impl<'a, T> From<&'a T> for Wrap<'a, T, Borrowed> {
    fn from(src: &'a T) -> Self {
        Wrap(WrapInner::Borrowed(src, ()))
    }
}

impl<T> Wrap<'_, T, Owned> {
    /// Returns the underlying value.
    pub fn into_inner(self) -> T {
        match self.0 {
            WrapInner::Borrowed(_, never) => never,
            WrapInner::Owned(v, _) => v,
        }
    }
}

impl<'a, T> Wrap<'a, T, Borrowed> {
    /// Returns the underlying value.
    pub fn into_inner(self) -> &'a T {
        match self.0 {
            WrapInner::Borrowed(v, _) => v,
            WrapInner::Owned(_, never) => never,
        }
    }
}
