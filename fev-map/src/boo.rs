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

use std::{
    marker::PhantomData,
    ops,
};

/// Types which wrap another type and denote it's borrowed or owned.
///
/// This trait is only implemented by two types, [`Owned`] and [`Borrowed`].
/// You **shouldn't** implement this trait for your own types!
///
/// To get the wrapped type, use the `Inner` associated type!
pub trait Marker {
    /// The wrapped type. Use this to define any bounds on the type.
    type Inner;

    /// For internal use: by setting this to `!` (never), we can make sure that
    /// the compiler removes some branches.
    type RefDummy;

    /// For internal use: by setting this to `!` (never), we can make sure that
    /// the compiler removes some branches.
    type OwnedDummy;
}

/// An owned type T. This type cannot be constructed and can only be used as a
/// marker.
pub struct Owned<T> {
    _never: !,
    _phantom: PhantomData<T>,
}

impl<T> Marker for Owned<T> {
    type Inner = T;
    type RefDummy = !;
    type OwnedDummy = ();
}

/// A borrowed type T. This type cannot be constructed and can only be used as a
/// marker.
pub struct Borrowed<T> {
    _never: !,
    _phantom: PhantomData<T>,
}

impl<T> Marker for Borrowed<T> {
    type Inner = T;
    type RefDummy = ();
    type OwnedDummy = !;
}

/// Holds either an owned or a borrowed value. The borrowed/owned state is
/// fully determined by the type parameter `T`!
///
/// This type can only be used in two ways: `Wrap<Borrowed<T>>` and
/// `Wrap<Owned<T>>`. To create an instance of this type use the appropriate
/// `From` impls (usually via `.into()`).
///
/// This type implements `Deref` which allows you to get a reference to the
/// underlying data.
pub enum Wrap<'a, T: Marker>
where
    T::Inner: 'a,
{
    Borrowed(&'a T::Inner, T::RefDummy),
    Owned(T::Inner, T::OwnedDummy),
}

impl<'a, T: Marker> ops::Deref for Wrap<'a, T> {
    type Target = T::Inner;
    fn deref(&self) -> &Self::Target {
        match self {
            Wrap::Borrowed(b,_ ) => b,
            Wrap::Owned(v, _) => v,
        }
    }
}

impl<'a, T> From<T> for Wrap<'a, Owned<T>> {
    fn from(src: T) -> Self {
        Wrap::Owned(src, ())
    }
}

impl<'a, T> From<&'a T> for Wrap<'a, Borrowed<T>> {
    fn from(src: &'a T) -> Self {
        Wrap::Borrowed(src, ())
    }
}
