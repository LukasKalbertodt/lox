//! Map data structures: associating a property (some data) with a vertex, face,
//! or edge.
//!
//! This crate is built around storing all your mesh properties
//! (position, normals, colors, ...) separately. And in particular, separate
//! from the connectivity information (the "core mesh"). This is more flexible
//! and, maybe counter-intuitively, can be faster in many situations. So how to
//! store those properties? Prop maps and prop stores!
//!
//!
//! # Traits
//!
//! This module defines three traits, which are connected via super-trait
//! bounds:
//!
//! ```ignore
//! trait PropMap<H> { ... }
//! trait PropStore<H>: PropMap<H> { ... }
//! trait PropStoreMut<H>: PropStore<H> { ... }
//! ```
//!
//! [`PropMap`] is the most abstract trait, only requiring the basic
//! `fn(self, Handle) -> Option<Property>` function. It can be implemented by data structures
//! storing properties, but also by closures generating the property on the
//! fly. [`PropStore`] and [`PropStoreMut`] is only for data structures actually
//! storing the properties and they offer a lot more methods, e.g. for
//! iterating or mutating the data structure.
//!
//!
//! # Implementations
//!
//! There are currently two implementations of `PropStore`, allowing you to
//! store properties:
//!
//! - [`SparseMap`]: A `HashMap` under the hood. Performs well in almost all
//!   situations, regardless property density.
//! - [`DenseMap`]: Well suited for cases where have a property for (almost) all
//!   mesh handles of a specific kind (e.g. faces). Faster than a `SparseMap`
//!   in these cases. Pretty bad in all other cases. Uses the handle's index
//!   to index into a `Vec`.
//!
//! In addition to the types above, the following types also (but only)
//! implement `PropMap`.
//!
//! - [`ConstMap`]: Returns the same prop value for all handles.
//! - [`EmptyMap`]: Returns `None` for all handles.
//! - [`FnMap`]: Uses a closure to calculate the prop for a handle.
//!
//!
//!
//!

use std::{ops, marker::PhantomData, fmt, borrow::Borrow};

use crate::{
    hsize,
    prelude::*,
};


#[cfg(test)]
#[macro_use]
mod tests;

pub mod adaptors;
mod dense;
mod fn_map;
pub mod set;
mod sparse;
mod special_maps;
mod util;

pub use self::{
    fn_map::FnMap,
    sparse::SparseMap,
    special_maps::{ConstMap, EmptyMap},
    dense::DenseMap,
};


// ===========================================================================
// ===== Main traits
// ===========================================================================

/// A mapping from a handle to some data (property).
///
/// This is a bare minimal trait representing all types that can map a handle
/// to optional data, called property. The returned property can be owned or
/// borrowed from `self`.
///
/// The abstraction over 'owned' and 'borrowed' is not easy and is more verbose
/// than I'd like. Ideally, this trait would have one `type Target<'s>` and the
/// `get` method would return `Option<Self::Target<'_>>`. However, that's far
/// from ideal in practice. To solve this, the returned value is wrapped in
/// [`Value`] which is a very thin wrapper, that also implements `Deref`. Also,
/// for several reasons, there are two associated types: [`Self::Target`] and
/// [`Self::Ret`].
///
/// Using the returned value from `get` is thus a bit weird. You can always read
/// it through its `Deref` impl. And in most cases, you have bound it by
/// something like `Pos3Like` anyway, which allows you to just `Copy` it.
///
///
/// # Examples
///
/// Using `PropMap` in a function:
///
/// ```
/// use lox::{VertexHandle, prelude::*};
///
/// fn highest_vertex<M, P>(mesh: &M, vertex_positions: &P) -> Option<P::Target>
/// where
///     M: Mesh,
///     P: PropMap<VertexHandle>,
///     P::Target: Pos3Like,
/// {
///     let mut out: Option<P::Target> = None;
///     for vh in mesh.vertex_handles() {
///         if let Some(pos) = vertex_positions.get(vh) {
///             // Is the new vertex higher up?
///             if out.map_or(true, |highest| highest.z() < pos.z()) {
///                 // With `*` we copy the value out of the `Value` wrapper.
///                 out = Some(*pos);
///             }
///         }
///     }
///
///     out
/// }
/// ```
///
/// For examples on implementing this, see the provided implementations in this
/// crate.
pub trait PropMap<H: Handle> {
    /// The owned prop type that this map maps to.
    ///
    /// This is basically what [`Self::get`] returns, but without any borrows of
    /// `self`. For example, `SparseMap<H, T>` sets this to `T`, but its `get`
    /// method returns `&T`. `Ret` is set to `&T` then.
    ///
    /// This is the associated type that should be used for trait bounds, i.e.
    /// if you have a prop map but need to constraint the prop type. Example:
    /// `M: PropMap<FaceHandle>, M::Target: fmt::Display`.
    type Target;

    /// Return type of [`Self::get`]: either `Self::Target` or `&Self::Target`.
    type Ret<'s>: Borrow<Self::Target> where Self: 's;

    /// Returns the property associated with `handle` or `None` if no such
    /// property exists. For technical reasons, the returned value is wrapped
    /// in [`Value`].
    fn get(&self, handle: H) -> Option<Value<Self::Ret<'_>, Self::Target>>;

    /// Returns `true` if there is a property associated with `handle`, `false`
    /// otherwise.
    fn contains_handle(&self, handle: H) -> bool {
        self.get(handle).is_some()
    }

    /// Creates a new prop map that applies the given function to each element
    /// of the original map. Very similar to [`Iterator::map`].
    ///
    ///
    /// # Example
    ///
    /// This example shows a [`SparseMap`] on which `map` is called.
    ///
    /// ```
    /// use lox::{
    ///     FaceHandle,
    ///     prelude::*,
    ///     map::SparseMap,
    /// };
    ///
    /// // Just shortcuts for later
    /// let f0 = FaceHandle::from_usize(0);
    /// let f1 = FaceHandle::from_usize(1);
    /// let f2 = FaceHandle::from_usize(2);
    ///
    /// // Create a sparse map and insert two values
    /// let mut orig = SparseMap::new();
    /// orig.insert(f0, "Anna");
    /// orig.insert(f1, "Peter");
    ///
    /// // Here we create a new map by applying the function that returns the
    /// // length of the string.
    /// let name_lens = orig.map(|s| s.len());
    ///
    /// assert_eq!(orig.get(f0).map(|v| *v), Some("Anna"));
    /// assert_eq!(name_lens.get(f0).map(|v| *v), Some(4));
    ///
    /// assert_eq!(orig.get(f1).map(|v| *v), Some("Peter"));
    /// assert_eq!(name_lens.get(f1).map(|v| *v), Some(5));
    ///
    /// assert_eq!(orig.get(f2), None);
    /// assert_eq!(name_lens.get(f2), None);
    ///
    ///
    /// // But we can also reborrow the value's contents.
    /// let shorter_names = orig.map(|s| &s[1..]);
    ///
    /// assert_eq!(orig.get(f0).map(|v| *v), Some("Anna"));
    /// assert_eq!(shorter_names.get(f0).map(|v| *v), Some("nna"));
    ///
    /// assert_eq!(orig.get(f1).map(|v| *v), Some("Peter"));
    /// assert_eq!(shorter_names.get(f1).map(|v| *v), Some("eter"));
    ///
    /// assert_eq!(orig.get(f2), None);
    /// assert_eq!(shorter_names.get(f2), None);
    /// ```
    fn map<F, TargetT>(&self, f: F) -> adaptors::Map<'_, Self, F>
    where
        Self: Sized,
        F: Fn(Value<Self::Ret<'_>, Self::Target>) -> TargetT,
    {
        adaptors::Map {
            inner: self,
            mapper: f,
        }
    }

    // TODO: filter
}


/// Types that store data associated with handles.
///
/// This trait adds various functionality to the barebone [`PropMap`] interface.
/// This includes an `ops::Index` impl and methods for iteration.
///
/// Use `PropMap` for trait bounds if it is sufficient for you, as it allows
/// your function to be called with more types. This is similar to how you
/// should use `FnOnce` in bounds if it works for you, instead of `FnMut` or
/// `Fn`.
pub trait PropStore<H: Handle>:
    PropMap<H> + ops::Index<H, Output = <Self as PropMap<H>>::Target>
{
    /// Returns a reference to the property associated with `handle` or `None`
    /// if no such property exists.
    fn get_ref(&self, handle: H) -> Option<&Self::Output>;

    /// Returns the number of properties stored in this map.
    fn num_props(&self) -> hsize;

    /// Type returned by [`iter`][Self::iter].
    type Iter<'s>: Iterator<Item = (H, &'s Self::Output)> where Self: 's;

    /// Returns an iterator over immutable references to the values and their
    /// associated handles. The order of this iterator is not specified.
    fn iter(&self) -> Self::Iter<'_>;

    /// Returns an iterator over all handles that have a value associated with
    /// them. The order of the handles is not specified.
    fn handles(&self) -> Handles<Self::Iter<'_>> {
        Handles(self.iter())
    }

    /// Returns an iterator over immutable references to the values. The order
    /// of the handles is not specified.
    fn values(&self) -> Values<Self::Iter<'_>> {
        Values(self.iter())
    }

    /// Returns `true` if there are no properties stored inside `self`
    /// (i.e. `get_ref` will return `None` for all handles).
    fn is_empty(&self) -> bool {
        self.num_props() == 0
    }
}

/// Types that store data (props) associated with handles and allow mutation.
///
/// This adds mutation-related functionality to [`PropStore`].
pub trait PropStoreMut<H: Handle>: Empty + PropStore<H> + ops::IndexMut<H> {
    /// Returns a mutable reference to the property associated with `handle` or
    /// `None` if no such property exists.
    fn get_mut(&mut self, handle: H) -> Option<&mut Self::Output>;

    /// Inserts the given property associated with `handle`. If there was
    /// already a property associated with `handle`, this property is returned.
    fn insert(&mut self, handle: H, prop: Self::Output) -> Option<Self::Output>
    where
        Self::Output: Sized;

    /// Removes the property associated with `handle` and returns it. If no
    /// property was associated with `handle`, nothing is removed and `None` is
    /// returned.
    fn remove(&mut self, handle: H) -> Option<Self::Output>
    where
        Self::Output: Sized;

    /// Removes all properties ([`is_empty`][PropStore::is_empty] will then
    /// return `true`).
    fn clear(&mut self);

    /// Reserves memory for at least `additional` new properties.
    fn reserve(&mut self, additional: hsize);

    /// The type returned by [`iter_mut`][Self::iter_mut].
    type IterMut<'s>: Iterator<Item = (H, &'s mut Self::Output)> where Self: 's;

    /// Returns an iterator over mutable references to the values and their
    /// associated handles. The order of this iterator is not specified.
    fn iter_mut(&mut self) -> Self::IterMut<'_>;

    /// Returns an iterator over mutable references to the values. The order of
    /// the handles is not specified.
    fn values_mut(&mut self) -> ValuesMut<Self::IterMut<'_>> {
        ValuesMut(self.iter_mut())
    }

    /// Returns an empty prop store with pre-allocated memory for `cap` many
    /// properties.
    fn with_capacity(cap: hsize) -> Self
    where
        Self: Sized,
    {
        let mut out = Self::empty();
        out.reserve(cap);
        out
    }
}


// ===========================================================================
// ===== Iterators
// ===========================================================================

/// Iterator over handles of a [`PropStore`]. Returned by
/// [`PropStore::handles`].
#[derive(Debug)]
pub struct Handles<I>(I);

impl<'map, H, T: 'map, I: Iterator<Item = (H, &'map T)>> Iterator for Handles<I> {
    type Item = H;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(h, _)| h)
    }
}

/// Iterator over immutable references to props of a [`PropStore`]. Returned by
/// [`PropStore::values`].
#[derive(Debug)]
pub struct Values<I>(I);

impl<'map, H, T: 'map, I: Iterator<Item = (H, &'map T)>> Iterator for Values<I> {
    type Item = &'map T;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(_, v)| v)
    }
}

/// Iterator over mutable references to props of a [`PropStoreMut`]. Returned by
/// [`PropStoreMut::values_mut`].
#[derive(Debug)]
pub struct ValuesMut<I>(I);

impl<'map, H, T: 'map, I: Iterator<Item = (H, &'map mut T)>> Iterator for ValuesMut<I> {
    type Item = &'map mut T;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(_, v)| v)
    }
}


// ===========================================================================
// ===== `Value` helper
// ===========================================================================

/// Wrapper for the value returned by [`PropMap::get`].
///
/// For why this is necessary, see the documentation of [`PropMap`].
pub struct Value<R, T>(R, PhantomData<T>);

impl<R: Borrow<T>, T> From<R> for Value<R, T> {
    fn from(value: R) -> Self {
        Self(value, PhantomData)
    }
}

impl<R: Borrow<T>, T> Value<R, T> {
    /// Returns the inner value.
    pub fn into_inner(self) -> R {
        self.0
    }
}

impl<R: Borrow<T>, T> ops::Deref for Value<R, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.borrow()
    }
}

impl<R: Borrow<T>, T: fmt::Debug> fmt::Debug for Value<R, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.borrow().fmt(f)
    }
}

impl<R: Borrow<T>, T: PartialEq> PartialEq for Value<R, T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.borrow().eq(other.0.borrow())
    }
}

impl<R: Borrow<T>, T: Eq> Eq for Value<R, T> {}

impl<R: Borrow<T>, T: PartialEq> PartialEq<T> for Value<R, T> {
    fn eq(&self, other: &T) -> bool {
        self.0.borrow().eq(other)
    }
}
