//! ...


use std::{
    ops,
};

use crate::{
    handle::{hsize, Handle},
    traits::Empty,
};


#[cfg(test)]
#[macro_use]
mod tests;

pub mod adaptors;
pub mod aliases;
pub mod boo;
mod dense;
mod fn_map;
pub mod set;
mod sparse;
mod special_maps;
mod tiny;

pub use self::{
    aliases::*,
    fn_map::FnMap,
    sparse::SparseMap,
    special_maps::{ConstMap, EmptyMap},
    tiny::TinyMap,
    dense::DenseMap,
};



/// A mapping from a handle to some data (property).
///
/// This is a bare minimal trait representing all types that can map a handle
/// to optional data, called property. The returned property can be owned or
/// borrowed from `self`.
///
///
/// # Completeness
///
/// In many contexts, a `PropMap` is required to return `Some(_)` values for
/// a specific set of handles. For example:
///
/// ```ignore
/// # // TODO: use proper mesh traits here and possibly make this compile!
/// fn print_face_props<'s>(mesh: ..., map: impl PropMap<'s, FaceHandle>) {
///     ...
/// }
/// ```
///
/// This function probably requires that `map` contains `Some(_)` data for all
/// face handles of `mesh`. This is stated as: "`map` needs to be complete
/// regarding `mesh`".
///
///
/// # TODO
///
/// - Example how to use `PropMap`s
/// - Example how to implement `PropMap`
/// - Explain strange `boo` thingies
/// - Trait alias
pub trait PropMap<H: Handle> {
    type Target;
    type Marker: boo::Marker;

    /// Returns the property associated with `handle` or `None` if no such
    /// property exists.
    fn get(&self, handle: H) -> Option<boo::Wrap<'_, Self::Target, Self::Marker>>;

    /// Returns `true` if there is a property associated with `handle`, `false`
    /// otherwise.
    fn contains_handle(&self, handle: H) -> bool {
        self.get(handle).is_some()
    }

    /// Creates a new prop map that applies the given function to each element
    /// of the original map. Very similar to `Iterator::map`.
    ///
    /// This method only works when the given closure returns a new property by
    /// value. If you want to instead borrow from the old property, use
    /// [`PropMap::map_ref`] instead.
    ///
    /// This adaptor doesn't change for which handles a value is present. So
    /// `contains_handle` always returns the same result as on the original
    /// map.
    ///
    /// # Example
    ///
    /// This example shows a [`SparseMap`] on which `map` is called. The
    /// element's borrowed state and type is changed (from `&str` to `usize`).
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
    /// // Here we create a new map by applying the function that simply
    /// // returns the length of the string.
    /// let mapped = orig.map_value(|s| s.len().into());
    ///
    ///
    /// assert_eq!(orig.get(f0).map(|v| *v), Some("Anna"));
    /// assert_eq!(mapped.get(f0).map(|v| *v), Some(4));
    ///
    /// assert_eq!(orig.get(f1).map(|v| *v), Some("Peter"));
    /// assert_eq!(mapped.get(f1).map(|v| *v), Some(5));
    ///
    /// assert_eq!(orig.get(f2), None);
    /// assert_eq!(mapped.get(f2), None);
    /// ```
    ///
    /// In the above example, the mapping function received a borrowed property
    /// and returns a new property by value. But this method also works with
    /// maps that return the original property by value.
    ///
    /// The closure always receives a `Wrap<...>`. As this type implements
    /// `Deref`, most operations just work. However, if you need access to the
    /// owned value, you need to use `into_inner()`. This is shown here:
    ///
    /// ```
    /// use lox::{
    ///     FaceHandle,
    ///     prelude::*,
    ///     map::FnMap,
    /// };
    ///
    /// // The `FnMap` returns its properties by value
    /// let orig = FnMap(|_| Some(vec![1, 2, 3]));
    ///
    /// // If you just need to access the property by immutable reference, you
    /// // can simply do that thanks to deref coercions.
    /// let mapped = orig.map_value(|v| v.len());
    /// assert_eq!(mapped.get(FaceHandle::from_usize(0)).map(|x| *x), Some(3usize));
    ///
    /// // However, sometimes you really need the owned value. In that case,
    /// // you need to call `into_inner` on the `Wrap` that was passed in.
    /// let mapped = orig.map_value(|v| v.into_inner().into_boxed_slice());
    /// assert_eq!(
    ///     mapped.get(FaceHandle::from_usize(0)).as_ref().map(|x| &**x),
    ///     Some(&vec![1, 2, 3].into_boxed_slice()),
    /// );
    /// ```
    fn map_value<F, TargetT>(&self, f: F) -> adaptors::Mapper<'_, Self, F>
    where
        Self: Sized,
        F: Fn(boo::Wrap<'_, Self::Target, Self::Marker>) -> TargetT,
    {
        adaptors::Mapper {
            inner: self,
            mapper: f,
        }
    }

    /// Creates a new prop map that applies the given function to each element
    /// of the original map. Similar to [`PropMap::map_value`].
    ///
    /// The closure given to this function has to borrow from the original
    /// property. If you want to return an owned value instead, use
    /// [`PropMap::map_value`]. Otherwise, this method works exactly like
    /// `map_value`.
    ///
    ///
    /// # Example
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
    ///
    /// // Create a sparse map and insert two values
    /// let mut orig = SparseMap::new();
    /// orig.insert(f0, ["a".to_string(), "b".to_string()]);
    /// orig.insert(f1, ["x".to_string(), "y".to_string()]);
    ///
    /// // Map the value by taking the first element.
    /// let mapped = orig.map_ref(|s| &s[0]);
    ///
    /// assert_eq!(mapped.get(f0).map(|v| v.into_inner().as_str()), Some("a"));
    /// assert_eq!(mapped.get(f1).map(|v| v.into_inner().as_str()), Some("x"));
    /// ```
    fn map_ref<F, TargetT>(&self, f: F) -> adaptors::RefMapper<'_, Self, F>
    where
        Self: Sized + PropMap<H, Marker = boo::Borrowed>,
        F: Fn(&Self::Target) -> &TargetT,
    {
        adaptors::RefMapper {
            inner: self,
            mapper: f,
        }
    }
}


/// A type that stores data associated with handles.
///
/// This type is similar to `PropMap`, but has more restrictions/features.
/// `PropMap::get` can return owned or borrowed values, whereas
/// `PropStore::get_ref` has to return a borrowed value. It also has
/// `ops::Index` as super trait, which requires the same. Furthermore, a
/// `PropStore` needs to be able to iterate over all of its data.
///
///
/// # Type level relationship between `PropStore` and `PropMap`
///
/// `PropStore` is a subtype of `PropMap`, as in: every `PropStore` is also a
/// `PropMap`. It would be really nice to implement `PropMap` for all types
/// that implement `PropStore` (at least provide a default implementation). But
/// this is currently problematic due to (a) coherence and (b) specialization
/// being unstable. TODO: try this in the future again.
///
///
/// # TODO
///
/// - Example how to use `PropStore`s
/// - Example how to implement `PropStore`
/// - When to use `PropMap` and when to use `PropStore`
/// - Trait alias
pub trait PropStore<H: Handle>:
    PropMap<H> + ops::Index<H, Output = <Self as PropMap<H>>::Target>
{
    /// Returns a reference to the property associated with `handle` or `None`
    /// if no such property exists.
    fn get_ref(&self, handle: H) -> Option<&Self::Output>;

    /// Returns the number of properties stored in this map.
    fn num_props(&self) -> hsize;

    /// Returns an iterator over immutable references to the values and their
    /// associated handles. The order of this iterator is not specified.
    ///
    /// TODO: improve with GATs
    fn iter(&self) -> Box<dyn Iterator<Item = (H, &Self::Output)> + '_>;

    /// Returns an iterator over all handles that have a value associated with
    /// them. The order of the handles is not specified.
    fn handles(&self) -> Handles<'_, H, Self::Output> {
        Handles(self.iter())
    }

    /// Returns an iterator over immutable references to the values. The order
    /// of the handles is not specified.
    fn values(&self) -> Values<'_, H, Self::Output> {
        Values(self.iter())
    }

    fn is_empty(&self) -> bool {
        self.num_props() == 0
    }
}

// TODO: maybe combine this with `PropStore`?
/// ...
pub trait PropStoreMut<H: Handle>: Empty + PropStore<H> + ops::IndexMut<H> {
    /// Returns a mutable reference to the property associated with `handle` or
    /// `None` if no such property exists.
    fn get_mut(&mut self, handle: H) -> Option<&mut Self::Output>;

    /// Inserts the given property associated with `handle`. If there was
    /// already a property associated with `handle`, this property is returnd.
    fn insert(&mut self, handle: H, prop: Self::Output) -> Option<Self::Output>
    where
        Self::Output: Sized;

    /// Removes the property associated with `handle` and returns it. If no
    /// property was associated with `handle`, nothing is removed and `None` is
    /// returned.
    fn remove(&mut self, handle: H) -> Option<Self::Output>
    where
        Self::Output: Sized;

    /// Removes all properties so that all `contains_handle()` returns `false`
    /// for all handles.
    fn clear(&mut self);

    /// Reserves memory for at least `additional` new properties.
    fn reserve(&mut self, additional: hsize);

    /// Returns an iterator over mutable references to the values and their
    /// associated handles. The order of this iterator is not specified.
    ///
    /// TODO: improve with GATs
    fn iter_mut(&mut self) -> Box<dyn Iterator<Item = (H, &mut Self::Output)> + '_>;

    /// Returns an iterator over mutable references to the values. The order of
    /// the handles is not specified.
    fn values_mut(&mut self) -> ValuesMut<'_, H, Self::Output> {
        ValuesMut(self.iter_mut())
    }

    fn with_capacity(cap: hsize) -> Self
    where
        Self: Sized,
    {
        let mut out = Self::empty();
        out.reserve(cap);
        out
    }
}


#[allow(missing_debug_implementations)] // TODO
pub struct Handles<'map, H, T>(Box<dyn Iterator<Item = (H, &'map T)> + 'map>);

impl<'map, H, T> Iterator for Handles<'map, H, T> {
    type Item = H;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(h, _)| h)
    }
}

#[allow(missing_debug_implementations)] // TODO
pub struct Values<'map, H, T>(Box<dyn Iterator<Item = (H, &'map T)> + 'map>);

impl<'map, H, T> Iterator for Values<'map, H, T> {
    type Item = &'map T;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(_, v)| v)
    }
}

#[allow(missing_debug_implementations)] // TODO
pub struct ValuesMut<'map, H, T>(Box<dyn Iterator<Item = (H, &'map mut T)> + 'map>);

impl<'map, H, T> Iterator for ValuesMut<'map, H, T> {
    type Item = &'map mut T;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(_, v)| v)
    }
}
