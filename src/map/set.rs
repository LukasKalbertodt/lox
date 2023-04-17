//! Using maps as sets (i.e. without values, but only handles).
//!
//! The main type in this module is [`Set`] which is a wrapper around some map
//! type. It offers a slightly nicer API than using map types directly. This
//! module also contains a bunch of type aliases which you should use.

use std::{
    marker::PhantomData,
};

use crate::{
    hsize,
    prelude::*,
};
use super::{
    DenseMap, Handles, PropMap, PropStore, PropStoreMut, SparseMap, TinyMap
};

/// A handle set that uses a bit vector to store handles.
///
/// See [`DenseMap`] for more information on memory requirements and speed.
pub type DenseSet<H> = Set<H, DenseMap<H, ()>>;

/// A handle set that uses hash set store handles.
///
/// See [`SparseMap`] for more information on memory requirements and speed.
pub type SparseSet<H> = Set<H, SparseMap<H, ()>>;

/// A handle set optimized to hold very few handles.
///
/// See [`TinyMap`] for more information on memory requirements and speed.
pub type TinySet<H> = Set<H, TinyMap<H, ()>>;


/// A *set* of handles (basically a map with `()` values).
///
/// This is actually just a wrapper around a map with `()` elements, but offers
/// a slightly better API than using maps directly.
#[derive(Debug, Clone, Empty)]
pub struct Set<H: Handle, M: Empty>{
    map: M,
    _dummy: PhantomData<H>,
}

impl<H: Handle, M: Empty + PropMap<H, Target = ()>> Set<H, M> {
    /// Returns `true` if the given `handle` is part of this set.
    pub fn contains_handle(&self, handle: H) -> bool {
        self.map.contains_handle(handle)
    }
}

impl<H: Handle, M: Empty + PropStore<H, Target = ()>> Set<H, M> {
    /// Returns the number of elements in this set.
    pub fn num_elements(&self) -> hsize {
        self.map.num_props()
    }

    /// Returns an iterator over all handles in this set. The order of the
    /// handles is not specified.
    pub fn handles(&self) -> Handles<'_, H, ()> {
        self.map.handles()
    }

    /// Returns `true` if this set is empty.
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

impl<H: Handle, M: Empty + PropStoreMut<H, Target = ()>> Set<H, M> {
    /// Creates an empty set with memory for `cap` many handles.
    pub fn with_capacity(cap: hsize) -> Self
    where
        Self: Sized,
    {
        let mut out = Self::empty();
        out.reserve(cap);
        out
    }

    /// Inserts the given `handle` into the set. Returns `true` if the handle
    /// was already in the set before, `false` otherwise.
    pub fn insert(&mut self, handle: H) -> bool {
        self.map.insert(handle, ()).is_some()
    }

    /// Removes the given `handle` from this set. If the handle is not in this
    /// set, `false` is returned and the set is not modified. Else `true` is
    /// returned.
    pub fn remove(&mut self, handle: H) -> bool {
        self.map.remove(handle).is_some()
    }

    /// Removes all handles from this set. Afterwards the set is empty.
    pub fn clear(&mut self) {
        self.map.clear()
    }

    /// Reserves memory for at least `additional` new handles.
    pub fn reserve(&mut self, additional: hsize) {
        self.map.reserve(additional)
    }
}
