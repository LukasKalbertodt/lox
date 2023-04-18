use std::{
    fmt,
    iter::FromIterator,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use stable_vec::{
    StableVec,
    core::DefaultCore,
};

use crate::{
    hsize,
    prelude::*,
};
use super::{PropMap, PropStore, PropStoreMut, Value, util::gen_mapped_iter};


/// A property map that uses a simple contiguous vector to store the
/// properties.
///
///
/// # Memory requirements and use cases
///
/// This data structure's memory requirement doesn't grow with the number of
/// elements stored inside this map, but rather with the highest handle ID. The
/// handle is simply used as an index into the underlying vector. This has two
/// important consequences:
///
/// - **Good**: this map usually has the best access times since it just is
///   just an array lookup. A hash map (used in [`SparseMap`][SparseMap]) would
///   need to calculate the hash and do more work to find an element for a
///   given handle.
/// - **Bad**: if you don't pay attention, you could waste a lot of memory with
///   this map and subsequently lose the speed advantage.
///
/// Most sources of handles (like all mesh data structures in this libary) will
/// produce handles with sequentially increasing IDs. So if you add three
/// vertices to a mesh, the handles of those vertices will have the IDs 0, 1
/// and 2. If you have a source of handles that works differently (e.g. by
/// using random number as handle IDs), this map is absolutely not useful for
/// you.
///
/// However, **if you have a handle source with sequential IDs and you want to
/// associated data with (almost) all of those handles, this map is the best
/// choice.** If you only want to associated data with some of those handles,
/// you should probably use [`SparseMap`][SparseMap] instead.
///
/// [SparseMap]: super::SparseMap
///
/// # Example
///
/// ```
/// use lox::{
///     FaceHandle, Handle,
///     map::{PropStore, PropStoreMut, DenseMap},
/// };
///
///
/// let mut map = DenseMap::new();
///
/// let f0 = FaceHandle::from_usize(0);
/// assert_eq!(map.get_ref(f0), None);
/// map.insert(f0, "bob");
/// assert_eq!(map.get_ref(f0), Some(&"bob"));
///
/// // Note that after this insert operation, the `DenseMap` has allocated memory
/// // for 6 elements (2 of which are used).
/// let f5 = FaceHandle::from_usize(5);
/// map.insert(f5, "lena");
/// assert_eq!(map.get_ref(f5), Some(&"lena"));
/// ```
///
/// TODO: more examples:
/// - generating some property for all vertices of a mesh
/// - iterator stuff
#[derive(Clone)]
pub struct DenseMap<H: Handle, T> {
    vec: StableVec<T>,
    _dummy: PhantomData<H>,
}

impl<H: Handle, T> DenseMap<H, T> {
    /// Creates an empty `DenseMap`.
    pub fn new() -> Self {
        Self {
            vec: StableVec::new(),
            _dummy: PhantomData,
        }
    }

    pub fn push(&mut self, elem: T) -> H {
        H::from_usize(self.vec.push(elem))
    }

    pub(crate) fn next_push_handle(&self) -> H {
        H::from_usize(self.vec.next_push_index())
    }

    pub(crate) fn last_handle(&self) -> Option<H> {
        self.vec.find_last_index().map(H::from_usize)
    }

    pub fn num_elements(&self) -> hsize {
        self.vec.num_elements() as hsize
    }

    pub unsafe fn get_unchecked(&self, handle: H) -> &T {
        self.vec.get_unchecked(handle.to_usize())
    }

    pub unsafe fn get_unchecked_mut(&mut self, handle: H) -> &mut T {
        self.vec.get_unchecked_mut(handle.to_usize())
    }
}

impl<H: Handle, T: Clone> DenseMap<H, T> {
    pub fn from_elem(elem: T, count: usize) -> Self {
        let mut v = StableVec::with_capacity(count);
        for _ in 0..count {
            v.push(elem.clone());
        }

        Self {
            vec: v,
            _dummy: PhantomData,
        }
    }
}

impl<H: Handle, T> PropMap<H> for DenseMap<H, T> {
    type Target = T;
    type Ret<'s> = &'s Self::Target where Self::Target: 's;

    fn get(&self, handle: H) -> Option<Value<Self::Ret<'_>, Self::Target>> {
        self.get_ref(handle).map(Into::into)
    }

    fn contains_handle(&self, handle: H) -> bool {
        self.vec.has_element_at(handle.to_usize())
    }
}

impl<H: Handle, T> Index<H> for DenseMap<H, T> {
    type Output = T;
    fn index(&self, handle: H) -> &Self::Output {
        match self.get_ref(handle) {
            None => panic!("no property found for handle '{:?}'", handle),
            Some(r) => r,
        }
    }
}

impl<H: Handle, T> PropStore<H> for DenseMap<H, T> {
    fn get_ref(&self, handle: H) -> Option<&Self::Output> {
        self.vec.get(handle.to_usize())
    }

    fn num_props(&self) -> hsize {
        self.vec.num_elements() as hsize
    }

    type Iter<'s> = Iter<'s, H, T> where Self: 's;
    fn iter(&self) -> Self::Iter<'_> {
        Iter(self.vec.iter(), PhantomData)
    }
}

impl<H: Handle, T> IndexMut<H> for DenseMap<H, T> {
    fn index_mut(&mut self, handle: H) -> &mut Self::Output {
        match self.get_mut(handle) {
            None => panic!("no property found for handle '{:?}'", handle),
            Some(r) => r,
        }
    }
}

impl<H: Handle, T> Empty for DenseMap<H, T> {
    fn empty() -> Self {
        Self::new()
    }
}

impl<H: Handle, T> PropStoreMut<H> for DenseMap<H, T> {
    fn get_mut(&mut self, handle: H) -> Option<&mut Self::Output> {
        self.vec.get_mut(handle.to_usize())
    }

    fn insert(&mut self, handle: H, elem: Self::Output) -> Option<Self::Output> {
        let idx = handle.to_usize();
        self.vec.reserve_for(idx);
        self.vec.insert(idx, elem)
    }

    fn remove(&mut self, handle: H) -> Option<Self::Output> {
        let idx = handle.to_usize();
        if idx >= self.vec.capacity() {
            return None;
        }

        self.vec.remove(idx)
    }

    fn clear(&mut self) {
        self.vec.clear()
    }

    fn reserve(&mut self, additional: hsize) {
        self.vec.reserve(additional as usize);
    }

    type IterMut<'s> = IterMut<'s, H, T> where Self: 's;
    fn iter_mut(&mut self) -> Self::IterMut<'_> {
        IterMut(self.vec.iter_mut(), PhantomData)
    }
}

impl<H: Handle, T: fmt::Debug> fmt::Debug for DenseMap<H, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map()
            .entries(self.vec.indices().map(|k| (H::from_usize(k), &self.vec[k])))
            .finish()
    }
}

impl<H: Handle, T> Extend<(H, T)> for DenseMap<H, T> {
    fn extend<I: IntoIterator<Item = (H, T)>>(&mut self, iter: I) {
        // We use the same strategy as the std `HashMap`: since keys may be
        // already present or show multiple times in the iterator, we don't
        // necessarily want to reserve too much.
        let iter = iter.into_iter();
        let cap = if self.is_empty() {
            // Just reserve the full lower bound
            iter.size_hint().0
        } else {
            // If we are not empty, we just reserve half. That way we
            // reallocate at most twice.
            (iter.size_hint().0 + 1) / 2
        };
        self.reserve(cap as hsize);

        for (handle, value) in iter {
            self.insert(handle, value);
        }
    }
}

impl<H: Handle, T> FromIterator<(H, T)> for DenseMap<H, T> {
    fn from_iter<I: IntoIterator<Item = (H, T)>>(iter: I) -> Self {
        let mut out = Self::empty();
        out.extend(iter);
        out
    }
}


gen_mapped_iter!(
    Iter<'a, H, T>(stable_vec::iter::Iter<'a, T, DefaultCore<T>>, PhantomData<H>);
    mut_token: [],
    extra_derives: [Clone],
    mapping: |(i, v)| (H::from_usize(i), v),
    double_ended: true,
);
gen_mapped_iter!(
    IterMut<'a, H, T>(stable_vec::iter::IterMut<'a, T, DefaultCore<T>>, PhantomData<H>);
    mut_token: [mut],
    extra_derives: [],
    mapping: |(i, v)| (H::from_usize(i), v),
    double_ended: true,
);


// ===== Tests ===================================================================================

#[cfg(test)]
mod tests {
    use super::*;

    gen_tests_for_store_impl!(DenseMap);
}
