use std::{
    fmt,
    iter::FromIterator,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use stable_vec::{Indices, StableVec, DefaultCore};

use crate::{
    handle::{hsize, Handle},
    traits::Empty,
};
use super::{
    PropMap, PropStore, PropStoreMut,
    boo,
};


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
///   just an array lookup. A hash map would need to calculate the hash and do
///   more work to find an element for a given handle.
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
/// you should probably use [`HashMap`][crate::map::HashMap] or `TinyMap`
/// instead (TODO: add link to tiny map once implemented).
///
///
/// # Example
///
/// ```
/// use lox::{
///     FaceHandle,
///     handle::Handle,
///     map::{PropStore, PropStoreMut, VecMap},
/// };
///
///
/// let mut map = VecMap::new();
///
/// let f0 = FaceHandle::from_usize(0);
/// assert_eq!(map.get_ref(f0), None);
/// map.insert(f0, "bob");
/// assert_eq!(map.get_ref(f0), Some(&"bob"));
///
/// // Note that after this insert operation, the `VecMap` has allocated memory
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
pub struct VecMap<H: Handle, T> {
    vec: StableVec<T>,
    _dummy: PhantomData<H>,
}

impl<H: Handle, T> VecMap<H, T> {
    /// Creates an empty `VecMap`.
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
        H::from_usize(self.vec.next_index())
    }

    pub(crate) fn last_handle(&self) -> Option<H> {
        self.vec.find_last_index().map(H::from_usize)
    }

    pub fn num_elements(&self) -> hsize {
        self.vec.num_elements() as hsize
    }

    pub fn handles(&self) -> Handles<H, T> {
        Handles {
            iter: self.vec.indices(),
            _dummy: PhantomData,
        }
    }

    pub fn values(&self) -> Values<'_, T> {
        Values {
            iter: self.vec.iter(),
        }
    }

    pub fn values_mut(&mut self) -> ValuesMut<'_, T> {
        ValuesMut {
            iter: self.vec.iter_mut(),
        }
    }
}

impl<H: Handle, T: Clone> VecMap<H, T> {
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

impl<H: Handle, T> PropMap<H> for VecMap<H, T> {
    type Target = T;
    type Marker = boo::Borrowed;

    fn get(&self, handle: H) -> Option<boo::Wrap<'_, Self::Target, Self::Marker>> {
        self.get_ref(handle).map(Into::into)
    }

    fn contains_handle(&self, handle: H) -> bool {
        self.vec.has_element_at(handle.to_usize())
    }
}

impl<H: Handle, T> Index<H> for VecMap<H, T> {
    type Output = T;
    fn index(&self, handle: H) -> &Self::Output {
        match self.get_ref(handle) {
            None => panic!("no property found for handle '{:?}'", handle),
            Some(r) => r,
        }
    }
}

impl<H: Handle, T> PropStore<H> for VecMap<H, T> {
    fn get_ref(&self, handle: H) -> Option<&Self::Output> {
        self.vec.get(handle.to_usize())
    }

    fn num_props(&self) -> hsize {
        self.vec.num_elements() as hsize
    }

    fn handles<'a>(&'a self) -> Box<dyn Iterator<Item = H> + 'a> {
        Box::new(self.handles())
    }
}

impl<H: Handle, T> IndexMut<H> for VecMap<H, T> {
    fn index_mut(&mut self, handle: H) -> &mut Self::Output {
        match self.get_mut(handle) {
            None => panic!("no property found for handle '{:?}'", handle),
            Some(r) => r,
        }
    }
}

impl<H: Handle, T> Empty for VecMap<H, T> {
    fn empty() -> Self {
        Self::new()
    }
}

impl<H: Handle, T> PropStoreMut<H> for VecMap<H, T> {
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
}

impl<H: Handle, T: fmt::Debug> fmt::Debug for VecMap<H, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map()
            .entries(self.vec.indices().map(|k| (H::from_usize(k), &self.vec[k])))
            .finish()
    }
}

impl<H: Handle, T> Extend<(H, T)> for VecMap<H, T> {
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
        self.reserve(cap as u32);

        for (handle, value) in iter {
            self.insert(handle, value);
        }
    }
}

impl<H: Handle, T> FromIterator<(H, T)> for VecMap<H, T> {
    fn from_iter<I: IntoIterator<Item = (H, T)>>(iter: I) -> Self {
        let mut out = Self::empty();
        out.extend(iter);
        out
    }
}


#[derive(Debug)]
pub struct Handles<'map, H: Handle, T> {
    iter: Indices<'map, T, DefaultCore<T>>,
    _dummy: PhantomData<H>,
}

impl<'map, H: Handle, T> Iterator for Handles<'map, H, T> {
    type Item = H;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(H::from_usize)
    }
}

#[derive(Debug)]
pub struct Values<'map, T> {
    iter: stable_vec::Iter<'map, T, DefaultCore<T>>,
}

impl<'map, T> Iterator for Values<'map, T> {
    type Item = &'map T;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

#[derive(Debug)]
pub struct ValuesMut<'map, T> {
    iter: stable_vec::IterMut<'map, T, DefaultCore<T>>,
}

impl<'map, T> Iterator for ValuesMut<'map, T> {
    type Item = &'map mut T;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    gen_tests_for_store_impl!(VecMap);
}
