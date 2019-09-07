use std::{
    fmt,
    iter::{FromIterator, FusedIterator},
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use stable_vec::{
    StableVec,
    core::DefaultCore,
    iter::{
        Indices, Iter as SvIter, IterMut as SvIterMut, Values as SvValues,
        ValuesMut as SvValuesMut,
    },
};

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
/// you should probably use [`SparseMap`][SparseMap] or [`TinyMap`][TinyMap]
/// instead.
///
/// [SparseMap]: crate::map::SparseMap
/// [TinyMap]: crate::map::TinyMap
///
/// # Example
///
/// ```
/// use lox::{
///     FaceHandle,
///     handle::Handle,
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

    // TODO: the following iterator methods are not necessary anymore once
    // `PropStore` can use statically dispatched iterators.

    pub fn iter(&self) -> Iter<'_, H, T> {
        Iter::new(self)
    }
    pub fn handles(&self) -> Handles<'_, H, T> {
        Handles::new(self)
    }
    pub fn values(&self) -> Values<'_, H, T> {
        Values::new(self)
    }
    pub fn iter_mut(&mut self) -> IterMut<'_, H, T> {
        IterMut::new(self)
    }
    pub fn values_mut(&mut self) -> ValuesMut<'_, H, T> {
        ValuesMut::new(self)
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
    type Marker = boo::Borrowed;

    fn get(&self, handle: H) -> Option<boo::Wrap<'_, Self::Target, Self::Marker>> {
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

    fn iter(&self) -> Box<dyn Iterator<Item = (H, &Self::Output)> + '_> {
        Box::new(self.iter())
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

    fn iter_mut(&mut self) -> Box<dyn Iterator<Item = (H, &mut Self::Output)> + '_> {
        Box::new(self.iter_mut())
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


// ===== Iterator wrappers =======================================================================

/// This macro generates an iterator wrapper. The usage is kinda awkward, but
/// this way we can avoid duplicate code.
macro_rules! gen_iter_wrapper {
    (
        $name:ident, $sv_name:ident, $iter_method:ident, [$($mutable:ident)?], [$($clone:ident)?],
        |$lt:tt, $h:ident, $t:ident| $item:ty,
        [$($mapping:tt)*] $(,)?
    ) => {
        #[derive(Debug, $($clone)?)]
        pub struct $name<$lt, $h: Handle, $t> {
            iter: $sv_name<$lt, $t, DefaultCore<$t>>,
            _dummy: PhantomData<&$lt $h>,
        }

        impl<$lt, $h: Handle, $t> $name<$lt, $h, $t> {
            fn new(map: &$lt $($mutable)? DenseMap<$h, $t>) -> Self {
                Self {
                    iter: map.vec.$iter_method(),
                    _dummy: PhantomData,
                }
            }
        }

        impl<$lt, $h: Handle, $t> Iterator for $name<$lt, $h, $t> {
            type Item = $item;
            fn next(&mut self) -> Option<Self::Item> {
                self.iter.next() $($mapping)*
            }

            fn size_hint(&self) -> (usize, Option<usize>) {
                self.iter.size_hint()
            }

            fn count(self) -> usize {
                self.iter.count()
            }

            fn last(mut self) -> Option<Self::Item> {
                self.next_back()
            }
        }

        impl<$lt, $h: Handle, $t> DoubleEndedIterator for $name<$lt, $h, $t> {
            fn next_back(&mut self) -> Option<Self::Item> {
                self.iter.next_back() $($mapping)*
            }
        }

        impl<$lt, $h: Handle, $t> ExactSizeIterator for $name<$lt, $h, $t> {
            fn len(&self) -> usize {
                self.iter.len()
            }
        }

        impl<$lt, $h: Handle, $t> FusedIterator for $name<$lt, $h, $t> {}
    };
}

gen_iter_wrapper!(
    Iter, SvIter, iter, [], [Clone],
    |'map, H, T| (H, &'map T),
    [.map(|(i, e)| (H::from_usize(i), e))],
);
gen_iter_wrapper!(
    IterMut, SvIterMut, iter_mut, [mut], [],
    |'map, H, T| (H, &'map mut T),
    [.map(|(i, e)| (H::from_usize(i), e))],
);
gen_iter_wrapper!(Handles, Indices, indices, [], [Clone], |'map, H, T| H, [.map(H::from_usize)]);
gen_iter_wrapper!(Values, SvValues, values, [], [Clone], |'map, H, T| &'map T, []);
gen_iter_wrapper!(ValuesMut, SvValuesMut, values_mut, [mut], [], |'map, H, T| &'map mut T, []);


// ===== Tests ===================================================================================

#[cfg(test)]
mod tests {
    use super::*;

    gen_tests_for_store_impl!(DenseMap);
}
