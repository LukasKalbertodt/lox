use std::{
    fmt,
    mem,
    ops::{Index, IndexMut},
};

use smallvec::SmallVec;

use crate::{
    handle::{hsize, Handle},
    util::Empty,
};
use super::{PropMap, PropStore, PropStoreMut, Value};


/// A property map optimized to hold very few elements.
///
/// This map is implemented with a simple vector; element lookup is a simple
/// linear search. Thus almost all operations have a runtime of O(n). This
/// makes the map extremely bad for holding more than a couple of elements.
///
/// But when you only want to store very few properties, this map can be
/// faster. In particular, this map stores a few elements inline (without heap
/// allocation) -- which can make a big difference in speed. Currently up to 4
/// elements are stored inline, but this number might change in the future.
///
/// As with most maps, the main way to use this type is via the map traits. See
/// the [module documentation][super] for more information.
/// - [`PropMap`][super::PropMap]
/// - [`PropStore`][super::PropStore]
/// - [`PropStoreMut`][super::PropStoreMut]
///
/// # TODO:
/// - examples
/// - when const generics are implemented, let users decide how many elements
///   are stored inline
#[derive(Clone)]
pub struct TinyMap<H: Handle, T> {
    vec: SmallVec<[(H, T); 4]>,
}

impl<H: Handle, T> TinyMap<H, T> {
    /// Creates an empty `TinyMap`.
    fn new() -> Self {
        Self {
            vec: SmallVec::new()
        }
    }
}

impl<H: Handle, T> PropMap<H> for TinyMap<H, T> {
    type Target = T;
    type Ret<'s> = &'s Self::Target where Self::Target: 's;

    fn get(&self, handle: H) -> Option<Value<Self::Ret<'_>, Self::Target>> {
        self.get_ref(handle).map(Into::into)
    }
}

impl<H: Handle, T> Index<H> for TinyMap<H, T> {
    type Output = T;
    fn index(&self, handle: H) -> &Self::Output {
        match self.get_ref(handle) {
            None => panic!("no property found for handle '{:?}'", handle),
            Some(r) => r,
        }
    }
}

impl<H: Handle, T> PropStore<H> for TinyMap<H, T> {
    fn get_ref(&self, query: H) -> Option<&Self::Output> {
        self.vec.iter()
            .find(|(handle, _)| *handle == query)
            .map(|(_, value)| value)
    }

    fn num_props(&self) -> hsize {
        self.vec.len() as hsize
    }

    fn iter(&self) -> Box<dyn Iterator<Item = (H, &Self::Output)> + '_> {
        Box::new(self.vec.iter().map(|(handle, v)| (*handle, v)))
    }
}

impl<H: Handle, T> IndexMut<H> for TinyMap<H, T> {
    fn index_mut(&mut self, handle: H) -> &mut Self::Output {
        match self.get_mut(handle) {
            None => panic!("no property found for handle '{:?}'", handle),
            Some(r) => r,
        }
    }
}

impl<H: Handle, T> Empty for TinyMap<H, T> {
    fn empty() -> Self {
        Self::new()
    }
}

impl<H: Handle, T> PropStoreMut<H> for TinyMap<H, T> {
    fn get_mut(&mut self, query: H) -> Option<&mut Self::Output> {
        self.vec.iter_mut()
            .find(|(handle, _)| *handle == query)
            .map(|(_, value)| value)
    }

    fn insert(&mut self, handle: H, mut elem: Self::Output) -> Option<Self::Output> {
        if let Some(pos) = self.vec.iter().position(|(h, _)| *h == handle) {
            mem::swap(&mut self.vec[pos].1, &mut elem);
            Some(elem)
        } else {
            self.vec.push((handle, elem));
            None
        }
    }

    fn remove(&mut self, handle: H) -> Option<Self::Output> {
        if let Some(pos) = self.vec.iter().position(|(h, _)| *h == handle) {
            Some(self.vec.swap_remove(pos).1)
        } else {
            None
        }
    }

    fn clear(&mut self) {
        self.vec.clear()
    }

    fn reserve(&mut self, additional: hsize) {
        self.vec.reserve(additional as usize);
    }

    fn iter_mut(&mut self) -> Box<dyn Iterator<Item = (H, &mut Self::Output)> + '_> {
        Box::new(self.vec.iter_mut().map(|(handle, v)| (*handle, v)))
    }
}

impl<H: Handle, T: fmt::Debug> fmt::Debug for TinyMap<H, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map()
            .entries(self.vec.iter().map(|(h, v)| (h, v)))
            .finish()
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    gen_tests_for_store_impl!(TinyMap);
}
