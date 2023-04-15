use std::{
    collections::HashMap,
    hash::Hash,
    ops::{Index, IndexMut},
};

use crate::{
    handle::{hsize, Handle},
    traits::Empty,
};
use super::{PropMap, PropStore, PropStoreMut, Value};


/// A property map using a hashmap to store the properties.
///
/// This kind of map is very useful when not every handle from one mesh has a
/// value associated with it. The lookup is a bit slower than for `DenseMap`,
/// but the memory usage depends only on the number of inserted values and
/// *not* on the highest handle ID! As a simple rule that's correct in most
/// cases: if you want to associated a value with less than half of all
/// handles, `SparseMap` is a good choice.
///
/// This is just a wrapper around `std::collections::HashMap`. We cannot
/// implement the traits for `std::collections::HashMap` directly, because it
/// doesn't implement `Index<K>`. Instead of implements `Index<&Q>` where `Q`
/// is something that allows to borrow `K` from it. But our `PropStore`
/// requires `Index<H>`, so we have to use this wrapper type.
///
/// This implementation currently uses `ahash` as the hash function. This is
/// the default hash function of `hashbrown` as it's fairly fast and resistant
/// to collision attacks. However, `FxHash` is usually faster on integer keys
/// as it involves only one multiplication. This can lead to some problems,
/// though, as lower bits of the hash are never influenced by higher bits of
/// the key. When the lower bit pattern of all/many keys is the same, this can
/// lead to many collisions and several problems in the hashmap. We might want
/// to switch to `FxHash` in the future if we are sure that it's very unlikely
/// to cause any trouble in ou situation.
#[derive(Clone, Debug)]
pub struct SparseMap<H: Handle + Hash, T>(HashMap<H, T, ahash::RandomState>);

impl<H: Handle + Hash, T> SparseMap<H, T> {
    /// Creates an empty `SparseMap`.
    pub fn new() -> Self {
        SparseMap(HashMap::default())
    }
}


impl<H: Handle + Hash, T> PropMap<H> for SparseMap<H, T> {
    type Target = T;
    type Ret<'s> = &'s Self::Target where Self::Target: 's;

    fn get(&self, handle: H) -> Option<Value<Self::Ret<'_>, Self::Target>> {
        self.get_ref(handle).map(Into::into)
    }

    fn contains_handle(&self, handle: H) -> bool {
        self.0.contains_key(&handle)
    }
}

impl<H: Handle + Hash, T> Index<H> for SparseMap<H, T> {
    type Output = T;
    fn index(&self, handle: H) -> &Self::Output {
        match self.get_ref(handle) {
            None => panic!("no property found for handle '{:?}'", handle),
            Some(r) => r,
        }
    }
}

impl<H: Handle + Hash, T> PropStore<H> for SparseMap<H, T> {
    fn get_ref(&self, handle: H) -> Option<&Self::Output> {
        self.0.get(&handle)
    }

    fn num_props(&self) -> hsize {
        self.0.len() as hsize
    }

    fn iter(&self) -> Box<dyn Iterator<Item = (H, &Self::Output)> + '_> {
        Box::new(self.0.iter().map(|(k, v)| (*k, v)))
    }
}

impl<H: Handle + Hash, T> IndexMut<H> for SparseMap<H, T> {
    fn index_mut(&mut self, handle: H) -> &mut Self::Output {
        match self.get_mut(handle) {
            None => panic!("no property found for handle '{:?}'", handle),
            Some(r) => r,
        }
    }
}

impl<H: Handle + Hash, T> Empty for SparseMap<H, T> {
    fn empty() -> Self {
        Self::new()
    }
}

impl<H: Handle + Hash, T> PropStoreMut<H> for SparseMap<H, T> {
    fn get_mut(&mut self, handle: H) -> Option<&mut Self::Output> {
        self.0.get_mut(&handle)
    }

    fn insert(&mut self, handle: H, elem: Self::Output) -> Option<Self::Output> {
        self.0.insert(handle, elem)
    }

    fn remove(&mut self, handle: H) -> Option<Self::Output> {
        self.0.remove(&handle)
    }

    fn clear(&mut self) {
        self.0.clear()
    }

    fn reserve(&mut self, additional: hsize) {
        self.0.reserve(additional as usize);
    }

    fn iter_mut(&mut self) -> Box<dyn Iterator<Item = (H, &mut Self::Output)> + '_> {
        Box::new(self.0.iter_mut().map(|(k, v)| (*k, v)))
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    gen_tests_for_store_impl!(SparseMap);
}
