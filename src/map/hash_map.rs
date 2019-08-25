use std::{
    collections::HashMap as StdHashMap,
    hash::Hash,
    ops::{Index, IndexMut},
};

use crate::{
    handle::{hsize, Handle},
    traits::Empty,
};
use super::{boo, PropMap, PropStore, PropStoreMut};


/// A property map using a hashmap to store the properties.
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
pub struct HashMap<H: Handle + Hash, T>(StdHashMap<H, T, ahash::ABuildHasher>);

impl<H: Handle + Hash, T> HashMap<H, T> {
    /// Creates an empty `HashMap`.
    ///
    /// To create a `HashMap` from a `std::collections::HashMap` you can use
    /// `HashMap::from()`.
    pub fn new() -> Self {
        HashMap(StdHashMap::default())
    }
}


impl<H: Handle + Hash, T> PropMap<H> for HashMap<H, T> {
    type Target = T;
    type Marker = boo::Borrowed;

    fn get(&self, handle: H) -> Option<boo::Wrap<'_, Self::Target, Self::Marker>> {
        self.get_ref(handle).map(Into::into)
    }

    fn contains_handle(&self, handle: H) -> bool {
        self.0.contains_key(&handle)
    }
}

impl<H: Handle + Hash, T> Index<H> for HashMap<H, T> {
    type Output = T;
    fn index(&self, handle: H) -> &Self::Output {
        match self.get_ref(handle) {
            None => panic!("no property found for handle '{:?}'", handle),
            Some(r) => r,
        }
    }
}

impl<H: Handle + Hash, T> PropStore<H> for HashMap<H, T> {
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

impl<H: Handle + Hash, T> IndexMut<H> for HashMap<H, T> {
    fn index_mut(&mut self, handle: H) -> &mut Self::Output {
        match self.get_mut(handle) {
            None => panic!("no property found for handle '{:?}'", handle),
            Some(r) => r,
        }
    }
}

impl<H: Handle + Hash, T> Empty for HashMap<H, T> {
    fn empty() -> Self {
        Self::new()
    }
}

impl<H: Handle + Hash, T> PropStoreMut<H> for HashMap<H, T> {
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

    gen_tests_for_store_impl!(HashMap);
}
