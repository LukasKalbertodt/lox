use std::{
    collections::HashMap,
    hash::Hash,
    ops::{Index, IndexMut},
};

use crate::{
    hsize,
    prelude::*,
};
use super::{PropMap, PropStore, PropStoreMut, Value, util::gen_mapped_iter};


/// A property map using a hashmap to store the properties.
///
/// This kind of map is very useful when not every handle from one mesh has a
/// value associated with it. The lookup is a bit slower than for
/// [`DenseMap`], but the memory usage depends only on the number of inserted
/// values and *not* on the highest handle ID! As a simple rule that's correct
/// in most cases: if you want to associated a value with less than half of all
/// handles, `SparseMap` is a good choice.
///
/// This is just a wrapper around `std::collections::HashMap` using the `ahash`
/// hashing function.
///
/// [`DenseMap`]: super::DenseMap
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

    type Iter<'s> = Iter<'s, H, T> where Self: 's;
    fn iter(&self) -> Self::Iter<'_> {
        Iter(self.0.iter())
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

    type IterMut<'s> = IterMut<'s, H, T> where Self: 's;
    fn iter_mut(&mut self) -> Self::IterMut<'_> {
        IterMut(self.0.iter_mut())
    }
}


gen_mapped_iter!(
    Iter<'a, H, T>(std::collections::hash_map::Iter<'a, H, T>);
    mut_token: [],
    extra_derives: [Clone],
    mapping: |(k, v)| (*k, v),
    double_ended: false,
);
gen_mapped_iter!(
    IterMut<'a, H, T>(std::collections::hash_map::IterMut<'a, H, T>);
    mut_token: [mut],
    extra_derives: [],
    mapping: |(k, v)| (*k, v),
    double_ended: false,
);


#[cfg(test)]
mod tests {
    use super::*;

    gen_tests_for_store_impl!(SparseMap);
}
