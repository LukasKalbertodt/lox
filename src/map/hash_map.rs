use std::{
    collections::HashMap as StdHashMap,
    hash::Hash,
    ops::{Index, IndexMut},
};

use crate::handle::Handle;
use super::{boo, PropMap, PropStore, PropStoreMut};


/// A property map using a hashmap to store the properties.
///
/// This is just a wrapper around `std::collections::HashMap`. We cannot
/// implement the traits for `std::collections::HashMap` directly, because it
/// doesn't implement `Index<K>`. Instead of implements `Index<&Q>` where `Q`
/// is something that allows to borrow `K` from it. But our `PropStore`
/// requires `Index<H>`, so we have to use this wrapper type.
#[derive(Clone, Debug)]
pub struct HashMap<H: Handle + Hash, T>(StdHashMap<H, T>);

impl<H: Handle + Hash, T> HashMap<H, T> {
    /// Creates an empty `HashMap`.
    ///
    /// To create a `HashMap` from a `std::collections::HashMap` you can use
    /// `HashMap::from()`.
    pub fn new() -> Self {
        HashMap(StdHashMap::new())
    }

    /// Returns an immutable reference to the inner
    /// `std::collections::HashMap`.
    pub fn inner(&self) -> &StdHashMap<H, T> {
        &self.0
    }

    /// Returns a mutable reference to the inner `std::collections::HashMap`.
    pub fn inner_mut(&mut self) -> &mut StdHashMap<H, T> {
        &mut self.0
    }
}


impl<H: Handle + Hash, T> PropMap<H> for HashMap<H, T> {
    type Target = T;
    type Marker = boo::Borrowed;

    fn get(&self, handle: H) -> Option<boo::Wrap<'_, Self::Target, Self::Marker>> {
        self.get_ref(handle).map(Into::into)
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
}

impl<H: Handle + Hash, T> IndexMut<H> for HashMap<H, T> {
    fn index_mut(&mut self, handle: H) -> &mut Self::Output {
        match self.get_mut(handle) {
            None => panic!("no property found for handle '{:?}'", handle),
            Some(r) => r,
        }
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

    fn empty() -> Self where Self: Sized {
        Self::new()
    }

    fn clear(&mut self) {
        self.0.clear()
    }

    fn reserve(&mut self, additional: usize) {
        self.0.reserve(additional);
    }
}

impl<H: Handle + Hash, T> From<StdHashMap<H, T>> for HashMap<H, T> {
    fn from(src: StdHashMap<H, T>) -> Self {
        HashMap(src)
    }
}



// TODO: tests
