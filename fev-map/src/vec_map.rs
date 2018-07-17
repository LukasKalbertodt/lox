use std::{
    marker::PhantomData,
    mem,
    ops::{Index, IndexMut},
};

use stable_vec::{Keys, StableVec};

use fev_core::{
    handle::{DefaultId, Handle},
};

use crate::{
    PropMap, PropStore, PropStoreMut,
    gat::{Family, RefFamily},
};


/// A property map that uses a simple vector to store the properties.
///
/// # TODO
///
/// - Explain memory requirements of this data structure
#[derive(Clone, Debug)]
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

    pub fn num_elements(&self) -> DefaultId {
        self.vec.num_elements() as DefaultId
    }

    pub fn handles(&self) -> Handles<H> {
        Handles {
            iter: self.vec.keys(),
            _dummy: PhantomData,
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
    type Target = RefFamily<T>;
    fn get(&'s self, handle: H) -> Option<<Self::Target as Family<'a>>::Ty> {
        self.get_ref(handle).map(Into::into)
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
}

impl<H: Handle, T> IndexMut<H> for VecMap<H, T> {
    fn index_mut(&mut self, handle: H) -> &mut Self::Output {
        match self.get_mut(handle) {
            None => panic!("no property found for handle '{:?}'", handle),
            Some(r) => r,
        }
    }
}

impl<H: Handle, T> PropStoreMut<H> for VecMap<H, T> {
    fn get_mut(&mut self, handle: H) -> Option<&mut Self::Output> {
        self.vec.get_mut(handle.to_usize())
    }

    fn insert(&mut self, handle: H, mut elem: Self::Output) -> Option<Self::Output> {
        let idx = handle.to_usize();
        if self.vec.has_element_at(idx) {
            mem::swap(&mut self.vec[idx], &mut elem);
            Some(elem)
        } else {
            // Make sure `idx` is not out of bounds by growing the vector if
            // necessary.
            let next_index = self.vec.next_index();
            if next_index <= idx {
                self.vec.grow(1 + idx - next_index);
            }

            // We made sure that there is no element at `idx` and that `idx`
            // is not out of bounds. So we can unwrap here.
            self.vec.insert_into_hole(idx, elem).ok().unwrap();
            None
        }
    }

    fn remove(&mut self, handle: H) -> Option<Self::Output> {
        self.vec.remove(handle.to_usize())
    }

    fn empty() -> Self where Self: Sized {
        Self::new()
    }

    fn clear(&mut self) {
        self.vec.clear()
    }
}




pub struct Handles<'map, H: Handle> {
    iter: Keys<'map>,
    _dummy: PhantomData<H>,
}

impl<'map, H: Handle> Iterator for Handles<'map, H> {
    type Item = H;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(H::from_usize)
    }
}


// TODO: tests
