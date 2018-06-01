use std::{
    marker::PhantomData,
    mem,
    ops::{Index, IndexMut},
};

use stable_vec::{Keys, StableVec};

use crate::{
    handle::{DefaultIndex, DefaultIndexExt, Handle},
    map::{PropMap, PropMapMut},
};



#[derive(Clone)]
pub struct VecMap<H: Handle, T> {
    vec: StableVec<T>,
    _dummy: PhantomData<H>,
}

impl<H: Handle, T> VecMap<H, T> {
    pub fn new() -> Self {
        Self {
            vec: StableVec::new(),
            _dummy: PhantomData,
        }
    }

    pub fn push(&mut self, elem: T) -> H {
        H::from_usize(self.vec.push(elem))
    }

    pub fn insert(&mut self, h: H, mut elem: T) -> Option<T> {
        let idx = h.idx().to_usize();
        if self.vec.has_element_at(idx) {
            mem::swap(&mut self.vec[idx], &mut elem);
            Some(elem)
        } else {
            // Make sure `idx` is not out of bounds
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

    pub fn num_elements(&self) -> DefaultIndex {
        self.vec.num_elements() as DefaultIndex
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
    fn get(&self, handle: H) -> Option<&Self::Output> {
        self.vec.get(handle.idx().to_usize())
    }
}

impl<H: Handle, T> PropMapMut<H> for VecMap<H, T> {
    fn get_mut(&mut self, handle: H) -> Option<&mut Self::Output> {
        self.vec.get_mut(handle.idx().to_usize())
    }
}


impl<H: Handle, T> Index<H> for VecMap<H, T> {
    type Output = T;
    fn index(&self, h: H) -> &Self::Output {
        &self.vec[h.idx().to_usize()]
    }
}

impl<H: Handle, T> IndexMut<H> for VecMap<H, T> {
    fn index_mut(&mut self, h: H) -> &mut Self::Output {
        &mut self.vec[h.idx().to_usize()]
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
