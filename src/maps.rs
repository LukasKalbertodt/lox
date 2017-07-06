use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

use stable_vec::StableVec;

use handle::{DefaultIndex, Handle, HandleIndex, FaceHandle, EdgeHandle, VertexHandle};


#[derive(Clone)]
pub struct AttrMap<H: Handle, T> {
    vec: StableVec<T>,
    _dummy: PhantomData<H>,
}

pub type FaceMap<T, Idx = DefaultIndex> = AttrMap<FaceHandle<Idx>, T>;
pub type EdgeMap<T, Idx = DefaultIndex> = AttrMap<EdgeHandle<Idx>, T>;
pub type VertexMap<T, Idx = DefaultIndex> = AttrMap<VertexHandle<Idx>, T>;

impl<H: Handle, T> AttrMap<H, T> {
    pub fn new() -> Self {
        Self {
            vec: StableVec::new(),
            _dummy: PhantomData,
        }
    }

    pub fn push(&mut self, elem: T) -> H {
        self.vec.push(elem).into()
    }
}

impl<H: Handle, T: Clone> AttrMap<H, T> {
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

impl<H: Handle, T> Index<H> for AttrMap<H, T> {
    type Output = T;
    fn index(&self, h: H) -> &Self::Output {
        &self.vec[h.idx().to_usize()]
    }
}

impl<H: Handle, T> IndexMut<H> for AttrMap<H, T> {
    fn index_mut(&mut self, h: H) -> &mut Self::Output {
        &mut self.vec[h.idx().to_usize()]
    }
}
