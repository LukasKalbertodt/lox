use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

use stable_vec::{Keys, StableVec};

use handle::{DefaultIndex, Handle, HandleIndex, FaceHandle, EdgeHandle, VertexHandle};


pub trait AttrMap<H: Handle>: Index<H> {}


#[derive(Clone)]
pub struct VecMap<H: Handle, T> {
    vec: StableVec<T>,
    _dummy: PhantomData<H>,
}

pub type FaceVecMap<T, Idx = DefaultIndex> = VecMap<FaceHandle<Idx>, T>;
pub type EdgeVecMap<T, Idx = DefaultIndex> = VecMap<EdgeHandle<Idx>, T>;
pub type VertexVecMap<T, Idx = DefaultIndex> = VecMap<VertexHandle<Idx>, T>;

impl<H: Handle, T> VecMap<H, T> {
    pub fn new() -> Self {
        Self {
            vec: StableVec::new(),
            _dummy: PhantomData,
        }
    }

    pub fn push(&mut self, elem: T) -> H {
        self.vec.push(elem).into()
    }

    pub fn num_elements(&self) -> H::Idx {
        H::Idx::from_usize(self.vec.num_elements())
    }

    pub fn handles(&self) -> Handles<H> {
        Handles {
            iter: self.vec.keys(),
            dummy: PhantomData,
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

impl<H: Handle, T> AttrMap<H> for VecMap<H, T> {}

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

// pub struct MapList {
//     maps: Ms,
// }
