use std::{
    marker::PhantomData,
    ops::Index,
};

use crate::{
    handle::Handle,
    map::PropMap,
};





pub struct ConstMap<H: Handle, T> {
    item: T,
    _dummy: PhantomData<H>,
}

impl<H: Handle, T> ConstMap<H, T> {
    pub fn new(item: T) -> Self {
        Self {
            item,
            _dummy: PhantomData,
        }
    }
}

impl<H: Handle, T> PropMap<H> for ConstMap<H, T> {
    fn get(&self, _: H) -> Option<&Self::Output> {
        Some(&self.item)
    }
}

impl<H: Handle, T> Index<H> for ConstMap<H, T> {
    type Output = T;
    fn index(&self, _: H) -> &Self::Output {
        &self.item
    }
}
