use std::marker::PhantomData;

pub trait Family<'a> {
    type Ty;
}

pub struct OwnedFamily<T> {
    _never: !,
    _phantom: PhantomData<T>,
}

impl<'a, T> Family<'a> for OwnedFamily<T> {
    type Ty = T;
}


pub struct RefFamily<T> {
    _never: !,
    _phantom: PhantomData<T>,
}

impl<'a, T: 'a> Family<'a> for RefFamily<T> {
    type Ty = &'a T;
}
