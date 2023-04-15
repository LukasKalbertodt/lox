use std::marker::PhantomData;

use crate::handle::Handle;
use super::{PropMap, Value};


/// A map that always returns the same value for all handles (always `Some()`).
///
/// # Example
///
/// ```
/// use lox::{
///     VertexHandle,
///     map::{ConstMap, PropMap},
/// };
///
/// fn foo(map: &impl PropMap<VertexHandle>) {}
///
/// foo(&ConstMap(27));  // always returns `&27`
/// ```
#[derive(Clone, Copy, Debug)]
pub struct ConstMap<T>(pub T);

impl<T, H: Handle> PropMap<H> for ConstMap<T> {
    type Target = T;
    type Ret<'s> = &'s Self::Target where Self::Target: 's;

    fn get(&self, _: H) -> Option<Value<Self::Ret<'_>, Self::Target>> {
        Some((&self.0).into())
    }
}


/// A map that always returns `None`. It is also generic over the type it
/// returns, so as far as the type system is concerned, it can return any type.
///
/// # Example
///
/// ```
/// use lox::{
///     VertexHandle,
///     map::{EmptyMap, PropMap},
/// };
///
/// // When the type is already fixed somewhere, you don't need to specify it
/// fn foo(map: &impl PropMap<VertexHandle, Target = f32>) {}
/// foo(&EmptyMap::new());
///
/// // If the type is not fixed, you might need to manually specify it like so
/// fn bar(map: &impl PropMap<VertexHandle>) {}
/// bar(&EmptyMap::<u32>::new());
/// ```
#[derive(Clone, Copy, Debug)]
pub struct EmptyMap<T>(PhantomData<T>);

impl<T> EmptyMap<T> {
    pub fn new() -> Self {
        EmptyMap(PhantomData)
    }
}

impl<T, H: Handle> PropMap<H> for EmptyMap<T> {
    type Target = T;
    type Ret<'s> = Self::Target where T: 's;

    fn get(&self, _: H) -> Option<Value<Self::Ret<'_>, Self::Target>> {
        None
    }
}
