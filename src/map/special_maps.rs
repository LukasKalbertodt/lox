use std::marker::PhantomData;

use crate::handle::Handle;
use super::{boo, PropMap};


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
pub struct ConstMap<T>(pub T);

impl<T, H: Handle> PropMap<H> for ConstMap<T> {
    type Target = T;
    type Marker = boo::Borrowed;

    fn get(&self, _: H) -> Option<boo::Wrap<'_, Self::Target, Self::Marker>> {
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
pub struct EmptyMap<T>(PhantomData<T>);

impl<T> EmptyMap<T> {
    pub fn new() -> Self {
        EmptyMap(PhantomData)
    }
}

impl<T, H: Handle> PropMap<H> for EmptyMap<T> {
    type Target = T;
    type Marker = boo::Owned;

    fn get(&self, _: H) -> Option<boo::Wrap<'_, Self::Target, Self::Marker>> {
        None
    }
}
