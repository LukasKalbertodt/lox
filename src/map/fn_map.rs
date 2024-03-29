use crate::Handle;
use super::{PropMap, Value};


/// A simple wrapper for property maps defined by functions (usually closures).
///
/// This wrapper only exists because we can't implement `PropMap` directly for
/// any `F` where `F: Fn(Handle) -> Target` due to coherence rules.
///
/// # Example
///
/// ```
/// use lox::{
///     Handle, VertexHandle,
///     map::{FnMap, PropMap},
/// };
///
/// fn foo(map: &impl PropMap<VertexHandle>) {}
///
///
/// // This property map returns 27 for all handles with an id smaller than 10.
/// let map = FnMap(|h: VertexHandle| {
///     if h.to_usize() < 10 {
///         Some(27)
///     } else {
///         None
///     }
/// });
/// foo(&map);  // works
///
/// // This property map always returns 3 (you don't need to use the handle).
/// // However, in this case, you should usually use `ConstMap`.
/// foo(&FnMap(|_| Some(3)));  // works
/// ```
#[derive(Clone, Copy, Debug)]
pub struct FnMap<F>(pub F);

impl<H, F, OutT> PropMap<H> for FnMap<F>
where
    H: Handle,
    F: Fn(H) -> Option<OutT>,
{
    type Target = OutT;
    type Ret<'s> = Self::Target where F: 's;

    fn get(&self, handle: H) -> Option<Value<Self::Ret<'_>, Self::Target>> {
        (self.0)(handle).map(Into::into)
    }
}
