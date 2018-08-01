use fev_core::handle::Handle;

use super::{boo, PropMap};


/// A simple wrapper for property maps defined by functions (usually closures).
///
/// This wrapper only exists because of some limitations of Rust in regards to
/// overlapping impls. In principle it is possible to `impl PropMap for F`
/// where `F: Fn(Handle) -> Target`. But this would be a a general impl that
/// could (in theory) apply to every type. This makes it impossible to add
/// other general impls such as `impl PropMap for &M where M: PropMap`. Since
/// the impl for references to property maps is important, property maps
/// defined by functions have to use this wrapper type.
///
/// # Example
///
/// ```
/// # extern crate fev_core;
/// # extern crate fev_map;
/// use fev_core::handle::{Handle, VertexHandle};
/// use fev_map::{FnMap, PropMap};
///
/// fn foo<'a>(map: &impl PropMap<'a, VertexHandle>) {}
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
/// foo(&FnMap(|_| Some(3)));  // works
/// ```
pub struct FnMap<F>(pub F);

impl<H, F, OutT> PropMap<H> for FnMap<F>
where
    H: Handle,
    F: Fn(H) -> Option<OutT>,
{
    type Target = boo::Owned<OutT>;

    fn get(&self, handle: H) -> Option<boo::Wrap<'_, Self::Target>> {
        // unimplemented!()
        (self.0)(handle).map(Into::into)
    }
}
