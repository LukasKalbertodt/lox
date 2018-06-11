use fev_core::handle::Handle;

use super::PropMap;


// ===========================================================================
// ===== Impls for closures and the like
// ===========================================================================
impl<'s, H, F, OutT> PropMap<'s, H> for F
where
    H: Handle,
    F: Fn(H) -> Option<OutT>,
{
    type Target = OutT;

    fn get(&'s self, handle: H) -> Option<Self::Target> {
        self(handle)
    }
}
