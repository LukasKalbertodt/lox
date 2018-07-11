use fev_core::handle::Handle;

use super::PropMap;


// References to prop maps are prop maps
impl<'a, 's, M, H> PropMap<'s, H> for &'a M
where
    M: PropMap<'s, H>,
    H: Handle,
{
    type Target = M::Target;

    fn get(&'s self, handle: H) -> Option<Self::Target> {
        <M as PropMap<'s, H>>::get(*self, handle)
    }
}
