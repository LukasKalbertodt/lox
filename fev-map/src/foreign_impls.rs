use fev_core::handle::Handle;

use super::{PropMap, boo};


// References to prop maps are prop maps
impl<'a, M: PropMap<H>, H: Handle> PropMap<H> for &'a M {
    type Target = M::Target;

    fn get(&self, handle: H) -> Option<boo::Wrap<Self::Target>> {
        M::get(*self, handle)
    }
}
