use fev_core::handle::Handle;

use crate::{
    PropMap,
    gat::Family,
};


// References to prop maps are prop maps
impl<'a, M: PropMap<H>, H: Handle> PropMap<H> for &'a M {
    type Target = M::Target;

    fn get(&'s self, handle: H) -> Option<<Self::Target as Family<'s>>::Ty> {
        M::get(*self, handle)
    }
}
