use crate::handle::Handle;
use super::{PropMap, boo};


/// Helper type for [`PropMap::map_value`]. See that method for more
/// information.
#[derive(Debug)]
pub struct Mapper<'m, M, F> {
    pub(super) inner: &'m M,
    pub(super) mapper: F,
}


impl<'m, H, M, F, TargetT, MarkerT> PropMap<H> for Mapper<'m, M, F>
where
    H: Handle,
    M: PropMap<H>,
    F: Fn(boo::Wrap<'_, M::Target, M::Marker>) -> boo::Wrap<'_, TargetT, MarkerT>,
    M::Target: 'm,
    MarkerT: boo::Marker,
{
    type Target = TargetT;
    type Marker = MarkerT;

    fn get(&self, handle: H) -> Option<boo::Wrap<'_, Self::Target, Self::Marker>> {
        self.inner.get(handle).map(&self.mapper)
    }
}
