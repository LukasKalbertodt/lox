use crate::handle::Handle;
use super::{PropMap, boo};


/// Helper type for [`PropMap::map_value`]. See that method for more
/// information.
#[derive(Debug)]
pub struct Mapper<'m, M, F> {
    pub(super) inner: &'m M,
    pub(super) mapper: F,
}

impl<'m, H, M, F, TargetT> PropMap<H> for Mapper<'m, M, F>
where
    H: Handle,
    M: PropMap<H>,
    F: Fn(boo::Wrap<'_, M::Target, M::Marker>) -> TargetT,
    M::Target: 'm,
{
    type Target = TargetT;
    type Marker = boo::Owned;

    fn get(&self, handle: H) -> Option<boo::Wrap<'_, Self::Target, Self::Marker>> {
        self.inner.get(handle)
            .map(|v| (&self.mapper)(v).into())
    }
}


/// Helper type for [`PropMap::map_ref`]. See that method for more
/// information.
#[derive(Debug)]
pub struct RefMapper<'m, M, F> {
    pub(super) inner: &'m M,
    pub(super) mapper: F,
}

impl<'m, H, M, F, TargetT> PropMap<H> for RefMapper<'m, M, F>
where
    H: Handle,
    M: PropMap<H, Marker = boo::Borrowed>,
    M::Target: 'm,
    F: Fn(&M::Target) -> &TargetT,
{
    type Target = TargetT;
    type Marker = boo::Borrowed;

    fn get(&self, handle: H) -> Option<boo::Wrap<'_, Self::Target, Self::Marker>> {
        self.inner.get(handle)
            .map(|v| (&self.mapper)(v.into_inner()).into())
    }
}
