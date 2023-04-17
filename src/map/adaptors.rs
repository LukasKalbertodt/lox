use crate::Handle;
use super::{PropMap, Value};


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
    M::Target: 'm,
    F: Fn(Value<M::Ret<'m>, M::Target>) -> TargetT,
{
    type Target = TargetT;
    type Ret<'s> = Self::Target where Self: 's;

    fn get(&self, handle: H) -> Option<Value<Self::Ret<'_>, Self::Target>> {
        self.inner.get(handle)
            .map(|v| (&self.mapper)(v).into())
    }
}
