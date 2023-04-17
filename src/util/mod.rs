//! Various helper traits and types.

use lina::Point3;

use crate::{
    hsize,
    sealed::Sealed,
};


mod list;
mod num;
mod prop;

pub use self::{
    list::{TriList, DiList, TriListIntoIter, DiListIntoIter, TriListIter, DiListIter},
    num::{CastFromPrimitive, CastIntoPrimitive, PrimitiveNum, PrimitiveCast, PrimitiveFloat},
    prop::{ColorLike, PrimitiveColorChannel, Pos3Like, Vec3Like},
};


// ===========================================================================
// ===== Extension traits
// ===========================================================================

/// Extension trait to add some useful methods to any type implementing
/// `Iterator`.
pub trait IteratorExt: Sized + Iterator {
    fn into_vec(self) -> Vec<Self::Item> {
        self.collect()
    }

    fn centroid(self) -> Option<Self::Item>
    where
        Self::Item: Pos3Like,
    {
        Point3::centroid(self.map(|item| item.to_point3())).map(|p| p.convert())
    }
}

impl<I: Iterator> IteratorExt for I {}


/// Extension trait to add a few useful methods to `hsize`.
pub trait HSizeExt {
    /// Returns a new index.
    ///
    /// When the index space has been exhausted and there is no new index, this
    /// function either panics or returns an old index. In debug mode, this
    /// function is guaranteed to panic in this case.
    fn next(self) -> Self;
}

impl HSizeExt for hsize {
    #[inline(always)]
    fn next(self) -> Self {
        self + 1
    }
}


// ===========================================================================
// ===== Type level bool
// ===========================================================================

/// Type level boolean. Only implemented by [`True`] and [`False`].
///
/// Once const generics land, this is not necessary anymore.
pub trait Bool: Sealed {
    const VALUE: bool;
}

/// Type level `true` boolean value.
#[allow(missing_debug_implementations)]
pub enum True {}
impl Sealed for True {}
impl Bool for True {
    const VALUE: bool = true;
}

/// Type level `false` boolean value.
#[allow(missing_debug_implementations)]
pub enum False {}
impl Sealed for False {}
impl Bool for False {
    const VALUE: bool = false;
}
