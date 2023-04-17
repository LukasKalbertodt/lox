//! This lives in its own module to make sure no one can access the private
//! field. To create a `Checked`, everyone has to go through the `unsafe`
//! method `new`.

use std::{fmt, ops};

use crate::{Handle, hsize};


/// A wrapper for handles to signal that they point to an existing element. Can
/// be used by data structures internally.
///
/// We optimally want to remove all bound checks when accessing mesh elements.
/// However, we certainly don't want to cause UB if the user specifies an
/// incorrect handle. But we can get rid of internal bound checks: as long as
/// our implementation is correct, it's fine. Internal bound checks account for
/// the majority of checks anyway.
///
/// To distinguish handles given by the user and handles that the data
/// structure guarantees to be correct, we use this wrapper type.
///
/// Note that this is not a super strict requirement: there can exist instance
/// of this with a handle that is invalid (does not point to an existing
/// element). But these instances should be temporary and must never persist
/// over multiple public method calls.
///
/// This is only a marker type and does not really do anything on its own. Data
/// structure implementations have the full responsibility in how they use this
/// marker type.
#[repr(transparent)] // <-- Danger: some unsafe code relies on this!
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Checked<H: Handle>(H);

impl<H: Handle> Checked<H> {
    /// Creates a new `Checked` instance.
    ///
    /// This function is `unsafe` not because this function itself can trigger
    /// UB, but because an invalid `Checked` value used to index a storage can
    /// cause UB later. So if UB is caused by `Checked`, the bug comes from
    /// where the instance is created via this function. As said in the type
    /// documentation, this function might actually be called with an invalid
    /// handle: this won't do any harm immediately. You just have to make sure
    /// this `Checked` instance is overwritten with a valid value before it is
    /// used.
    #[inline(always)]
    pub unsafe fn new(handle: H) -> Self {
        Self(handle)
    }
}

impl<H: Handle> optional::Noned for Checked<H> {
    fn is_none(&self) -> bool {
        self.0.idx() == hsize::max_value()
    }
    fn get_none() -> Self {
        Self(H::new(hsize::max_value()))
    }
}
impl<H: Handle> optional::OptEq for Checked<H> {
    fn opt_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl<H: Handle> fmt::Debug for Checked<H> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<H: Handle> ops::Deref for Checked<H> {
    type Target = H;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
