//! Polygon mesh **d**ata **s**tructures.
//!
//! This module contains the different implementations of the mesh traits in
//! this libary.
//!
//! # TODO
//! - list all implementations
//! - explain advantages and disadvantages of data structures

use std::{fmt, ops};

use crate::{
    handle::{Handle, hsize},
};


#[cfg(test)]
#[macro_use]
mod tests;

// mod face_delegate;
pub mod half_edge;
mod shared_vertex;

pub use self::{
    // face_delegate::FaceDelegateMesh,
    half_edge::HalfEdgeMesh,
    shared_vertex::SharedVertexMesh,
};


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
struct Checked<H: Handle>(H);

impl<H: Handle> Handle for Checked<H> {
    #[inline(always)]
    fn new(idx: hsize) -> Self {
        Self(H::new(idx))
    }

    #[inline(always)]
    fn idx(&self) -> hsize {
        self.0.idx()
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
