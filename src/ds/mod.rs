//! Polygon mesh **d**ata **s**tructures.
//!
//! This module contains the different implementations of the mesh traits in
//! this libary.
//!
//! # TODO
//! - list all implementations
//! - explain advantages and disadvantages of data structures

use std::fmt;

use crate::{
    traits::marker::{Bool, True, False},
};

#[cfg(test)]
#[macro_use]
mod tests;

mod checked;
pub mod directed_edge;
// mod face_delegate;
pub mod half_edge;
mod shared_vertex;

pub use self::{
    directed_edge::DirectedEdgeMesh,
    // face_delegate::FaceDelegateMesh,
    half_edge::HalfEdgeMesh,
    shared_vertex::SharedVertexMesh,
};

pub(crate) use self::checked::Checked;


pub(crate) trait TypeOrVoid<T: Copy + fmt::Debug>: Bool {
    type Output: Copy + fmt::Debug;
    fn new(t: T) -> Self::Output;
}

impl<B: Bool, T: Copy + fmt::Debug> TypeOrVoid<T> for B {
    // Unreachable
    default type Output = !;
    default fn new(_: T) -> Self::Output {
        unreachable!()
    }
}

impl<T: Copy + fmt::Debug> TypeOrVoid<T> for True {
    type Output = T;
    fn new(t: T) -> Self::Output {
        t
    }
}

impl<T: Copy + fmt::Debug> TypeOrVoid<T> for False {
    type Output = ();
    fn new(_: T) -> Self::Output {
        ()
    }
}
