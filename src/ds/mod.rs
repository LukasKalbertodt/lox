//! Polygon mesh **d**ata **s**tructures.
//!
//! This module contains the different implementations of the mesh traits in
//! this libary.
//!
//! # TODO
//! - list all implementations
//! - explain advantages and disadvantages of data structures

#[cfg(test)]
#[macro_use]
mod tests;

mod linked_face;
mod shared_vertex;

pub use self::{
    linked_face::LinkedFaceMesh,
    shared_vertex::SharedVertexMesh,
};
