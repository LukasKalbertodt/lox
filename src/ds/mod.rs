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

mod shared_vertex;

pub use self::{
    shared_vertex::SharedVertexMesh,
};
