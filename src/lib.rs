//! Everything related to meshes.
//!
//! **TODO**: Everything.

#![feature(never_type)]


pub mod ds;
pub mod handle;
mod macros;
pub mod map;
pub mod math;
mod mesh;
pub mod refs;

pub use self::{
    handle::{EdgeHandle, FaceHandle, VertexHandle},
    mesh::{Mesh, TriMesh, MeshElement, ExplicitFace, ExplicitVertex},
};
