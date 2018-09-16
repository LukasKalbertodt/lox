//! Everything related to meshes.
//!
//! **TODO**: Everything.

pub mod handle;
pub mod math;
mod mesh;
pub mod refs;

pub use self::{
    handle::{EdgeHandle, FaceHandle, VertexHandle},
    mesh::{Mesh, TriMesh, MeshElement, ExplicitFace, ExplicitVertex},
};
