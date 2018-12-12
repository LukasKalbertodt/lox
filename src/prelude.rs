//! Reexports of all important traits of this library for convenience.
//!
//! As with every prelude, the main usage is to glob import everything from
//! this module:
//!
//! ```
//! use lox::prelude::*;
//! ```
//!
//! Now you have all important traits in scope.

pub use crate::{
    Mesh, TriMesh, MeshMut, TriMeshMut, MeshUnsorted, MeshSource, MeshSink, Empty,
    handle::{EdgeHandle, FaceHandle, Handle, VertexHandle},
    map::{PropMap, PropStore, PropStoreMut},
    math::{Pos3Like, Vec3Like},
    io::{IntoMeshWriter, MeshWriter},
};
