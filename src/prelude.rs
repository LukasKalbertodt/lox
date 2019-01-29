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
    Mesh, TriMesh, MeshMut, TriMeshMut, TriVerticesOfFace, Empty,
    FacesAroundVertex, VerticesAroundVertex, TriFacesAroundFace,
    handle::{EdgeHandle, FaceHandle, Handle, VertexHandle},
    map::{PropMap, PropStore, PropStoreMut},
    math::{Pos3Like, Vec3Like},
    io::{StreamSource, MemSink},
    util::{PointIteratorExt, TriArrayExt},
};
