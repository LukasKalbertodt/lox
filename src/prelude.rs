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
    Empty, MemSource, MemSink,
    handle::{EdgeHandle, FaceHandle, Handle, VertexHandle},
    map::{PropMap, PropStore, PropStoreMut},
    prop::{ColorLike, Pos3Like, Vec3Like},
    io::{
        StreamSource, StreamSink, MemSource, MemSink, DynStreamSource, DynStreamSink,
        util::{MemSourceExt},
    },
    traits::{
        Empty, Mesh, MeshMut, TriMesh, PolyMesh, TriMeshMut, PolyMeshMut, EdgeMesh,
        adj::{
            VerticesAroundFace, FacesAroundVertex, VerticesAroundVertex, FacesAroundFace,
            EToF, EToV,
        },
        marker::{SupportsMultiBlade},
    },
    util::{IteratorExt, TriArrayExt, DynList},
};
