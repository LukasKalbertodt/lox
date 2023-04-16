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
    handle::{EdgeHandle, FaceHandle, Handle, VertexHandle},
    map::{PropMap, PropStore, PropStoreMut},
    prop::{ColorLike, Pos3Like, Vec3Like},
    core::{
        Mesh, MeshMut, TriMesh, PolyMesh, EdgeMesh,
        BasicAdj, FullAdj, EdgeAdj, SupportsMultiBlade,
    },
    util::{Empty, IteratorExt},
};

// #[cfg(feature = "io")]
// pub use crate::{
//     MemSink, MemSource,
//     io::{
//         StreamSource, StreamSink, MemSource, MemSink, DynStreamSource, DynStreamSink,
//         util::{MemSourceExt},
//     },
// };
