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

pub use leer::Empty;
pub use crate::{
    Handle,
    core::{
        Mesh, MeshMut, TriMesh, PolyMesh, EdgeMesh,
        BasicAdj, FullAdj, EdgeAdj, SupportsMultiBlade,
    },
    map::{PropMap, PropStore, PropStoreMut},
    util::{IteratorExt, ColorLike, Pos3Like, Vec3Like},
};

// This is just for internal convenience: types that are used all the time.
pub(crate) use crate::{EdgeHandle, FaceHandle, VertexHandle};


// #[cfg(feature = "io")]
// pub use crate::{
//     MemSink, MemSource,
//     io::{
//         StreamSource, StreamSink, MemSource, MemSink, DynStreamSource, DynStreamSink,
//         util::{MemSourceExt},
//     },
// };
