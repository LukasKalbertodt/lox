//! Fat meshes: TODO

use cgmath::Point3;

// For proc macros
use crate as lox;

use crate::{
    VertexHandle, Empty, MemSink,
    map::VecMap,
    traits::TriMeshMut,
};


/// A fat mesh that only stores connectivity and `f32` vertex positions.
///
/// The type of the core mesh is specified by the type parameter `M`. The cast
/// rigor for the vertex positions is "lossy", i.e. all kinds of casts are
/// allowed.
#[derive(Empty, MemSink, Debug)]
pub struct MiniMesh<M: TriMeshMut> {
    mesh: M,
    #[lox(vertex_positions(cast = "lossy"))]
    vertex_positions: VecMap<VertexHandle, Point3<f32>>,
}
