//! Fat meshes: TODO

use cgmath::Point3;

// For proc macros
use crate as lox;

use crate::{
    VertexHandle, Empty, MemSink,
    cast,
    io::{Error, MemSource, PrimitiveType, Primitive},
    map::{PropMap, VecMap},
    traits::{TriMeshMut, TriVerticesOfFace},
};


/// A fat mesh that only stores connectivity and `f32` vertex positions.
///
/// The type of the core mesh is specified by the type parameter `M`. The cast
/// rigor for the vertex positions is "lossy", i.e. all kinds of casts are
/// allowed.
#[derive(Empty, MemSink, Debug)]
pub struct MiniMesh<M: TriMeshMut + TriVerticesOfFace> {
    #[lox(core_mesh)]
    pub mesh: M,

    #[lox(vertex_position, cast = "lossy")]
    pub vertex_positions: VecMap<VertexHandle, Point3<f32>>,
}


impl<M: TriMeshMut + TriVerticesOfFace> MemSource for MiniMesh<M> {
    type CoreMesh = M;
    fn core_mesh(&self) -> &Self::CoreMesh {
        &self.mesh
    }
    fn vertex_position_type(&self) -> Option<PrimitiveType> {
        Some(PrimitiveType::Float32)
    }
    fn vertex_position<T: Primitive>(&self, v: VertexHandle) -> Result<Option<Point3<T>>, Error> {
        if !cast::is_cast_possible::<cast::Lossy, T, f32>() {
            return Err(Error::SourceIncompatible {
                prop: lox::io::PropKind::VertexPosition,
                requested_type: T::TY,
            });
        }

        Ok(self.vertex_positions.get(v).map(|p| p.map(|s| cast::lossy(s))))
    }
}
