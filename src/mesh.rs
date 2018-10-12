use crate::{
    handle::{DefaultInt, FaceHandle, VertexHandle},
    refs::{FaceRef, VertexRef},
};

/// The three basic elements in a polygon mesh.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MeshElement {
    Edge,
    Face,
    Vertex,
}


/// Some kind of polygon mesh.
pub trait Mesh {
    /// Returns an empty mesh instance.
    fn empty() -> Self;
}

/// A triangular mesh: all faces are triangles.
pub trait TriMesh: Mesh {}


// Alternative names:
// - HasVertices
// - ExplicitVertex
// - ContainsVertices
// - VertexIndex
// - WithVerts
pub trait ExplicitVertex {
    fn num_vertices(&self) -> DefaultInt;

    fn add_vertex(&mut self) -> VertexHandle;

    fn vertices<'s>(&'s self) -> Box<dyn Iterator<Item = VertexRef<'s, Self>> + 's>;
    // TODO: visit_mut
    // TODO: iterator over handles
    // TODO: mutable iterator?
}

pub trait ExplicitFace {
    fn num_faces(&self) -> DefaultInt;

    // CCW!
    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle;

    fn faces<'s>(&'s self) -> Box<dyn Iterator<Item = FaceRef<'s, Self>> + 's>;
    // TODO: visit_mut
    // TODO: iterator over handles
    // TODO: mutable iterator?
}


pub trait MeshUnsorted {
    /// Maybe we should return vertex refs? CCW!
    fn vertices_of_face(&self, face: FaceHandle) -> [VertexHandle; 3];
}
