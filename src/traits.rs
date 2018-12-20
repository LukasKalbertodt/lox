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

pub trait Empty {
    /// Returns an empty value of this type.
    fn empty() -> Self;
}

/// Some kind of polygon mesh.
pub trait Mesh: Empty {
    // ===== Vertices ========================================================
    fn num_vertices(&self) -> DefaultInt;
    fn vertices<'s>(&'s self) -> Box<dyn Iterator<Item = VertexRef<'s, Self>> + 's>;

    // TODO: visit_mut
    // TODO: iterator over handles
    // TODO: mutable iterator?


    // ===== Faces ===========================================================
    fn num_faces(&self) -> DefaultInt;
    fn faces<'s>(&'s self) -> Box<dyn Iterator<Item = FaceRef<'s, Self>> + 's>;
    // TODO: visit_mut
    // TODO: iterator over handles
    // TODO: mutable iterator?
}

pub trait MeshMut: Mesh {
    fn add_vertex(&mut self) -> VertexHandle;
}

/// A triangular mesh: all faces are triangles.
pub trait TriMesh: Mesh {}

pub trait TriMeshMut: TriMesh + MeshMut {
    // CCW!
    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle;

}

pub trait TriVerticesOfFace: TriMesh {
    /// Maybe we should return vertex refs? CCW!
    fn vertices_of_face(&self, face: FaceHandle) -> [VertexHandle; 3];
}
