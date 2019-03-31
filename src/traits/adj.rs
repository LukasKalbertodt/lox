//! Contains traits related to **adj**acency information.
//!
//! In almost all mesh algorithms we need some kind of information about the
//! connectivity of the mesh's elements (vertex, edge, face). TODO.

use crate::{
    handle::{FaceHandle, VertexHandle},
    util::{DynList, TriList},
};
use super::{TriMesh, Mesh};

/// Meshes with *O*(1) face-to-vertex neighborhood information.
pub trait VerticesAroundFace: Mesh {
    /// Returns the vertices of the given triangular face in front-face CCW
    /// order.
    fn vertices_around_triangle(&self, face: FaceHandle) -> [VertexHandle; 3]
    where
        Self: TriMesh;

    /// Returns the vertices around the given face in front-face CCW order.
    ///
    /// If you are dealing with a triangular mesh, rather use
    /// [`vertices_around_triangle`][VerticesAroundFace::vertices_around_triangle]
    /// instead as it's usually faster.
    fn vertices_around_face(&self, face: FaceHandle) -> DynList<'_, VertexHandle>;

    /// Checks whether the given vertex is adjacent to the given face.
    fn is_vertex_of_face(&self, vertex: VertexHandle, face: FaceHandle) -> bool {
        self.vertices_around_face(face).any(|v| v == vertex)
    }
}

/// Meshes with *O*(1) face-to-face neighborhood information.
pub trait FacesAroundFace: Mesh {
    /// Returns the faces around the given triangular face in front-face CCW
    /// order.
    fn faces_around_triangle(&self, face: FaceHandle) -> TriList<FaceHandle>
    where
        Self: TriMesh;

    /// Returns the faces around the given face in front-face CCW order.
    ///
    /// If you are dealing with a triangular mesh, rather use
    /// [`faces_around_triangle`][FacesAroundFace::faces_around_triangle]
    /// instead as it's usually faster.
    fn faces_around_face(&self, face: FaceHandle) -> DynList<'_, FaceHandle>;

    /// Checks whether the two given faces share an edge (are "adjacent" to one
    /// another).
    fn are_faces_adjacent(&self, a: FaceHandle, b: FaceHandle) -> bool {
        self.faces_around_face(a).any(|f| f == b)
    }
}

/// Meshes with *O*(1) vertex-to-face neighborhood information.
pub trait FacesAroundVertex: Mesh {
    /// Returns a list of all faces adjacent to the given vertex.
    ///
    /// The faces are listed in front-face CW (clockwise) order.
    fn faces_around_vertex(&self, vertex: VertexHandle) -> DynList<'_, FaceHandle>;
}

/// Meshes with *O*(1) vertex-to-vertex neighborhood information.
pub trait VerticesAroundVertex: Mesh {
    /// Returns a list of all faces adjacent to the given vertex.
    ///
    /// The faces are listed in front-face CW (clockwise) order.
    fn vertices_around_vertex(&self, vertex: VertexHandle) -> DynList<'_, VertexHandle>;
}
