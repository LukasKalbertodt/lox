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
pub trait TriVerticesOfFace: TriMesh {
    /// Returns the vertices of the given triangular face in front-face CCW
    /// order.
    fn vertices_of_face(&self, face: FaceHandle) -> [VertexHandle; 3];

    /// Checks whether the given vertex is adjacent to the given face.
    fn is_vertex_of_face(&self, vertex: VertexHandle, face: FaceHandle) -> bool {
        self.vertices_of_face(face).contains(&vertex)
    }
}

/// Meshes with *O*(1) face-to-face neighborhood information.
pub trait TriFacesAroundFace: TriMesh {
    /// Returns the faces around the given triangular face in front-face CCW
    /// order.
    fn faces_around_face(&self, face: FaceHandle) -> TriList<FaceHandle>;

    fn are_adjacent_faces(&self, a: FaceHandle, b: FaceHandle) -> bool {
        self.faces_around_face(a).contains(&b)
    }
}

/// Meshes with *O*(1) vertex-to-face neighborhood information.
pub trait FacesAroundVertex: Mesh {
    /// Returns a list of all faces adjacent to the given vertex.
    ///
    /// The faces are listed in front-face CW (clockwise) order.
    fn faces_around_vertex(
        &self,
        vertex: VertexHandle,
    ) -> Box<dyn DynList<Item = FaceHandle> + '_>;
}

/// Meshes with *O*(1) vertex-to-vertex neighborhood information.
pub trait VerticesAroundVertex: Mesh {
    /// Returns a list of all faces adjacent to the given vertex.
    ///
    /// The faces are listed in front-face CW (clockwise) order.
    fn vertices_around_vertex(
        &self,
        vertex: VertexHandle,
    ) -> Box<dyn DynList<Item = VertexHandle> + '_>;
}
