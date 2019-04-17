//! Contains traits related to **adj**acency information.
//!
//! In almost all mesh algorithms we need some kind of information about the
//! connectivity of the mesh's elements (vertex, edge, face). TODO.

// TODO: ideas
//
// - Rename all traits to `XToX` (where X = {E | F | V})
// - define some collection traits (`FullConnectivity`) or so

use crate::{
    handle::{EdgeHandle, FaceHandle, VertexHandle},
    util::{DiList, DynList, TriList},
};
use super::{TriMesh, Mesh, EdgeMesh};

/// Meshes with *O*(1) face-to-vertex adjacency information.
///
/// This is the most important type of connectivity information since it's
/// needed for most kinds of rendering and for writing to files (since most
/// files are a simple *shared vertex mesh*). Almost all mesh data structures
/// (all of the ones in this library) offer this kind of information.
pub trait BasicAdj: Mesh {
    /// Returns the vertices of the given triangular face in front-face CCW
    /// order.
    fn vertices_around_triangle(&self, face: FaceHandle) -> [VertexHandle; 3]
    where
        Self: TriMesh;

    /// Returns the vertices around the given face in front-face CCW order.
    ///
    /// If you are dealing with a triangular mesh, rather use
    /// [`vertices_around_triangle`][BasicAdj::vertices_around_triangle]
    /// instead as it's usually faster.
    fn vertices_around_face(&self, face: FaceHandle) -> DynList<'_, VertexHandle>;

    /// Checks whether the given vertex is adjacent to the given face.
    fn is_vertex_around_face(&self, vertex: VertexHandle, face: FaceHandle) -> bool {
        self.vertices_around_face(face).any(|v| v == vertex)
    }
}

/// Meshes with full *O*(1) adjacency information between vertices and faces.
///
/// This includes:
/// - Face to vertex (from [`BasicAdj`])
/// - Face to face
/// - Vertex to vertex
/// - Vertex to face
/// - Is a vertex/face on the boundary?
pub trait FullAdj: BasicAdj {
    /// Returns the faces around the given triangular face in front-face CCW
    /// order.
    fn faces_around_triangle(&self, face: FaceHandle) -> TriList<FaceHandle>
    where
        Self: TriMesh;

    /// Returns the faces around the given face in front-face CCW order.
    ///
    /// If you are dealing with a triangular mesh, rather use
    /// [`faces_around_triangle`][FullAdj::faces_around_triangle] instead as
    /// it's usually faster.
    fn faces_around_face(&self, face: FaceHandle) -> DynList<'_, FaceHandle>;

    /// Returns a list of all faces adjacent to the given vertex.
    ///
    /// The faces are listed in front-face CW (clockwise) order.
    fn faces_around_vertex(&self, vertex: VertexHandle) -> DynList<'_, FaceHandle>;

    /// Returns a list of all faces adjacent to the given vertex.
    ///
    /// The faces are listed in front-face CW (clockwise) order.
    fn vertices_around_vertex(&self, vertex: VertexHandle) -> DynList<'_, VertexHandle>;


    /// Checks if the given face lies on a boundary. A face is a boundary face
    /// if the number of adjacent faces does not match the number of adjacent
    /// vertices.
    ///
    /// *Note to implementors*: you should usually overwrite this method, as
    /// the default implementation is fairly slow.
    fn is_boundary_face(&self, face: FaceHandle) -> bool {
        self.faces_around_face(face).count() != self.vertices_around_face(face).count()
    }

    /// Checks if the given vertex lies on a boundary. A vertex is a boundary
    /// vertex if the number of adjacent faces does not match the number of
    /// adjacent vertices.
    ///
    /// *Note to implementors*: you should usually overwrite this method, as
    /// the default implementation is fairly slow.
    fn is_boundary_vertex(&self, vertex: VertexHandle) -> bool {
        self.faces_around_vertex(vertex).count() != self.vertices_around_vertex(vertex).count()
    }

    /// Checks whether the two given faces share an edge (are "adjacent" to one
    /// another).
    fn are_faces_adjacent(&self, a: FaceHandle, b: FaceHandle) -> bool {
        self.faces_around_face(a).any(|f| f == b)
    }

    /// Checks whether the two given faces share an edge (are "adjacent" to one
    /// another).
    fn are_vertices_adjacent(&self, a: VertexHandle, b: VertexHandle) -> bool {
        self.vertices_around_vertex(a).any(|v| v == b)
    }
}

/// Meshes with full *O*(1) adjacency information between vertices, faces *and*
/// edges.
///
/// This includes:
/// - Full vertex/face adjecency information (via [`FullAdj`])
/// - Edge to Vertex
/// - Edge to Face
/// - Vertex to Edge
/// - Face to Edge
pub trait EdgeAdj: FullAdj + EdgeMesh {
    fn endpoints_of_edge(&self, edge: EdgeHandle) -> [VertexHandle; 2];
    fn faces_of_edge(&self, edge: EdgeHandle) -> DiList<FaceHandle>;
    // TODO
}
