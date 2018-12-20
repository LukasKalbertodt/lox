use crate::{
    handle::{DefaultInt, FaceHandle, VertexHandle},
    refs::{FaceRef, VertexRef},
    util::DynList,
};


/// Types that have a notion of “being empty” and can create such an empty
/// instance.
///
/// This is very similar to `Default` from the standard library, but makes it
/// explicit that the returned instance is *empty* and not just any default
/// instance.
pub trait Empty {
    /// Returns an empty value of this type.
    fn empty() -> Self;
}

/// Some kind of polygon mesh.
pub trait Mesh: Empty {
    // ===== Vertices ========================================================
    /// Returns the number of vertices in this mesh.
    fn num_vertices(&self) -> DefaultInt;

    /// Returns an iterator over all vertices in this mesh. The order of these
    /// vertices is unspecified, but each vertex is yielded by the iterator
    /// exactly once.
    fn vertices(&self) -> Box<dyn Iterator<Item = VertexRef<'_, Self>> + '_>;

    // TODO: visit_mut
    // TODO: iterator over handles
    // TODO: mutable iterator?


    // ===== Faces ===========================================================
    /// Returns the number of faces in this mesh.
    fn num_faces(&self) -> DefaultInt;

    /// Returns an iterator over all faces in this mesh. The order of these
    /// faces is unspecified, but each vertex is yielded by the iterator
    /// exactly once.
    fn faces(&self) -> Box<dyn Iterator<Item = FaceRef<'_, Self>> + '_>;

    // TODO: visit_mut
    // TODO: iterator over handles
    // TODO: mutable iterator?
}

/// Some kind of polygon mesh that allows modifications.
pub trait MeshMut: Mesh {
    /// Adds a new, unconnected vertex to the mesh and returns the handle
    /// representing that vertex.
    ///
    /// Note that this method doesn't require you to pass any properties (like
    /// the vertex position) as meshes only store the connectivity. All
    /// properties are stored in external property maps. See [module
    /// `map`][crate::map].
    ///
    /// ## Example
    ///
    /// ```
    /// use lox::MeshMut;
    ///
    /// fn add_two_vertices(mesh: &mut impl MeshMut) {
    ///     let a = mesh.add_vertex();
    ///     let b = mesh.add_vertex();
    ///
    ///     assert_ne!(a, b);
    /// }
    /// ```
    fn add_vertex(&mut self) -> VertexHandle;
}

/// A triangular mesh: all faces are triangles.
pub trait TriMesh: Mesh {}

/// A triangular mesh that allows modifications.
pub trait TriMeshMut: TriMesh + MeshMut {
    /// Adds a new triangular face defined by the three vertices to this mesh
    /// and returns the handle representing that face.
    ///
    /// The vertices have to be given in front-face CCW (counterclockwise)
    /// order. This means: if you look at front of the face you want to create
    /// (the face's normal is pointing to you), the vertices should appear in
    /// CCW order. Or in more mathy terms: the face's normal is equal to `(v0 -
    /// v1) ⨯ (v0 - v2)` in the right-handed coordinate system (where `⨯` is
    /// cross-product).
    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle;
}

/// Meshes with *O*(1) face-to-vertex neighorhood information.
pub trait TriVerticesOfFace: TriMesh {
    /// Returns the vertices of the given triangular face in face-front CCW
    /// order.
    fn vertices_of_face(&self, face: FaceHandle) -> [VertexHandle; 3];

    /// Checks whether the given vertex is adjacent to the given face.
    fn is_vertex_of_face(&self, vertex: VertexHandle, face: FaceHandle) -> bool {
        self.vertices_of_face(face).contains(&vertex)
    }
}
/// Meshes with *O*(1) vertex-to-face neighorhood information.
pub trait FacesAroundVertex: Mesh {
    /// Returns a list of all faces adjacent to the given vertex.
    ///
    /// The faces are listed in front-face CW (clockwise) order.
    fn faces_around_vertex(
        &self,
        vertex: VertexHandle,
    ) -> Box<dyn DynList<Item = FaceHandle> + '_>;
}

/// Meshes with *O*(1) vertex-to-vertex neighorhood information.
pub trait VerticesAroundVertex: Mesh {
    /// Returns a list of all faces adjacent to the given vertex.
    ///
    /// The faces are listed in front-face CW (clockwise) order.
    fn vertices_around_vertex(
        &self,
        vertex: VertexHandle,
    ) -> Box<dyn DynList<Item = VertexHandle> + '_>;
}
