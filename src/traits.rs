use crate::{
    handle::{Handle, hsize, FaceHandle, VertexHandle},
    refs::{ElementRef, ElementRefMut, FaceRef, VertexRef},
    util::{DynList, TriList},
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
    fn num_vertices(&self) -> hsize;

    /// Returns an iterator over the handles of all vertices in this mesh.
    ///
    /// Note that this iterator only yields the handles. To get an iterator
    /// over `VertexRef`s, use [`vertices()`][Mesh::vertices], which is often
    /// more useful.
    ///
    /// The order of these vertices is unspecified, but each vertex is yielded
    /// by the iterator exactly once.
    fn vertex_handles(&self) -> Box<dyn Iterator<Item = VertexHandle> + '_>;



    /// Checks if the given vertex handle refers to a valid vertex of this
    /// mesh.
    fn contains_vertex(&self, vertex: VertexHandle) -> bool;

    // TODO: visit_mut
    // TODO: iterator over handles
    // TODO: mutable iterator?


    // ===== Faces ===========================================================
    /// Returns the number of faces in this mesh.
    fn num_faces(&self) -> hsize;

    /// Returns an iterator over the handles of all faces in this mesh.
    ///
    /// Note that this iterator only yields the handles. To get an iterator
    /// over `VertexRef`s, use [`faces()`][Mesh::faces], which is often more
    /// useful.
    ///
    /// The order of these faces is unspecified, but each vertex is yielded by
    /// the iterator exactly once.
    fn face_handles(&self) -> Box<dyn Iterator<Item = FaceHandle> + '_>;

    /// Checks if the given face handle refers to a valid face of this mesh.
    fn contains_face(&self, face: FaceHandle) -> bool;

    // TODO: visit_mut
    // TODO: iterator over handles
    // TODO: mutable iterator?

    // ===== Provided methods ================================================
    /// Returns an `ElementRef` with the given handle referencing this mesh.
    fn get_ref<H: Handle>(&self, handle: H) -> ElementRef<'_, H, Self> {
        ElementRef::new(self, handle)
    }

    /// Returns an `ElementRefMut` with the given handle referencing this mesh.
    fn get_ref_mut<H: Handle>(&mut self, handle: H) -> ElementRefMut<'_, H, Self> {
        ElementRefMut::new(self, handle)
    }

    /// Returns an iterator over all vertices in this mesh.
    ///
    /// This iterator yields `VertexRef`s. If you are only interested in the
    /// handle, use [`vertex_handles()`][Mesh::vertex_handles].
    fn vertices(&self) -> Box<dyn Iterator<Item = VertexRef<'_, Self>> + '_> {
        Box::new(self.vertex_handles().map(move |h| ElementRef::new(self, h))
    }

    /// Returns an iterator over all faces in this mesh.
    ///
    /// This iterator yields `FaceRef`s. If you are only interested in the
    /// handle, use [`face_handles()`][Mesh::face_handles].
    fn faces(&self) -> Box<dyn Iterator<Item = FaceRef<'_, Self>> + '_> {
        Box::new(self.face_handles().map(move |h| ElementRef::new(self, h))
    }
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
    /// # //TODO: make it run
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

    /// Removes all vertices of this mesh. This can be more efficient than
    /// calling [`remvove_vertex`](TODO) for each vertex individually.
    ///
    /// The caller of this method has to make sure that all vertices of this
    /// mesh are unconnected. In other words, there must not be any edges or
    /// faces in this mesh. Otherwise this function will panic.
    ///
    /// ## Example
    ///
    /// ```
    /// # //TODO: make it run
    /// use lox::MeshMut;
    ///
    /// fn add_two_vertices(mesh: &mut impl MeshMut) {
    ///     mesh.add_vertex();
    ///     mesh.add_vertex();
    ///     assert_eq!(mesh.num_vertices(), 2);
    ///
    ///     mesh.remove_all_vertices();
    ///     assert_eq!(mesh.num_vertices(), 0);
    /// }
    /// ```
    ///
    /// If the mesh contains faces, this method will panic:
    ///
    /// ```
    /// # //TODO: make it run
    /// use lox::TriMeshMut;
    ///
    /// fn add_two_vertices(mesh: &mut impl TriMeshMut) {
    ///     let a = mesh.add_vertex();
    ///     let b = mesh.add_vertex();
    ///     let c = mesh.add_vertex();
    ///     mesh.add_face([a, b, c]);
    ///
    ///     // Panics
    ///     mesh.remove_all_vertices();
    /// }
    /// ```
    fn remove_all_vertices(&mut self);

    /// Removes all faces of this mesh. This can be more efficient than calling
    /// [`remvove_face`](TODO) for each face individually.
    fn remove_all_faces(&mut self);

    /// Reserves memory for `count` additional vertices.
    ///
    /// This is just an optimization that can reduce the number allocations
    /// done by this data structure. But this function might also do nothing
    /// (that's exactly what the provided default implementation does).
    fn reserve_for_vertices(&mut self, _count: hsize) {}

    /// Reserves memory for `count` additional faces.
    ///
    /// This is just an optimization that can reduce the number allocations
    /// done by this data structure. But this function might also do nothing
    /// (that's exactly what the provided default implementation does).
    fn reserve_for_faces(&mut self, _count: hsize) {}
}

/// A triangular mesh: all faces are triangles.
pub trait TriMesh: Mesh {}

/// A mesh that allows additions of triangular faces.
pub trait TriMeshMut: MeshMut {
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

/// Marker trait: implemented by meshes that support multi fan-blade vertices
/// (technically not 2-manifold).
///
/// TODO: more explanation and image.
pub trait SupportsMultiBlade: MeshMut {}


// ===========================================================================
// ===== Implementations
// ===========================================================================
impl Empty for () {
    fn empty() -> Self {
        ()
    }
}

impl<T> Empty for Vec<T> {
    fn empty() -> Self {
        Vec::new()
    }
}

impl<T> Empty for Option<T> {
    fn empty() -> Self {
        None
    }
}
