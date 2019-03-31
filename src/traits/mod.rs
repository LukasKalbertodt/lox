use crate::{
    handle::{Handle, hsize, FaceHandle, VertexHandle, EdgeHandle},
    refs::{ElementRef, ElementRefMut, EdgeRef, FaceRef, VertexRef},
};
use self::{
    marker::{FaceKind, TriFaces, PolyFaces},
};

pub mod marker;
pub mod adj;




/// Types that have a notion of “being empty” and can create such an empty
/// instance.
///
/// This is very similar to `Default` from the standard library, but makes it
/// explicit that the returned instance is *empty* and not just any default
/// instance.
///
/// This trait is implemented for several standard types.
///
///
/// # Deriving
///
/// This trait can automatically be derived:
///
/// ```
/// use lox::Empty; // this imports the custom-derive and not the trait!
///
/// #[derive(Empty)]
/// struct MyStruct {
///     a: Vec<u32>,        // => `vec![]`
///     b: Option<String>,  // => `None`
///     c: (),              // => `()`
/// }
/// ```
///
/// This can only be derived for structs. All struct fields need to implement
/// `Empty` in order for the derive to work. If your struct has generic
/// parameters, they won't be bounded with `Empty` in the generated impl block.
/// This is useful most of the time, because things like `Vec<T>` and
/// `Option<T>` don't require `T: Empty` to implement `Empty`. But this means
/// that you sometimes have to add a global `Empty` bound to your parameter or
/// implement `Empty` manually.
pub trait Empty {
    /// Returns an empty value of this type.
    fn empty() -> Self;
}

/// Some kind of polygon mesh.
///
/// TODO: add info:
/// - rather use `TriMesh` or `PolyMesh` as bound
pub trait Mesh: Empty {
    /// The kind of faces this mesh type can store. Either [`TriFaces`]
    /// or [`PolyFaces`].
    ///
    /// Many data structures are specialized to only work with triangular
    /// faces. This is useful because many operations can be implemented more
    /// efficiently and adjacency information can often be stored with less
    /// memory. But of course, sometimes you need meshes that support
    /// non-triangular faces. And of course, there are data structures for that
    /// as well.
    type FaceKind: FaceKind;

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
        Box::new(self.vertex_handles().map(move |h| ElementRef::new(self, h)))
    }

    /// Returns an iterator over all faces in this mesh.
    ///
    /// This iterator yields `FaceRef`s. If you are only interested in the
    /// handle, use [`face_handles()`][Mesh::face_handles].
    fn faces(&self) -> Box<dyn Iterator<Item = FaceRef<'_, Self>> + '_> {
        Box::new(self.face_handles().map(move |h| ElementRef::new(self, h)))
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
    /// use lox::traits::MeshMut;
    ///
    /// fn add_two_vertices(mesh: &mut impl MeshMut) {
    ///     let a = mesh.add_vertex();
    ///     let b = mesh.add_vertex();
    ///
    ///     assert_ne!(a, b);
    /// }
    /// ```
    fn add_vertex(&mut self) -> VertexHandle;

    /// Adds a new triangular face (defined by the three vertices) to this mesh
    /// and returns the handle representing that face.
    ///
    /// The vertices have to be given in front-face CCW (counterclockwise)
    /// order. This means: if you look at front of the face you want to create
    /// (the face's normal is pointing to you), the vertices should appear in
    /// CCW order. Or in more mathy terms: the face's normal is equal to `(v0 -
    /// v1) ⨯ (v0 - v2)` in the right-handed coordinate system (where `⨯` is
    /// cross-product).
    ///
    /// TODO: what if face already there?
    fn add_triangle(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle;

    /// Removes all vertices of this mesh. This can be more efficient than
    /// calling `remove_vertex` (TODO: link) for each vertex individually.
    ///
    /// The caller of this method has to make sure that all vertices of this
    /// mesh are unconnected. In other words, there must not be any edges or
    /// faces in this mesh. Otherwise this function will panic.
    ///
    /// ## Example
    ///
    /// ```
    /// # //TODO: make it run
    /// use lox::traits::MeshMut;
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
    /// use lox::traits::TriMeshMut;
    ///
    /// fn add_two_vertices(mesh: &mut impl TriMeshMut) {
    ///     let a = mesh.add_vertex();
    ///     let b = mesh.add_vertex();
    ///     let c = mesh.add_vertex();
    ///     mesh.add_triangle([a, b, c]);
    ///
    ///     // Panics
    ///     mesh.remove_all_vertices();
    /// }
    /// ```
    fn remove_all_vertices(&mut self);

    /// Removes all faces of this mesh. This can be more efficient than calling
    /// `remove_face` (TODO: link) for each face individually.
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

/// A mesh that has explicit edges. This allows to store per-edge attributes.
pub trait EdgeMesh: Mesh {
    /// Returns the number of edges in this mesh.
    fn num_edges(&self) -> hsize;

    /// Returns an iterator over the handles of all edges in this mesh.
    ///
    /// Note that this iterator only yields the handles. To get an iterator
    /// over `EdgeRef`s, use [`edges()`][EdgeMesh::edges], which is often more
    /// useful.
    ///
    /// The order of the edges is unspecified, but each edge is yielded by the
    /// iterator exactly once.
    fn edge_handles(&self) -> Box<dyn Iterator<Item = EdgeHandle> + '_>;

    /// Checks if the given edge handle refers to a valid edge of this mesh.
    fn contains_edge(&self, edge: EdgeHandle) -> bool;

    /// Returns an iterator over all edges in this mesh.
    ///
    /// This iterator yields `EdgeRef`s. If you are only interested in the
    /// handle, use [`edge_handles()`][EdgeMesh::edge_handles].
    fn edges(&self) -> Box<dyn Iterator<Item = EdgeRef<'_, Self>> + '_> {
        Box::new(self.edge_handles().map(move |h| ElementRef::new(self, h)))
    }
}

/// A triangular mesh: all faces are triangles.
///
/// A triangular mesh is more restrictive than a poly mesh when adding or
/// modifying faces (faces always have to be triangles), but can be easier to
/// deal with in many situations.
///
/// This trait is automatically implemented for all types with `FaceKind =
/// TriFaces`. As such, this trait is just a convenience alias to allow for
/// more concise trait bounds (i.e. `T: TriMesh` instead of `T: Mesh<FaceKind =
/// TriFaces>`).
pub trait TriMesh: Mesh<FaceKind = TriFaces> {}
impl<T> TriMesh for T where T: Mesh<FaceKind = TriFaces> {}

/// A poly mesh: faces can be arbitrary polygons (different polygons can be in
/// the same mesh).
///
/// A poly mesh is less restrictive than a tri mesh when adding or modifying
/// faces, but can be a lot harder to deal with in many situations.
///
/// This trait is automatically implemented for all types with `FaceKind =
/// PolyFaces`. As such, this trait is just a convenience alias to allow for
/// more concise trait bounds (i.e. `T: TriMesh` instead of `T: Mesh<FaceKind =
/// PolyFaces>`).
pub trait PolyMesh: Mesh<FaceKind = PolyFaces> {}
impl<T> PolyMesh for T where T: Mesh<FaceKind = PolyFaces> {}

/// A triangular mesh that allows modifications. Alias for `MeshMut<FaceKind =
/// TriFaces>`.
pub trait TriMeshMut: MeshMut<FaceKind = TriFaces> {}
impl<T> TriMeshMut for T where T: MeshMut<FaceKind = TriFaces> {}

/// A poly mesh that allows modifications. Alias for `MeshMut<FaceKind =
/// PolyFaces>`.
pub trait PolyMeshMut: MeshMut<FaceKind = PolyFaces> {}
impl<T> PolyMeshMut for T where T: MeshMut<FaceKind = PolyFaces> {}



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
