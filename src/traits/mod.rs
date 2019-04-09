use std::marker::PhantomData;

use crate::{
    handle::{Handle, hsize, FaceHandle, VertexHandle, EdgeHandle},
    mesh::{ElementRefIter, HandleIter, HandleIterMut},
    refs::{ElementRef, ElementRefMut},
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

    /// Returns the next handle of an existing vertex with an index ≥ `start`'s
    /// index, or `None` if there is no such handle.
    ///
    /// This is a low level building block for iteration. As a user of this
    /// library, you usually don't want to use this method directly.
    ///
    /// Example: assume a mesh contains three vertices, with the handles with
    /// indices 0, 1 and 3. Then this method returns the following values:
    /// - `next_vertex_handle_from(VertexHandle(0))` → `Some(VertexHandle(0))`
    /// - `next_vertex_handle_from(VertexHandle(1))` → `Some(VertexHandle(1))`
    /// - `next_vertex_handle_from(VertexHandle(2))` → `Some(VertexHandle(3))`
    /// - `next_vertex_handle_from(VertexHandle(3))` → `Some(VertexHandle(3))`
    /// - `next_vertex_handle_from(VertexHandle(4))` → `None`
    ///
    /// In particular, this implies the following properties:
    /// - If a vertex exists, calling this method with its handle will return
    ///   the same handle.
    /// - Using this for simple iteration (starting at 0, each consecutive call
    ///   with the last returned handle + 1, stop at `None`), each vertex
    ///   handle is returned exactly once.
    fn next_vertex_handle_from(&self, start: VertexHandle) -> Option<VertexHandle>;

    /// Returns the vertex handle for an existing vertex with the highest
    /// index, or `None` if there are no vertices in the mesh.
    ///
    /// This is a low level building block. As a user of this library, you
    /// probably don't want to use this method directly.
    fn last_vertex_handle(&self) -> Option<VertexHandle>;

    // TODO: visit_mut
    // TODO: mutable iterator?


    // ===== Faces ===========================================================
    /// Returns the number of faces in this mesh.
    fn num_faces(&self) -> hsize;

    /// Returns the next handle of an existing face with an index ≥ `start`'s
    /// index, or `None` if there is no such handle.
    ///
    /// This is a low level building block for iteration. As a user of this
    /// library, you usually don't want to use this method directly.
    ///
    /// See the documentation of [`Mesh::next_vertex_handle_from`] for more
    /// information. This method works exactly like that, but for faces.
    fn next_face_handle_from(&self, start: FaceHandle) -> Option<FaceHandle>;

    /// Returns the face handle for an existing face with the highest index, or
    /// `None` if there are no vertices in the mesh.
    ///
    /// This is a low level building block. As a user of this library, you
    /// probably don't want to use this method directly.
    fn last_face_handle(&self) -> Option<FaceHandle>;


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

    /// Checks if the given vertex handle refers to a valid vertex of this
    /// mesh.
    fn contains_vertex(&self, vertex: VertexHandle) -> bool {
        self.next_vertex_handle_from(vertex) == Some(vertex)
    }

    /// Returns an iterator over the handles of all vertices in this mesh.
    ///
    /// Note that this iterator only yields the handles. To get an iterator
    /// over `VertexRef`s, use [`vertices()`][Mesh::vertices], which is often
    /// more useful.
    ///
    /// The order of these vertices is unspecified, but each vertex is yielded
    /// by the iterator exactly once.
    fn vertex_handles(&self) -> HandleIter<'_, Self, VertexHandle> {
        HandleIter::new(self)
    }

    /// Returns an iterator over the handles of all vertices which can return a
    /// mutable reference to the mesh. This is useful when it is necessary to
    /// mutate the mesh while iterating.
    ///
    /// Using this iterator is tricky, so please see the documentation of
    /// [`HandleIterMut`] for more information.
    fn vertex_handles_mut(&mut self) -> HandleIterMut<'_, Self, VertexHandle> {
        HandleIterMut::<'_, Self, VertexHandle>::new(self)
    }

    /// Returns an iterator over all vertices in this mesh.
    ///
    /// This iterator yields `VertexRef`s. If you are only interested in the
    /// handle, use [`vertex_handles()`][Mesh::vertex_handles].
    fn vertices(&self) -> ElementRefIter<'_, Self, VertexHandle> {
        ElementRefIter::new(self)
    }


    /// Checks if the given face handle refers to a valid face of this mesh.
    fn contains_face(&self, face: FaceHandle) -> bool {
        self.next_face_handle_from(face) == Some(face)
    }

    /// Returns an iterator over the handles of all faces in this mesh.
    ///
    /// Note that this iterator only yields the handles. To get an iterator
    /// over `VertexRef`s, use [`faces()`][Mesh::faces], which is often more
    /// useful.
    ///
    /// The order of these faces is unspecified, but each vertex is yielded by
    /// the iterator exactly once.
    fn face_handles(&self) -> HandleIter<'_, Self, FaceHandle> {
        HandleIter::new(self)
    }

    /// Returns an iterator over the handles of all faces which can return a
    /// mutable reference to the mesh. This is useful when it is necessary to
    /// mutate the mesh while iterating.
    ///
    /// Using this iterator is tricky, so please see the documentation of
    /// [`HandleIterMut`] for more information.
    fn face_handles_mut(&mut self) -> HandleIterMut<'_, Self, FaceHandle> {
        HandleIterMut::<'_, Self, FaceHandle>::new(self)
    }

    /// Returns an iterator over all faces in this mesh.
    ///
    /// This iterator yields `FaceRef`s. If you are only interested in the
    /// handle, use [`face_handles()`][Mesh::face_handles].
    fn faces(&self) -> ElementRefIter<'_, Self, FaceHandle> {
        ElementRefIter::new(self)
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

    // TODO: default impl this with `remove_face`
    fn split_face(&mut self, f: FaceHandle) -> VertexHandle;
}

/// A mesh that has explicit edges. This allows to store per-edge attributes.
pub trait EdgeMesh: Mesh {
    /// Returns the number of edges in this mesh.
    fn num_edges(&self) -> hsize;

    /// Returns the next handle of an existing edge with an index ≥ `start`'s
    /// index, or `None` if there is no such handle.
    ///
    /// This is a low level building block for iteration. As a user of this
    /// library, you usually don't want to use this method directly.
    ///
    /// See the documentation of [`Mesh::next_vertex_handle_from`] for more
    /// information. This method works exactly like that, but for edges.
    fn next_edge_handle_from(&self, start: EdgeHandle) -> Option<EdgeHandle>;

    /// Returns the edge handle for an existing edge with the highest index, or
    /// `None` if there are no vertices in the mesh.
    ///
    /// This is a low level building block. As a user of this library, you
    /// probably don't want to use this method directly.
    fn last_edge_handle(&self) -> Option<EdgeHandle>;


    /// Returns an iterator over the handles of all edges in this mesh.
    ///
    /// Note that this iterator only yields the handles. To get an iterator
    /// over `EdgeRef`s, use [`edges()`][EdgeMesh::edges], which is often more
    /// useful.
    ///
    /// The order of the edges is unspecified, but each edge is yielded by the
    /// iterator exactly once.
    fn edge_handles(&self) -> HandleIter<'_, Self, EdgeHandle> {
        HandleIter::new(self)
    }

    /// Returns an iterator over the handles of all edges which can return a
    /// mutable reference to the mesh. This is useful when it is necessary to
    /// mutate the mesh while iterating.
    ///
    /// Using this iterator is tricky, so please see the documentation of
    /// [`HandleIterMut`] for more information.
    fn edge_handles_mut(&mut self) -> HandleIterMut<'_, Self, EdgeHandle> {
        HandleIterMut::<'_, Self, EdgeHandle>::new(self)
    }

    /// Checks if the given edge handle refers to a valid edge of this mesh.
    fn contains_edge(&self, edge: EdgeHandle) -> bool {
        self.next_edge_handle_from(edge) == Some(edge)
    }

    /// Returns an iterator over all edges in this mesh.
    ///
    /// This iterator yields `EdgeRef`s. If you are only interested in the
    /// handle, use [`edge_handles()`][EdgeMesh::edge_handles].
    fn edges(&self) -> ElementRefIter<'_, Self, EdgeHandle> {
        ElementRefIter::new(self)
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
pub trait PolyMeshMut: MeshMut<FaceKind = PolyFaces> {
    /// Adds a new face to this mesh and returns the handle representing that
    /// face.
    ///
    /// The `vertices` have to be given in front-face CCW (counterclockwise)
    /// order. This means: if you look at front of the face you want to create
    /// (the face's normal is pointing to you), the vertices should appear in
    /// CCW order.
    fn add_face(&mut self, vertices: &[VertexHandle]) -> FaceHandle;
}

pub trait TriEdgeMeshMut: EdgeMesh + MeshMut<FaceKind = TriFaces> {
    fn flip_edge(&mut self, edge: EdgeHandle);
}



// ===========================================================================
// ===== Implementations
// ===========================================================================
macro_rules! impl_empty_via_default {
    ($( { $($impl_header:tt)+ } ,)*) => {
        $(
            $($impl_header)* {
                fn empty() -> Self {
                    Self::default()
                }
            }
        )*
    }
}

impl_empty_via_default!(
    { impl Empty for () },
    { impl<T: ?Sized> Empty for PhantomData<T> },
    { impl<T> Empty for Option<T> },
    { impl Empty for String },
    { impl<T> Empty for Vec<T> },
    { impl<T: Ord> Empty for std::collections::BTreeSet<T> },
    { impl<T: Eq + std::hash::Hash> Empty for std::collections::HashSet<T> },
    { impl<T> Empty for std::collections::LinkedList<T> },
    { impl<T> Empty for std::collections::VecDeque<T> },
    { impl<K: Ord, V> Empty for std::collections::BTreeMap<K, V> },
    { impl<K: Eq + std::hash::Hash, V> Empty for std::collections::HashMap<K, V> },
);

impl<T: Empty> Empty for Box<T> {
    fn empty() -> Self {
        Box::new(T::empty())
    }
}

impl<A: Empty> Empty for (A,) {
    fn empty() -> Self { (A::empty(),) }
}
impl<A: Empty, B: Empty> Empty for (A, B) {
    fn empty() -> Self { (A::empty(), B::empty()) }
}
impl<A: Empty, B: Empty, C: Empty> Empty for (A, B, C) {
    fn empty() -> Self { (A::empty(), B::empty(), C::empty()) }
}
impl<A: Empty, B: Empty, C: Empty, D: Empty> Empty for (A, B, C, D) {
    fn empty() -> Self { (A::empty(), B::empty(), C::empty(), D::empty()) }
}

impl<T: Empty> Empty for [T; 0] {
    fn empty() -> Self { [] }
}
impl<T: Empty> Empty for [T; 1] {
    fn empty() -> Self { [T::empty()] }
}
impl<T: Empty> Empty for [T; 2] {
    fn empty() -> Self { [T::empty(), T::empty()] }
}
impl<T: Empty> Empty for [T; 3] {
    fn empty() -> Self { [T::empty(), T::empty(), T::empty()] }
}
impl<T: Empty> Empty for [T; 4] {
    fn empty() -> Self { [T::empty(), T::empty(), T::empty(), T::empty()] }
}
