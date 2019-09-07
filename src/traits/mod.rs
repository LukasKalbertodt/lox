use std::{
    fmt,
    marker::PhantomData,
};

use crate::{
    handle::{Handle, hsize, FaceHandle, VertexHandle, EdgeHandle},
    mesh::{ElementRefIter, HandleIter, HandleIterMut, SplitEdgeWithFacesResult},
    refs::ElementRef,
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
/// This trait can automatically be derived. See [the derive
/// macro](../derive.Empty.html) for more information on that.
pub trait Empty {
    /// Returns an empty value of this type.
    fn empty() -> Self;
}

/// Some kind of polygon mesh.
///
/// TODO: add info:
/// - rather use `TriMesh` or `PolyMesh` as bound
pub trait Mesh: Empty + fmt::Debug {
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


    // ===== Edges ===========================================================
    /// Returns the number of edges in this mesh.
    fn num_edges(&self) -> hsize
    where
        Self: EdgeMesh;

    /// Returns the next handle of an existing edge with an index ≥ `start`'s
    /// index, or `None` if there is no such handle.
    ///
    /// This is a low level building block for iteration. As a user of this
    /// library, you usually don't want to use this method directly.
    ///
    /// See the documentation of [`Mesh::next_vertex_handle_from`] for more
    /// information. This method works exactly like that, but for edges.
    fn next_edge_handle_from(&self, start: EdgeHandle) -> Option<EdgeHandle>
    where
        Self: EdgeMesh;

    /// Returns the edge handle for an existing edge with the highest index, or
    /// `None` if there are no vertices in the mesh.
    ///
    /// This is a low level building block. As a user of this library, you
    /// probably don't want to use this method directly.
    fn last_edge_handle(&self) -> Option<EdgeHandle>
    where
        Self: EdgeMesh;



    // ===========================================================================================
    // ===== Provided methods
    // ===========================================================================================
    /// Returns an `ElementRef` with the given handle referencing this mesh.
    fn get_ref<H: Handle>(&self, handle: H) -> ElementRef<'_, H, Self> {
        ElementRef::new(self, handle)
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
        HandleIter::<Self, VertexHandle>::new(self)
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
        ElementRefIter::<Self, VertexHandle>::new(self)
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
        HandleIter::<Self, FaceHandle>::new(self)
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
        ElementRefIter::<Self, FaceHandle>::new(self)
    }

    /// Checks if the given edge handle refers to a valid edge of this mesh.
    fn contains_edge(&self, edge: EdgeHandle) -> bool
    where
        Self: EdgeMesh,
    {
        self.next_edge_handle_from(edge) == Some(edge)
    }

    /// Returns an iterator over the handles of all edges in this mesh.
    ///
    /// Note that this iterator only yields the handles. To get an iterator
    /// over `EdgeRef`s, use [`edges()`][Mesh::edges], which is often more
    /// useful.
    ///
    /// The order of the edges is unspecified, but each edge is yielded by the
    /// iterator exactly once.
    fn edge_handles(&self) -> HandleIter<'_, Self, EdgeHandle>
    where
        Self: EdgeMesh,
    {
        HandleIter::<Self, EdgeHandle>::new(self)
    }

    /// Returns an iterator over the handles of all edges which can return a
    /// mutable reference to the mesh. This is useful when it is necessary to
    /// mutate the mesh while iterating.
    ///
    /// Using this iterator is tricky, so please see the documentation of
    /// [`HandleIterMut`] for more information.
    fn edge_handles_mut(&mut self) -> HandleIterMut<'_, Self, EdgeHandle>
    where
        Self: EdgeMesh,
    {
        HandleIterMut::<'_, Self, EdgeHandle>::new(self)
    }

    /// Returns an iterator over all edges in this mesh.
    ///
    /// This iterator yields `EdgeRef`s. If you are only interested in the
    /// handle, use [`edge_handles()`][Mesh::edge_handles].
    fn edges(&self) -> ElementRefIter<'_, Self, EdgeHandle>
    where
        Self: EdgeMesh,
    {
        ElementRefIter::<Self, EdgeHandle>::new(self)
    }

    /// Performs a number of integrity checks on internal data and panics if
    /// something is broken.
    ///
    /// This method is mainly intended for unit tests and similar situations.
    /// This method *should* never panic. If internal data of a mesh data
    /// structure is broken (and this method consequently panics), then that's
    /// an internal bug in `lox` and not the user's fault.
    ///
    /// The default implementation does not perform any checks. It is
    /// recommended for all data structures to override this and perform as
    /// many checks as possible. This method is allowed to have a runtime of
    /// `O(|V| + |E| + |F|)`.
    fn check_integrity(&self) {}
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
    /// TODO: panics if vertex handles not unique, panics if one invalid vertex
    /// handle
    fn add_triangle(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle;


    /// Adds a new face to this mesh and returns the handle representing that
    /// face.
    ///
    /// The `vertices` have to be given in front-face CCW (counterclockwise)
    /// order. This means: if you look at front of the face you want to create
    /// (the face's normal is pointing to you), the vertices should appear in
    /// CCW order.
    ///
    /// TODO: panics if len < 3, panics if vertices invalid
    fn add_face(&mut self, vertices: &[VertexHandle]) -> FaceHandle
    where
        Self: PolyMesh;

    /// Removes the given isolated `vertex` from the mesh. **You have to make
    /// sure that the given vertex is indeed isolated!**
    ///
    /// All implementations which can will check if `vertex` is isolated and
    /// panic if not. However, not all implementations are able to check this
    /// quickly so they will not perform this test. In these cases, calling
    /// this method with a non-isolated vertex will lead to unspecified
    /// behavior of the mesh, mostly resulting in panics in other methods
    /// later. While it will not lead to memory unsafety (as this method is not
    /// `unsafe`), it can still lead to hard to debug bugs.
    fn remove_isolated_vertex(&mut self, vertex: VertexHandle);

    fn remove_face(&mut self, face: FaceHandle);

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
    /// use lox::traits::{TriMesh, MeshMut};
    ///
    /// fn add_two_vertices(mesh: &mut (impl TriMesh + MeshMut)) {
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

    /// Splits the face `f` into k new faces (where k is the valence of `f`) by
    /// inserting a center vertex. This new vertex is returned. This operation
    /// is sometimes called "1-to-n split".
    ///
    /// TODO: nice SVG image (maybe inline?)
    ///
    /// After calling this function, the face `f` might be invalid and you
    /// cannot assume it now refers to one of the new faces.
    ///
    /// # Panics
    ///
    /// Panics if `f` is not a valid face.
    // TODO: default impl this with `remove_face`
    fn split_face(&mut self, f: FaceHandle) -> VertexHandle;

    /// Performs the "edge flip" operation on `e`. Requires `e` to be an
    /// interior edge (i.e. being adjacent to two faces).
    ///
    /// TODO: nice SVG image
    ///
    /// The direction of the "rotation" is not specified, meaning that you
    /// cannot assume which face is on what side. After this function was
    /// called, the handle `e` will remain valid and refers to the now flipped
    /// edge.
    ///
    /// # Panics
    ///
    /// Panics if `e` is not a valid, interior edge.
    // TODO: think about adding these method with a version where you specify
    // two vertices? (e.g. `flip_edge_between`, `split_face_2_to_4(a, b)` and
    // `split_boundary_face_1to2`)
    fn flip_edge(&mut self, e: EdgeHandle)
    where
        Self: EdgeMesh + TriMesh;

    /// TODO
    ///
    /// - edge handle stays valid and is one of the resulting center edges.
    ///   TODO: or make this unspecified? I mean: the user does not gain
    ///   anything from the knowledge that the old handle stays valid, right?
    ///   It's still unspecified on which side it is.
    /// - maybe rename to `split_edge`. There is no other operation that this
    ///   could be confused with. Only problem: with `split_edge` some people
    ///   could think that faces are untouched (only makes sense in a poly
    ///   mesh).
    fn split_edge_with_faces(&mut self, edge: EdgeHandle) -> SplitEdgeWithFacesResult
    where
        Self: EdgeMesh + TriMesh;
}

/// A mesh that has explicit edges. This allows to store per-edge attributes.
///
/// This is just a marker trait. All methods related to edges can be found in
/// [`Mesh`] or [`MeshMut`].
pub trait EdgeMesh: Mesh {}

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
