use std::fmt;

use leer::Empty;
use crate::{
    Handle, hsize, FaceHandle, VertexHandle, EdgeHandle,
    mesh::{ElementRefIter, HandleIter, HandleIterMut, SplitEdgeWithFacesResult},
    refs::ElementRef,
    util::{Bool, True, False, TriList, DiList},
};
use super::{FaceKind, TriFaces, PolyFaces};


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

    /// Denotes whether this mesh is always orientable *or* potentially
    /// non-orientable.
    ///
    /// Most meshes that represent real-life objects are orientable, e.g. a sphere
    /// or a cube. A famous non-orientable surface is the Möbius strip. From
    /// [Wikipedia on orientability][wiki-orientable]:
    ///
    /// > Orientability is a property of surfaces in Euclidean space that measures
    /// > whether it is possible to make a consistent choice of surface normal
    /// > vector at every point.
    ///
    /// This is set to `True` by types which guarantee that they represent an
    /// orientable mesh. For example, the *half edge mesh* can only represent
    /// orientable meshes. On the other hand, the *shared vertex mesh* does not
    /// care about the orientability of meshes and can thus store
    /// non-orientable ones. It sets this to `False` as it cannot guarantee
    /// anything about orientability.
    ///
    /// Note however, that a `False` value does *not* mean the mesh is always
    /// non-orientable. A shared vertex mesh can *also* represent orientable
    /// meshes.
    ///
    /// Also see the [`Orientable`] and [`NonOrientable`] traits.
    ///
    ///
    /// [wiki-orientable]: https://en.wikipedia.org/wiki/Orientability
    type Orientable: Bool;

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

    /// Returns `true` if `Self` is a `TriMesh`, meaning that all faces are
    /// triangles. If `false` is returned, that means that you can't assume all
    /// faces are triangles, but it could still be the case.
    fn is_tri_mesh(&self) -> bool {
        Self::FaceKind::ONLY_TRIANGLES
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
    /// use lox::core::MeshMut;
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
    /// The function of this method has the same semantics as
    /// [`MeshMut::add_face`]. The only difference is that this is specialized
    /// for triangles. See [`MeshMut::add_face`]'s documentation for important
    /// information about the usage of this method.
    fn add_triangle(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle;


    /// Adds a new face to this mesh and returns the handle representing that
    /// face.
    ///
    /// The `vertices` have to be given in front-face CCW (counterclockwise)
    /// order. This means: if you look at front of the face you want to create
    /// (the face's normal is pointing to you), the vertices should appear in
    /// CCW order.
    ///
    /// You have to respect `Mesh::Orientable` when adding faces. If it is
    /// `True`, you must not add faces in a way that creates a non-orientable
    /// mesh.
    ///
    ///
    /// # Panics
    ///
    /// This method panics ...
    /// - ... if `vertices.len() < 3`: faces must have at least three vertices!
    /// - ... if any given vertex handle is invalid, i.e. does not refer to a
    ///   vertex in this mesh.
    /// - ... TODO
    ///
    ///
    /// TODO: what if face already there?
    /// TODO: panics if vertex handles not unique
    /// TODO: return result for certain problems, like non orientability or so?
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
    /// use lox::core::MeshMut;
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
    /// use lox::core::{TriMesh, MeshMut};
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


/// Marker trait for mesh types that guarantee orientable meshes.
///
/// This trait is automatically implemented for all types that implement
/// `Mesh<Orientable = True>`. See [`Mesh::Orientable`] for more information.
pub trait Orientable: Mesh {}

impl<M: Mesh<Orientable = True>> Orientable for M {}


/// Marker trait for mesh types that allow non-orientable meshes.
///
/// This trait is automatically implemented for all types that implement
/// `Mesh<Orientable = False>`. See [`Mesh::Orientable`] for more information.
pub trait NonOrientable: Mesh {}

impl<M: Mesh<Orientable = False>> NonOrientable for M {}

/// Marker trait for meshes that support multi fan-blade vertices
/// (technically not 2-manifold).
///
/// TODO: more explanation and image.
pub trait SupportsMultiBlade: MeshMut {}


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

    type VerticesAroundFaceIter<'s>: Iterator<Item = VertexHandle> where Self: 's;

    /// Returns the vertices around the given face in front-face CCW order.
    ///
    /// If you are dealing with a triangular mesh, rather use
    /// [`vertices_around_triangle`][BasicAdj::vertices_around_triangle]
    /// instead as it's usually faster.
    fn vertices_around_face(&self, face: FaceHandle) -> Self::VerticesAroundFaceIter<'_>;

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
    ///
    /// TODO: explain that there can be duplicates in neighbor faces. We are
    /// basically iterating over edges and returning their other face.
    fn faces_around_triangle(&self, face: FaceHandle) -> TriList<FaceHandle>
    where
        Self: TriMesh;

    type FacesAroundFaceIter<'s>: Iterator<Item = FaceHandle> where Self: 's;

    /// Returns the faces around the given face in front-face CCW order.
    ///
    /// If you are dealing with a triangular mesh, rather use
    /// [`faces_around_triangle`][FullAdj::faces_around_triangle] instead as
    /// it's usually faster.
    ///
    /// TODO: explain that there can be duplicates in neighbor faces. We are
    /// basically iterating over edges and returning their other face.
    fn faces_around_face(&self, face: FaceHandle) -> Self::FacesAroundFaceIter<'_>;

    type FacesAroundVertexIter<'s>: Iterator<Item = FaceHandle> where Self: 's;

    /// Returns a list of all faces adjacent to the given vertex.
    ///
    /// The faces are listed in front-face CW (clockwise) order.
    fn faces_around_vertex(&self, vertex: VertexHandle) -> Self::FacesAroundVertexIter<'_>;

    type VerticesAroundVertexIter<'s>: Iterator<Item = VertexHandle> where Self: 's;

    /// Returns a list of all faces adjacent to the given vertex.
    ///
    /// The faces are listed in front-face CW (clockwise) order.
    fn vertices_around_vertex(&self, vertex: VertexHandle) -> Self::VerticesAroundVertexIter<'_>;


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
    /// adjacent vertices *or* if it is an isolated vertex.
    ///
    /// *Note to implementors*: you should usually overwrite this method, as
    /// the default implementation is fairly slow.
    fn is_boundary_vertex(&self, vertex: VertexHandle) -> bool {
        self.faces_around_vertex(vertex).count() != self.vertices_around_vertex(vertex).count()
            || self.is_isolated_vertex(vertex)
    }

    /// Checks if the given vertex is isolated, meaning that it has no adjacent
    /// faces, edges or vertices.
    fn is_isolated_vertex(&self, vertex: VertexHandle) -> bool {
        self.vertices_around_vertex(vertex).next().is_none()
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
    // Most not change! Always same order for one given edge.
    fn endpoints_of_edge(&self, edge: EdgeHandle) -> [VertexHandle; 2];
    fn faces_of_edge(&self, edge: EdgeHandle) -> DiList<FaceHandle>;
    // TODO

    type EdgesAroundVertexIter<'s>: Iterator<Item = EdgeHandle> where Self: 's;
    fn edges_around_vertex(&self, vertex: VertexHandle) -> Self::EdgesAroundVertexIter<'_>;

    type EdgesAroundFaceIter<'s>: Iterator<Item = EdgeHandle> where Self: 's;
    fn edges_around_face(&self, face: FaceHandle) -> Self::EdgesAroundFaceIter<'_>;

    fn edges_around_triangle(&self, face: FaceHandle) -> [EdgeHandle; 3]
    where
        Self: TriMesh;

    fn is_boundary_edge(&self, edge: EdgeHandle) -> bool {
        self.faces_of_edge(edge).len() != 2
    }

    // TODO: remove `_vertices` from the name? For faces, there can be multiple
    // shared edges.
    fn edge_between_vertices(&self, a: VertexHandle, b: VertexHandle) -> Option<EdgeHandle> {
        self.edges_around_vertex(a)
            .find(|&e| self.endpoints_of_edge(e).contains(&b))
    }
}
