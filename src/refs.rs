//! Types for references to elements within a mesh.

use crate::{
    prelude::*,
};


/// A reference to an element within a mesh.
///
/// This is just a handle paired with a reference to the mesh associated with
/// that handle.
#[derive(Debug)]
pub struct ElementRef<'a, HandleT: Handle, MeshT: 'a + ?Sized> {
    handle: HandleT,
    mesh: &'a MeshT,
}

/// A reference to an edge within a mesh.
///
/// This is just an edge handle with a reference to the mesh. See
/// [`ElementRef`] for more information.
pub type EdgeRef<'a, MeshT> = ElementRef<'a, EdgeHandle, MeshT>;

/// A reference to a face within a mesh.
///
/// This is just a face handle with a reference to the mesh. See [`ElementRef`]
/// for more information.
pub type FaceRef<'a, MeshT> = ElementRef<'a, FaceHandle, MeshT>;

/// A reference to a vertex within a mesh.
///
/// This is just a vertex handle with a reference to the mesh. See
/// [`ElementRef`] for more information.
pub type VertexRef<'a, MeshT> = ElementRef<'a, VertexHandle, MeshT>;



impl<'a, HandleT: Handle, MeshT: 'a + ?Sized> ElementRef<'a, HandleT, MeshT> {
    pub fn new(mesh: &'a MeshT, handle: HandleT) -> Self {
        Self { mesh, handle }
    }
}

impl<'a, HandleT: Handle, MeshT: 'a> Clone for ElementRef<'a, HandleT, MeshT> {
    fn clone(&self) -> Self {
        Self {
            handle: self.handle,
            mesh: self.mesh,
        }
    }
}

impl<'a, HandleT: Handle, MeshT: 'a> Copy for ElementRef<'a, HandleT, MeshT> {}


impl<'a, HandleT: Handle, MeshT: 'a> ElementRef<'a, HandleT, MeshT> {
    /// Returns the stored handle.
    pub fn handle(&self) -> HandleT {
        self.handle
    }

    /// Returns an immutable reference to the linked mesh.
    pub fn mesh(&self) -> &MeshT {
        self.mesh
    }
}

// ===========================================================================
// ===== With VertexHandle
// ===========================================================================

impl<'a, MeshT: 'a> VertexRef<'a, MeshT> {
    /// Returns an iterator over all ring1 neighbors of this vertex (the
    /// vertices that are directly connected to `self` via an edge).
    ///
    /// This is just a convenience method wrapping [`FullAdj::vertices_around_vertex`].
    /// For more information about guarantees and the order of returned
    /// vertices, take a look at its documentation.
    pub fn ring1_neighbors(&self) -> impl Iterator<Item = VertexRef<'a, MeshT>>
    where
        MeshT: FullAdj,
    {
        let mesh = self.mesh;
        self.mesh.vertices_around_vertex(self.handle)
            .map(move |h| VertexRef::new(mesh, h))
    }

    /// Returns an iterator over all faces adjacent to this vertex.
    ///
    /// This is just a convenience method wrapping [`FullAdj::faces_around_vertex`]. For
    /// more information about guarantees and the order of returned faces, take
    /// a look at its documentation.
    ///
    /// # Examples
    ///
    /// ```
    /// use lox::{
    ///     prelude::*,
    ///     refs::VertexRef,
    ///     ds::FaceDelegateMesh,
    /// };
    ///
    /// //    (A)---(D)
    /// //     | \ Y |
    /// //     |  \  |
    /// //     | X \ |
    /// //     |    \|
    /// //    (B)---(C)
    /// let mut mesh = FaceDelegateMesh::empty();
    /// let va = mesh.add_vertex();
    /// let vb = mesh.add_vertex();
    /// let vc = mesh.add_vertex();
    /// let vd = mesh.add_vertex();
    /// let fx = mesh.add_triangle([va, vc, vb]);
    /// let fy = mesh.add_triangle([va, vd, vc]);
    ///
    /// let v = VertexRef::new(&mesh, va);
    /// let face_handles = v.adjacent_faces()
    ///     .map(|f| f.handle())
    ///     .collect::<Vec<_>>();
    ///
    /// assert_eq!(face_handles.len(), 2);
    /// assert!(face_handles.contains(&fx));
    /// assert!(face_handles.contains(&fy));
    /// ```
    ///
    /// Another example:
    ///
    /// ```
    /// #![feature(proc_macro_hygiene)]
    /// use lox::{
    ///     mesh,
    ///     prelude::*,
    ///     ds::FaceDelegateMesh,
    ///     map::VecMap,
    /// };
    ///
    /// //    (A)---(D)
    /// //     | \ Y | \
    /// //     |  \  |  \
    /// //     | X \ | Z \
    /// //     |    \|    \
    /// //    (B)---(C)---(E)
    /// let mesh = mesh! {
    ///     type: FaceDelegateMesh,
    ///     vertices: [va, vb, vc, vd, ve],
    ///     faces: [
    ///         [va, vc, vb],
    ///         [va, vd, vc],
    ///         [vd, ve, vc],
    ///     ],
    /// };
    ///
    /// // Now we have a prop map storing the number of adjacent faces per vertex.
    /// let number_of_faces = mesh.vertices()
    ///     .map(|v| (v.handle(), v.adjacent_faces().count()))
    ///     .collect::<VecMap<_, _>>();
    /// ```
    ///
    pub fn adjacent_faces(&self) -> impl Iterator<Item = FaceRef<'a, MeshT>>
    where
        MeshT: FullAdj,
    {
        let mesh = self.mesh;
        self.mesh.faces_around_vertex(self.handle)
            .map(move |h| FaceRef::new(mesh, h))
    }
}


// ===========================================================================
// ===== With FaceHandle
// ===========================================================================

impl<'a, MeshT: 'a> FaceRef<'a, MeshT> {
    /// Returns an iterator over all vertices of this face.
    pub fn adjacent_vertices(&self) -> impl Iterator<Item = VertexRef<'_, MeshT>>
    where
        MeshT: BasicAdj,
    {
        let mesh = self.mesh;
        self.mesh.vertices_around_face(self.handle)
            .map(move |h| VertexRef::new(mesh, h))
    }

    /// Returns an iterator over all faces adjacent to this face.
    ///
    /// See `VertexRef::adjacent_faces` for more information.
    pub fn adjacent_faces(&self) -> impl Iterator<Item = FaceRef<'_, MeshT>>
    where
        MeshT: FullAdj,
    {
        let mesh = &*self.mesh;
        self.mesh.faces_around_face(self.handle)
            .map(move |h| FaceRef::new(mesh, h))
    }

    pub fn is_adjacent_to_face(&self, fh: FaceHandle) -> bool
    where
        MeshT: FullAdj,
    {
        self.mesh.are_faces_adjacent(self.handle, fh)
    }
}

impl<'a, MeshT: 'a> EdgeRef<'a, MeshT> {
    /// Returns the two vertex endpoints of this edge.
    pub fn endpoints(&self) -> [VertexRef<'_, MeshT>; 2]
    where
        MeshT: EdgeAdj,
    {
        let mesh = self.mesh;
        let handles = self.mesh.endpoints_of_edge(self.handle);
        [VertexRef::new(mesh, handles[0]), VertexRef::new(mesh, handles[1])]
    }

    /// Returns an iterator over all faces adjacent to this face.
    ///
    /// See `VertexRef::adjacent_faces` for more information.
    pub fn adjacent_faces(&self) -> impl Iterator<Item = FaceRef<'_, MeshT>>
    where
        MeshT: EdgeAdj,
    {
        let mesh = &*self.mesh;
        self.mesh.faces_of_edge(self.handle)
            .into_iter()
            .map(move |h| FaceRef::new(mesh, h))
    }

    /// Returns whether or not this edge is a boundary edge (that is, if it has
    /// less than 2 adjacent faces).
    pub fn is_boundary(&self) -> bool
    where
        MeshT: EdgeAdj,
    {
        self.mesh.faces_of_edge(self.handle).len() != 2
    }
}
