//! Types for references to elements within a mesh.

use crate::{
    handle::{EdgeHandle, FaceHandle, VertexHandle, Handle},
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


/// A mutable reference to an element within a mesh.
///
/// This is just a handle paired with a mutable reference to the mesh
/// associated with that handle.
#[derive(Debug)]
pub struct ElementRefMut<'a, HandleT: Handle, MeshT: 'a + ?Sized> {
    handle: HandleT,
    mesh: &'a mut MeshT,
}

/// A mutable reference to an edge within a mesh.
///
/// This is just an edge handle with a mutable reference to the mesh. See
/// [`ElementRefMut`] for more information.
pub type EdgeRefMut<'a, MeshT> = ElementRefMut<'a, EdgeHandle, MeshT>;

/// A mutable reference to a face within a mesh.
///
/// This is just a face handle with a mutable reference to the mesh. See
/// [`ElementRefMut`] for more information.
pub type FaceRefMut<'a, MeshT> = ElementRefMut<'a, FaceHandle, MeshT>;

/// A mutable reference to a vertex within a mesh.
///
/// This is just a vertex handle with a mutable reference to the mesh. See
/// [`ElementRefMut`] for more information.
pub type VertexRefMut<'a, MeshT> = ElementRefMut<'a, VertexHandle, MeshT>;



/// Allows to create multiple impl blocks with different headers but same body.
macro_rules! multi_impl {
    (
        [$(
            { $($header:tt)* },
        )*]
        $body:tt
    ) => {
        $(
            $($header)*
            $body
        )*
    }
}


impl<'a, HandleT: Handle, MeshT: 'a> ElementRef<'a, HandleT, MeshT> {
    pub fn new(mesh: &'a MeshT, handle: HandleT) -> Self {
        Self { mesh, handle }
    }
}

impl<'a, HandleT: Handle, MeshT: 'a> ElementRefMut<'a, HandleT, MeshT> {
    pub fn new(mesh: &'a mut MeshT, handle: HandleT) -> Self {
        Self { mesh, handle }
    }

    /// Returns a mutable reference to the linked mesh.
    pub fn mesh_mut(&mut self) -> &mut MeshT {
        self.mesh
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


multi_impl!{
    [
        { impl<'a, HandleT: Handle, MeshT: 'a> ElementRef<'a, HandleT, MeshT> },
        { impl<'a, HandleT: Handle, MeshT: 'a> ElementRefMut<'a, HandleT, MeshT> },
    ]
    {
        /// Returns the stored handle.
        pub fn handle(&self) -> HandleT {
            self.handle
        }

        /// Returns an immutable reference to the linked mesh.
        pub fn mesh(&self) -> &MeshT {
            self.mesh
        }
    }
}

// ===========================================================================
// ===== With VertexHandle
// ===========================================================================
multi_impl!{
    [
        { impl<'a, MeshT: 'a> VertexRef<'a, MeshT> },
        { impl<'a, MeshT: 'a> VertexRefMut<'a, MeshT> },
    ]
    {

    }
}



// ===========================================================================
// ===== With FaceHandle
// ===========================================================================
multi_impl!{
    [
        { impl<'a, MeshT: 'a> FaceRef<'a, MeshT> },
        { impl<'a, MeshT: 'a> FaceRefMut<'a, MeshT> },
    ]
    {
    }
}
