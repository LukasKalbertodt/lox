use crate::{
    handle::{Handle, HSizeExt, EdgeHandle, FaceHandle, VertexHandle},
    traits::{EdgeMesh, Mesh},
};


/// An iterator over the handles of the elements of a mesh. Yields handles with
/// increasing index value.
///
/// Instances of this type are returned by:
/// - [`Mesh::vertex_handles`]
/// - [`Mesh::face_handles`]
/// - [`EdgeMesh::edge_handles`]
#[derive(Debug, Clone)]
pub struct HandleIter<'a, M: Mesh + ?Sized, H: Handle> {
    current: H,
    mesh: &'a M,
}

impl<'a, M: Mesh + ?Sized, H: Handle> HandleIter<'a, M, H> {
    pub(crate) fn new(mesh: &'a M) -> Self {
        Self {
            current: H::new(0),
            mesh,
        }
    }
}

impl<M: Mesh + ?Sized> Iterator for HandleIter<'_, M, VertexHandle> {
    type Item = VertexHandle;

    fn next(&mut self) -> Option<Self::Item> {
        let out = self.mesh.next_vertex_handle_from(self.current);
        if let Some(out) = out {
            self.current = VertexHandle::new(out.idx().next());
        }

        out
    }
}

impl<M: Mesh + ?Sized> Iterator for HandleIter<'_, M, FaceHandle> {
    type Item = FaceHandle;

    fn next(&mut self) -> Option<Self::Item> {
        let out = self.mesh.next_face_handle_from(self.current);
        if let Some(out) = out {
            self.current = FaceHandle::new(out.idx().next());
        }

        out
    }
}

impl<M: EdgeMesh + ?Sized> Iterator for HandleIter<'_, M, EdgeHandle> {
    type Item = EdgeHandle;

    fn next(&mut self) -> Option<Self::Item> {
        let out = self.mesh.next_edge_handle_from(self.current);
        if let Some(out) = out {
            self.current = EdgeHandle::new(out.idx().next());
        }

        out
    }
}
