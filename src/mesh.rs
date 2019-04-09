use crate::{
    handle::{Handle, HSizeExt, EdgeHandle, FaceHandle, VertexHandle},
    traits::{EdgeMesh, Mesh},
    refs::ElementRef,
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

macro_rules! impl_handle_iter {
    ($mesh_trait:ident, $handle:ident, $method:ident) => {
        impl<M: $mesh_trait + ?Sized> Iterator for HandleIter<'_, M, $handle> {
            type Item = $handle;

            fn next(&mut self) -> Option<Self::Item> {
                let out = self.mesh.$method(self.current);
                if let Some(out) = out {
                    self.current = $handle::new(out.idx().next());
                }

                out
            }
        }
    }
}

impl_handle_iter!(Mesh, VertexHandle, next_vertex_handle_from);
impl_handle_iter!(Mesh, FaceHandle, next_face_handle_from);
impl_handle_iter!(EdgeMesh, EdgeHandle, next_edge_handle_from);


/// An iterator over elements of a mesh. Yields elements with increasing handle
/// index value.
///
/// Instances of this type are returned by:
/// - [`Mesh::vertices`]
/// - [`Mesh::faces`]
/// - [`EdgeMesh::edges`]
#[derive(Debug, Clone)]
pub struct ElementRefIter<'a, M: Mesh + ?Sized, H: Handle> {
    handles: HandleIter<'a, M, H>,
}

impl<'a, M: Mesh + ?Sized, H: Handle> ElementRefIter<'a, M, H> {
    pub(crate) fn new(mesh: &'a M) -> Self {
        Self {
            handles: HandleIter::new(mesh),
        }
    }
}

impl<'a, M, H> Iterator for ElementRefIter<'a, M, H>
where
    M: Mesh + ?Sized,
    H: Handle,
    HandleIter<'a, M, H>: Iterator<Item = H>,
{
    type Item = ElementRef<'a, H, M>;

    fn next(&mut self) -> Option<Self::Item> {
        self.handles.next().map(|h| ElementRef::new(self.handles.mesh, h))
    }
}
