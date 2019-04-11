use crate::{
    handle::{hsize, Handle, HSizeExt, EdgeHandle, FaceHandle, VertexHandle},
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
    count: hsize,
}



macro_rules! impl_handle_iter {
    ($mesh_trait:ident, $handle:ident, $method:ident, $num_fn:ident) => {
        impl<'a, M: $mesh_trait + ?Sized> HandleIter<'a, M, $handle> {
            pub(crate) fn new(mesh: &'a M) -> Self {
                Self {
                    current: $handle::new(0),
                    mesh,
                    count: mesh.$num_fn(),
                }
            }
        }

        impl<M: $mesh_trait + ?Sized> Iterator for HandleIter<'_, M, $handle> {
            type Item = $handle;

            fn next(&mut self) -> Option<Self::Item> {
                let out = self.mesh.$method(self.current);
                if let Some(out) = out {
                    self.current = $handle::new(out.idx().next());
                    self.count -= 1;
                }

                out
            }

            fn size_hint(&self) -> (usize, Option<usize>) {
                (self.count as usize, Some(self.count as usize))
            }
        }

        impl<M: $mesh_trait + ?Sized> ExactSizeIterator for HandleIter<'_, M, $handle> {}
    }
}

impl_handle_iter!(Mesh, VertexHandle, next_vertex_handle_from, num_vertices);
impl_handle_iter!(Mesh, FaceHandle, next_face_handle_from, num_faces);
impl_handle_iter!(EdgeMesh, EdgeHandle, next_edge_handle_from, num_edges);



/// An iterator over the handles of the elements of a mesh. Yields handles with
/// increasing index value. Can give mutable access to the underlying mesh.
///
/// Note that using this iterator is a bit tricky and you have to pay attention
/// to not shoot yourself in the foot. This iterator iterates through all
/// existing element handles from handle index 0 to the initial
/// `self.last_{element}_handle` (that is: what this method returned when this
/// iterator was created). That means that:
///
/// - Adding new elements with a handle higher than the "initial last handle"
///   won't affect the iteration.
/// - Adding or removing elements with handle indices lower than the handle
///   that was last yielded by this iterator, won't affect the iteration.
/// - Adding or removing elements with handle indices higher than the handle
///   last yielded by this iterator but smaller than the "initial last handle"
///   *will* affect the iteration. You should avoid doing that.
#[derive(Debug)]
pub struct HandleIterMut<'a, M: Mesh + ?Sized, H: Handle> {
    current: H,
    last: H,
    mesh: &'a mut M,
}

impl<'a, M: Mesh + ?Sized, H: Handle> HandleIterMut<'a, M, H> {
    pub fn mesh(&mut self) -> &mut M {
        self.mesh
    }
}

macro_rules! impl_handle_iter_mut {
    ($mesh_trait:ident, $handle:ident, $next:ident, $last:ident) => {
        impl<'a, M: $mesh_trait + ?Sized> HandleIterMut<'a, M, $handle> {
            pub(crate) fn new(mesh: &'a mut M) -> Self {
                // If the mesh has none of these elements, the `last` value is
                // not important.
                Self {
                    current: $handle::new(0),
                    last: mesh.$last().unwrap_or($handle::new(0)),
                    mesh,
                }
            }
        }

        impl<M: $mesh_trait + ?Sized> Iterator for HandleIterMut<'_, M, $handle> {
            type Item = $handle;

            fn next(&mut self) -> Option<Self::Item> {
                let out = self.mesh.$next(self.current);

                if let Some(out) = out {
                    if out.idx() > self.last.idx() {
                        return None;
                    }

                    self.current = $handle::new(out.idx().next());
                }

                out
            }
        }
    }
}

impl_handle_iter_mut!(Mesh, VertexHandle, next_vertex_handle_from, last_vertex_handle);
impl_handle_iter_mut!(Mesh, FaceHandle, next_face_handle_from, last_face_handle);
impl_handle_iter_mut!(EdgeMesh, EdgeHandle, next_edge_handle_from, last_edge_handle);

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

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.handles.size_hint()
    }
}

impl<'a, M, H> ExactSizeIterator for ElementRefIter<'a, M, H>
where
    M: Mesh + ?Sized,
    H: Handle,
    HandleIter<'a, M, H>: Iterator<Item = H>,
{}

macro_rules! impl_element_iter {
    ($mesh_trait:ident, $handle:ident) => {
        impl<'a, M: $mesh_trait + ?Sized> ElementRefIter<'a, M, $handle> {
            pub(crate) fn new(mesh: &'a M) -> Self {
                Self {
                    handles: HandleIter::<M, $handle>::new(mesh),
                }
            }
        }
    }
}

impl_element_iter!(Mesh, VertexHandle);
impl_element_iter!(Mesh, FaceHandle);
impl_element_iter!(EdgeMesh, EdgeHandle);
