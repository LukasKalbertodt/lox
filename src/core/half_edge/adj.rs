//! Contains internal circulators, all kinds of iterators and implementations
//! for the adjacency traits.

use optional::Optioned as Opt;

use crate::{
    prelude::*,
    util::{DiList, TriList},
};
use super::{
    Config, HalfEdgeMesh, Checked, HalfEdgeHandle,
};

// ===============================================================================================
// ===== Internal circulators
// ===============================================================================================

/// An iterator that circulates around a vertex in clockwise order, yielding
/// the outgoing halfedge.
#[derive(Debug)]
pub(super) enum CwVertexCirculator<'a, C: Config> {
    Empty,
    NonEmpty {
        mesh: &'a HalfEdgeMesh<C>,
        current_he: Checked<HalfEdgeHandle>,
        start_he: Checked<HalfEdgeHandle>,
    },
}

impl<'a, C: Config> CwVertexCirculator<'a, C> {
    pub(super) fn new(mesh: &'a HalfEdgeMesh<C>, start_he: Checked<HalfEdgeHandle>) -> Self {
        CwVertexCirculator::NonEmpty {
            mesh,
            current_he: start_he,
            start_he,
        }
    }
}

impl<C: Config> Iterator for CwVertexCirculator<'_, C> {
    type Item = Checked<HalfEdgeHandle>;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            CwVertexCirculator::Empty => None,
            CwVertexCirculator::NonEmpty { mesh, ref mut current_he, start_he } => {
                let out = *current_he;

                // Advance iterator
                let next = mesh[out.twin()].next;
                if next == start_he {
                    // If we reached the start edge again, we are done and set
                    // the iterator to `Empty`.
                    *self = CwVertexCirculator::Empty;
                } else {
                    // If not, we just set the `current_he` to the next one in
                    // the cycle.
                    *current_he = next;
                }

                Some(out)
            }
        }
    }
}


/// An iterator that circulates around a face in counter-clockwise order,
/// yielding the inner halfedge.
#[derive(Debug)]
pub(super) enum FaceCirculator<'a, C: Config> {
    Empty,
    NonEmpty {
        mesh: &'a HalfEdgeMesh<C>,
        current_he: Checked<HalfEdgeHandle>,
        start_he: Checked<HalfEdgeHandle>,
    },
}

impl<C: Config> Iterator for FaceCirculator<'_, C> {
    type Item = Checked<HalfEdgeHandle>;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            FaceCirculator::Empty => None,
            FaceCirculator::NonEmpty { mesh, ref mut current_he, start_he } => {
                let out = *current_he;

                // Advance iterator
                let next = mesh[out].next;
                if next == start_he {
                    // If we reached the start edge again, we are done and set
                    // the iterator to `Empty`.
                    *self = FaceCirculator::Empty;
                } else {
                    // If not, we just set the `current_he` to the next one in
                    // the cycle.
                    *current_he = next;
                }

                Some(out)
            }
        }
    }
}


// ===============================================================================================
// ===== Neighborhood trait implementations
// ===============================================================================================

impl<C: Config> BasicAdj for HalfEdgeMesh<C> {
    fn vertices_around_triangle(&self, face: FaceHandle) -> [VertexHandle; 3]
    where
        Self: TriMesh,
    {
        let face = self.check_face(face);
        let he0 = self[face].edge;
        let he1 = self[he0].next;
        let he2 = self[he1].next;

        [he0, he1, he2].map(|he| *self[he].target)
    }

    type VerticesAroundFaceIter<'s> = FaceToVertexIter<'s, C>;

    fn vertices_around_face(&self, face: FaceHandle) -> Self::VerticesAroundFaceIter<'_> {
        FaceToVertexIter {
            it: self.circulate_around_face(self.check_face(face)),
            mesh: self,
        }
    }
}

impl<C: Config> FullAdj for HalfEdgeMesh<C> {
    fn faces_around_triangle(&self, face: FaceHandle) -> TriList<FaceHandle>
    where
        Self: TriMesh,
    {
        let face = self.check_face(face);
        let he0 = self[face].edge;
        let he1 = self[he0].next;
        let he2 = self[he1].next;

        TriList::new(
            [he0, he1, he2].map(|he| self[he.twin()].face.into_option().map(|f| *f))
        )
    }


    type FacesAroundFaceIter<'s> = FaceToFaceIter<'s, C>;

    fn faces_around_face(&self, face: FaceHandle) -> Self::FacesAroundFaceIter<'_> {
        FaceToFaceIter {
            it: self.circulate_around_face(self.check_face(face)),
            mesh: self,
        }
    }


    type FacesAroundVertexIter<'s> = VertexToFaceIter<'s, C>;

    fn faces_around_vertex(&self, vh: VertexHandle) -> Self::FacesAroundVertexIter<'_> {
        VertexToFaceIter {
            it: self.circulate_around_vertex(self.check_vertex(vh)),
            mesh: self,
        }
    }


    type VerticesAroundVertexIter<'s> = VertexToVertexIter<'s, C>;

    fn vertices_around_vertex(&self, vh: VertexHandle) -> Self::VerticesAroundVertexIter<'_> {
        VertexToVertexIter {
            it: self.circulate_around_vertex(self.check_vertex(vh)),
            mesh: self,
        }
    }

    fn is_boundary_face(&self, face: FaceHandle) -> bool {
        self.circulate_around_face(self.check_face(face))
            .any(|inner| self[inner.twin()].face.is_none())
    }

    fn is_boundary_vertex(&self, vertex: VertexHandle) -> bool {
        // This half edge mesh keeps an important invariant for exactly this
        // function: if a vertex is a boundary vertex, its `outgoing` half edge
        // is a boundary half edge. So we can very easily check if a vertex is
        // a boundary vertex.
        let vertex = self.check_vertex(vertex);
        match self[vertex].outgoing.into_option() {
            None => true,
            Some(outgoing) => self[outgoing].face.is_none(),
        }
    }

    fn is_isolated_vertex(&self, vertex: VertexHandle) -> bool {
        let vertex = self.check_vertex(vertex);
        self[vertex].outgoing.is_none()
    }

    fn are_faces_adjacent(&self, a: FaceHandle, b: FaceHandle) -> bool {
        let a = self.check_face(a);
        let b = self.check_face(b);

        self.circulate_around_face(a)
            .any(|inner| self[inner.twin()].face == Opt::some(b))
    }

    fn are_vertices_adjacent(&self, a: VertexHandle, b: VertexHandle) -> bool {
        let a = self.check_vertex(a);
        let b = self.check_vertex(b);

        self.circulate_around_vertex(a)
            .any(|outgoing| self[outgoing].target == b)
    }
}


impl<C: Config> EdgeAdj for HalfEdgeMesh<C> {
    fn endpoints_of_edge(&self, edge: EdgeHandle) -> [VertexHandle; 2] {
        let a = self.checked_half_of(edge);
        let b = a.twin();
        [*self[a].target, *self[b].target]
    }

    fn faces_of_edge(&self, edge: EdgeHandle) -> DiList<FaceHandle> {
        let a = self.checked_half_of(edge);
        let b = a.twin();
        DiList::from_options(
            self[a].face.into_option().map(|f| *f),
            self[b].face.into_option().map(|f| *f),
        )
    }

    type EdgesAroundVertexIter<'s> = VertexToEdgeIter<'s, C>;
    fn edges_around_vertex(&self, vertex: VertexHandle) -> Self::EdgesAroundVertexIter<'_> {
        VertexToEdgeIter {
            it: self.circulate_around_vertex(self.check_vertex(vertex)),
        }
    }

    type EdgesAroundFaceIter<'s> = FaceToEdgeIter<'s, C>;
    fn edges_around_face(&self, face: FaceHandle) -> Self::EdgesAroundFaceIter<'_> {
        FaceToEdgeIter {
            it: self.circulate_around_face(self.check_face(face)),
        }
    }

    fn edges_around_triangle(&self, face: FaceHandle) -> [EdgeHandle; 3]
    where
        Self: TriMesh
    {
        let face = self.check_face(face);
        let he0 = self[face].edge;
        let he1 = self[he0].next;
        let he2 = self[he1].next;

        [he0, he1, he2].map(|he| he.full_edge())
    }

    fn is_boundary_edge(&self, edge: EdgeHandle) -> bool {
        let he = self.checked_half_of(edge);
        self[he].face.is_none() || self[he.twin()].face.is_none()
    }

    fn edge_between_vertices(&self, a: VertexHandle, b: VertexHandle) -> Option<EdgeHandle> {
        let a = self.check_vertex(a);
        let b = self.check_vertex(b);

        self.circulate_around_vertex(a)
            .find(|&outgoing| self[outgoing].target == b)
            .map(|he| he.full_edge())
    }
}


// ===============================================================================================
// ===== Iterators used by public interfaces
// ===============================================================================================

/// Iterator over all faces of a face. Is returned by `faces_around_face`.
#[derive(Debug)]
pub struct FaceToFaceIter<'a, C: Config> {
    it: FaceCirculator<'a, C>,
    mesh: &'a HalfEdgeMesh<C>,
}

impl<C: Config> Iterator for FaceToFaceIter<'_, C> {
    type Item = FaceHandle;

    fn next(&mut self) -> Option<Self::Item> {
        // We simply skip the edge that don't have a face adjacent to them.
        let mesh = self.mesh;
        self.it.by_ref()
            .filter_map(|inner| mesh[inner.twin()].face.into_option())
            .map(|f| *f)
            .next()
    }
}

/// Iterator over all edges of a face. Is returned by `edges_around_face`.
#[derive(Debug)]
pub struct FaceToEdgeIter<'a, C: Config> {
    it: FaceCirculator<'a, C>,
}

impl<C: Config> Iterator for FaceToEdgeIter<'_, C> {
    type Item = EdgeHandle;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|inner| inner.full_edge())
    }
}

/// Iterator over all neighbor vertices of a vertex. Is returned by
/// `vertices_around_vertex`.
#[derive(Debug)]
pub struct FaceToVertexIter<'a, C: Config> {
    it: FaceCirculator<'a, C>,
    mesh: &'a HalfEdgeMesh<C>,
}

impl<C: Config> Iterator for FaceToVertexIter<'_, C> {
    type Item = VertexHandle;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|inner| *self.mesh[inner].target)
    }
}

/// Iterator over all faces of a vertex. Is returned by `faces_around_vertex`.
#[derive(Debug)]
pub struct VertexToFaceIter<'a, C: Config> {
    it: CwVertexCirculator<'a, C>,
    mesh: &'a HalfEdgeMesh<C>,
}

impl<C: Config> Iterator for VertexToFaceIter<'_, C> {
    type Item = FaceHandle;

    fn next(&mut self) -> Option<Self::Item> {
        // We simply skip the edge that don't have a face adjacent to them.
        let mesh = self.mesh;
        self.it.by_ref()
            .filter_map(|outgoing| mesh[outgoing].face.into_option())
            .map(|f| *f)
            .next()
    }
}

/// Iterator over all neighbor vertices of a vertex. Is returned by
/// `vertices_around_vertex`.
#[derive(Debug)]
pub struct VertexToVertexIter<'a, C: Config> {
    it: CwVertexCirculator<'a, C>,
    mesh: &'a HalfEdgeMesh<C>,
}

impl<C: Config> Iterator for VertexToVertexIter<'_, C> {
    type Item = VertexHandle;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|outgoing| *self.mesh[outgoing].target)
    }
}

/// Iterator over all edges of a vertex. Is returned by `edges_around_vertex`.
#[derive(Debug)]
pub struct VertexToEdgeIter<'a, C: Config> {
    it: CwVertexCirculator<'a, C>,
}

impl<C: Config> Iterator for VertexToEdgeIter<'_, C> {
    type Item = EdgeHandle;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|outgoing| outgoing.full_edge())
    }
}
