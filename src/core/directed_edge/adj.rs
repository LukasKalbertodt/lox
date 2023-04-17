//! Contains internal circulators, all kinds of iterators and implementations
//! for the adjacency traits.

use std::array;

use crate::{
    prelude::*,
    util::{TriList, TriListIntoIter},
};
use super::{
    Config, DirectedEdgeMesh, Checked, HalfEdgeHandle, Twin,
};

// ===============================================================================================
// ===== Internal circulators
// ===============================================================================================

/// An iterator that circulates around a vertex in clockwise order, yielding
/// the outgoing halfedge. Note that this yields only every second boundary
/// edge. Since only one half of boundary edges are actually stored, it is not
/// possible to find an outgoing half edge for each edge.
#[derive(Debug)]
pub(super) struct CwVertexCirculator<'a, C: Config> {
    pub(super) mesh: &'a DirectedEdgeMesh<C>,
    pub(super) state: CwVertexCirculatorState,
}

#[derive(Debug)]
pub(super) enum CwVertexCirculatorState {
    Empty,
    NonEmpty {
        current_he: Checked<HalfEdgeHandle>,
        start_he: Checked<HalfEdgeHandle>,
    },
}

impl<'a, C: Config> CwVertexCirculator<'a, C> {
    pub(crate) fn new(mesh: &'a DirectedEdgeMesh<C>, start_he: Checked<HalfEdgeHandle>) -> Self {
        Self {
            mesh,
            state: CwVertexCirculatorState::NonEmpty {
                current_he: start_he,
                start_he,
            }
        }
    }
}

impl<C: Config> Iterator for CwVertexCirculator<'_, C> {
    type Item = Checked<HalfEdgeHandle>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            CwVertexCirculatorState::Empty => None,
            CwVertexCirculatorState::NonEmpty { ref mut current_he, start_he } => {
                let out = *current_he;

                // Advance iterator
                let next = self.mesh.next_he(self.mesh[out].twin.or_next_boundary_he());
                if next == start_he {
                    // If we reached the start edge again, we are done and set
                    // the iterator to `Empty`.
                    self.state = CwVertexCirculatorState::Empty;
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

impl<C: Config> BasicAdj for DirectedEdgeMesh<C> {
    fn vertices_around_triangle(&self, face: FaceHandle) -> [VertexHandle; 3] {
        let [a, b, c] = self.checked_half_edges_around(face);
        [*self[a].target, *self[b].target, *self[c].target]
    }

    type VerticesAroundFaceIter<'s> = array::IntoIter<VertexHandle, 3>;

    fn vertices_around_face(&self, face: FaceHandle) -> Self::VerticesAroundFaceIter<'_> {
        self.vertices_around_triangle(face).into_iter()
    }
}


impl<C: Config> FullAdj for DirectedEdgeMesh<C> {
    fn faces_around_triangle(&self, face: FaceHandle) -> TriList<FaceHandle>
    where
        Self: TriMesh,
    {
        let inner_hes = self.checked_half_edges_around(face);
        let faces = inner_hes.map(|he| self[he].twin.as_real_twin().map(|he| he.face()));

        TriList::new(faces)
    }


    type FacesAroundFaceIter<'s> = TriListIntoIter<FaceHandle>;
    fn faces_around_face(&self, face: FaceHandle) -> Self::FacesAroundFaceIter<'_> {
        self.faces_around_triangle(face).into_iter()
    }


    type FacesAroundVertexIter<'s> = VertexToFaceIter<'s, C>;
    fn faces_around_vertex(&self, vh: VertexHandle) -> Self::FacesAroundVertexIter<'_> {
        VertexToFaceIter(self.circulate_around_vertex(self.check_vertex(vh)))
    }


    type VerticesAroundVertexIter<'s> = VertexToVertexIter<'s, C>;
    fn vertices_around_vertex(&self, vh: VertexHandle) -> Self::VerticesAroundVertexIter<'_> {
        VertexToVertexIter::from_center(self, vh)
    }

    fn is_boundary_face(&self, face: FaceHandle) -> bool {
        let [a, b, c] = self.checked_half_edges_around(face);
        self[a].is_boundary() || self[b].is_boundary() || self[c].is_boundary()
    }

    fn is_boundary_vertex(&self, vertex: VertexHandle) -> bool {
        // This directed edge mesh keeps an important invariant for exactly
        // this function: if a vertex is a boundary vertex, its `outgoing` half
        // edge is a boundary half edge. So we can very easily check if a
        // vertex is a boundary vertex.
        let vertex = self.check_vertex(vertex);
        match self[vertex].outgoing.into_option() {
            None => true,
            Some(outgoing) => self[outgoing].is_boundary(),
        }
    }

    fn is_isolated_vertex(&self, vertex: VertexHandle) -> bool {
        let vertex = self.check_vertex(vertex);
        self[vertex].outgoing.is_none()
    }

    fn are_faces_adjacent(&self, f: FaceHandle, g: FaceHandle) -> bool {
        self.check_face(g);

        self.checked_half_edges_around(f)
            .iter()
            .filter_map(|&he| self[he].twin.as_real_twin())
            .any(|twin| twin.face() == g)
    }
}



// ===============================================================================================
// ===== Iterators used by public interfaces
// ===============================================================================================

/// Iterator over all faces of a vertex. Is returned by `faces_around_vertex`.
#[derive(Debug)]
pub struct VertexToFaceIter<'a, C: Config>(CwVertexCirculator<'a, C>);

impl<C: Config> Iterator for VertexToFaceIter<'_, C> {
    type Item = FaceHandle;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|outgoing| outgoing.face())
    }
}

/// Iterator over all neighbor vertices of a vertex. Is returned by
/// `vertices_around_vertex`.
#[derive(Debug)]
pub struct VertexToVertexIter<'a, C: Config>{
    mesh: &'a DirectedEdgeMesh<C>,
    state: VertexToVertexIterState,
}

#[derive(Debug)]
enum VertexToVertexIterState {
    Empty,
    AtOutgoing {
        outgoing: Checked<HalfEdgeHandle>,
        start_he: Checked<HalfEdgeHandle>,
    },
    AtIncoming {
        incoming: Checked<HalfEdgeHandle>,
        start_he: Checked<HalfEdgeHandle>,
    },
}

impl<'a, C: Config> VertexToVertexIter<'a, C> {
    fn from_center(mesh: &'a DirectedEdgeMesh<C>, center: VertexHandle) -> Self {
        let center = mesh.check_vertex(center);
        let state = match mesh[center].outgoing.into_option() {
            None => VertexToVertexIterState::Empty,
            Some(start_he) => VertexToVertexIterState::AtOutgoing {
                outgoing: start_he,
                start_he,
            }
        };

        Self { mesh, state }
    }
}

impl<C: Config> Iterator for VertexToVertexIter<'_, C> {
    type Item = VertexHandle;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            VertexToVertexIterState::Empty => None,
            VertexToVertexIterState::AtOutgoing { outgoing, start_he } => {
                let out = self.mesh[outgoing].target;

                // Advance iterator
                match self.mesh[outgoing].twin.decode() {
                    Twin::Twin(twin) => {
                        let next = self.mesh.next_he(twin);
                        self.state = if next == start_he {
                            VertexToVertexIterState::Empty
                        } else {
                            VertexToVertexIterState::AtOutgoing {
                                outgoing: next,
                                start_he,
                            }
                        }
                    }
                    Twin::NextBoundaryHe(boundary_he) => {
                        self.state = VertexToVertexIterState::AtIncoming {
                            incoming: boundary_he,
                            start_he,
                        };
                    }
                }

                Some(*out)
            }
            VertexToVertexIterState::AtIncoming { incoming, start_he } => {
                let out = self.mesh[self.mesh.prev_he(incoming)].target;

                // Advance iterator
                let next = self.mesh.next_he(incoming);
                self.state = if next == start_he {
                    VertexToVertexIterState::Empty
                } else {
                    VertexToVertexIterState::AtOutgoing {
                        outgoing: next,
                        start_he,
                    }
                };

                Some(*out)
            }
        }
    }
}
