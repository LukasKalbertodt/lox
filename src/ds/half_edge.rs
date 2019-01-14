//! Everything related to the `HalfEdgeMesh`.

use std::fmt;

use crate as lox;
#[allow(unused_imports)] // TODO
use crate::{
    Empty,
    handle::{DefaultInt, FaceHandle, VertexHandle, Opt, Handle},
    map::{VecMap, PropMap, PropStoreMut},
    traits::{TriVerticesOfFace, Mesh, TriMesh, TriMeshMut, MeshMut},
    refs::{FaceRef, VertexRef},
};



#[derive(Clone, Empty, Debug)]
pub struct HalfEdgeMesh {
    vertices: VecMap<VertexHandle, Vertex>,
    faces: VecMap<FaceHandle, Face>,
    half_edges: VecMap<HalfEdgeHandle, HalfEdge>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct HalfEdgeHandle(DefaultInt);

impl HalfEdgeHandle {
    /// Returns the handle of the half edge twin (the half edge right next to
    /// this half edge, but pointing int he opposite direction).
    ///
    /// This method only works due to some assumptions about the data
    /// structure, so this is only valid together with data structure in this
    /// module! In particular, it assumes that two half edge twins are always
    /// stored right next to each other and that the handles start counting at
    /// an even number (0 in our case). Thus, we can simply flip the last bit
    /// of the handle id to get the twin handle.
    fn twin(&self) -> HalfEdgeHandle {
        Self::from_id(self.id() & 1)
    }
}

impl Handle for HalfEdgeHandle {
    fn from_id(id: DefaultInt) -> Self {
        HalfEdgeHandle(id)
    }
    fn id(&self) -> DefaultInt {
        self.0
    }
}

impl fmt::Debug for HalfEdgeHandle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "HE{}", self.id())
    }
}

#[derive(Debug, Clone, Copy)]
struct Face {
    edge: HalfEdgeHandle,
}

#[derive(Debug, Clone, Copy)]
struct Vertex {
    outgoing: Opt<HalfEdgeHandle>,
}

#[derive(Debug, Clone, Copy)]
struct HalfEdge {
    face: Opt<FaceHandle>,
    target: VertexHandle,
    next: HalfEdgeHandle,
}

impl HalfEdgeMesh {
    fn circulate_around(&self, center: VertexHandle) -> VertexCirculator<'_> {
        match self.vertices[center].outgoing.to_option() {
            None => VertexCirculator::Empty,
            Some(start_he) => VertexCirculator::NonEmpty {
                mesh: self,
                current_he: start_he,
                start_he,
            }
        }
    }

    fn he_between(&self, from: VertexHandle, to: VertexHandle) -> Option<HalfEdgeHandle> {
        self.circulate_around(from)
            .find(|&outgoing| self.half_edges[outgoing].target == to)
    }
}

impl Mesh for HalfEdgeMesh {
    fn num_vertices(&self) -> DefaultInt {
        self.vertices.num_elements()
    }

    fn vertices<'s>(&'s self) -> Box<Iterator<Item = VertexRef<Self>> + 's> {
        Box::new(self.vertices.handles().map(move |handle| {
            VertexRef::new(self, handle)
        }))
    }

    fn contains_vertex(&self, vertex: VertexHandle) -> bool {
        self.vertices.contains_handle(vertex)
    }

    fn num_faces(&self) -> DefaultInt {
        self.faces.num_elements()
    }

    fn faces<'s>(&'s self) -> Box<Iterator<Item = FaceRef<Self>> + 's> {
        Box::new(self.faces.handles().map(move |handle| {
            FaceRef::new(self, handle)
        }))
    }

    fn contains_face(&self, face: FaceHandle) -> bool {
        self.faces.contains_handle(face)
    }
}

impl MeshMut for HalfEdgeMesh {
    fn add_vertex(&mut self) -> VertexHandle {
        self.vertices.push(Vertex {
            outgoing: Opt::none()
        })
    }

    fn remove_all_vertices(&mut self) {
        assert!(
            self.num_faces() == 0,
            "call to `remove_all_vertices`, but there are faces in the mesh!",
        );
        // TODO: check edges == 0, too?

        self.vertices.clear();
    }

    fn remove_all_faces(&mut self) {
        // TODO: remove all links to faces
        self.faces.clear();
    }
}


impl TriMeshMut for HalfEdgeMesh {
    fn add_face(&mut self, [a, b, c]: [VertexHandle; 3]) -> FaceHandle {
        // We need to:
        // - Add between 0 and 3 edges
        //      - Set `target` -> easy
        //      - Set `face` -> easy
        //      - Set `next` -> hard
        // - Add face
        //      - Set `edge` link -> easy

        /// Adds two half edges between `from` and `to`, partially filled with
        /// dummy values.
        ///
        /// This function:
        /// - Correctly sets the `outgoing` field of the vertices, if
        ///   necessary.
        /// - Correctly sets the `target` field of the half edges.
        /// - Always sets the `face` field of the half edges to `None`.
        /// - Sets the `next` field of the half edges to a dummy value. You
        ///   have to overwrite this value!
        fn add_edge(
            mesh: &mut HalfEdgeMesh,
            from: VertexHandle,
            to: VertexHandle,
        ) -> HalfEdgeHandle {
            // Default values that mostly get overwritten later.
            let default = HalfEdge {
                target: from, // never used
                // This might get overwritten later
                face: Opt::none(),

                // This will always be overwritten later!
                next: HalfEdgeHandle::from_id(0),
            };

            // Create the two new half edges.
            let from_outgoing = mesh.half_edges.push(HalfEdge { target: to, .. default });
            let to_outgoing = mesh.half_edges.push(HalfEdge { target: from, .. default });

            // Reference the edge from the vertices, if the vertices don't
            // reference an edge already.
            if mesh.vertices[from].outgoing.is_none() {
                mesh.vertices[from].outgoing = Opt::some(from_outgoing);
            }
            if mesh.vertices[to].outgoing.is_none() {
                mesh.vertices[to].outgoing = Opt::some(to_outgoing);
            }

            // Return the one from `from` to `to`
            from_outgoing
        };

        // Try to find existing edges
        let inner_ab = self.he_between(a, b);
        let inner_bc = self.he_between(b, c);
        let inner_ca = self.he_between(c, a);

        // If edges were not found, we insert a new pair instead. We have to do
        // this in two steps, since we `add_edge` inserts dummy data which
        // would trip the vertex circulator and thus `he_between`.
        let inner_ab = inner_ab.unwrap_or_else(|| add_edge(self, a, b));
        let inner_bc = inner_bc.unwrap_or_else(|| add_edge(self, b, c));
        let inner_ca = inner_ca.unwrap_or_else(|| add_edge(self, c, a));


        let new_face = self.faces.push(Face {
            edge: inner_ab,
        });

        // Set the `face` handle of the inner edges.
        for &he in &[inner_ab, inner_bc, inner_ca] {
            self.half_edges[he].face = Opt::some(new_face);
        }

        new_face
    }
}

// impl TriVerticesOfFace for HalfEdgeMesh {
//     fn vertices_of_face(&self, face: FaceHandle) -> [VertexHandle; 3] {
//         self.faces[face]
//     }
// }

/// An iterator that circulates around a vertex in clockwise order, yielding
/// the outgoing edge.
enum VertexCirculator<'a> {
    Empty,
    NonEmpty {
        mesh: &'a HalfEdgeMesh,
        current_he: HalfEdgeHandle,
        start_he: HalfEdgeHandle,
    },
}

impl Iterator for VertexCirculator<'_> {
    type Item = HalfEdgeHandle;

    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            VertexCirculator::Empty => {
                println!("yielding None");
                None
            }
            VertexCirculator::NonEmpty { mesh, ref mut current_he, start_he } => {
                let out = *current_he;

                // Advance iterator
                let next = mesh.half_edges[out.twin()].next;
                if next == start_he {
                    // If we reached the start edge again, we are done and set
                    // the iterator to `Empty`.
                    *self = VertexCirculator::Empty;
                } else {
                    // If not, we just set the `current_he` to the next one in
                    // the cycle.
                    *current_he = next;
                }

                println!("yielding Some({:?})", out);
                Some(out)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    gen_tri_mesh_tests!(HalfEdgeMesh: []);
}
