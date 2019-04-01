//! Everything related to the `HalfEdgeMesh`.

use std::fmt;

use crate as lox;
use crate::{
    prelude::*,
    handle::{hsize, Opt, Handle},
    map::VecMap,
    traits::marker::TriFaces,
    util::{DiList, TriList},
};



const NON_MANIFOLD_VERTEX_ERR: &str =
    "new face would add a non-manifold vertex (no hole found in cycle)";
const NON_MANIFOLD_EDGE_ERR: &str =
    "new face would add a non-manifold edge";

// ===============================================================================================
// ===== Definition of types stored inside the data structure & HeHandle
// ===============================================================================================

#[derive(Clone, Empty, Debug)]
pub struct HalfEdgeMesh {
    vertices: VecMap<VertexHandle, Vertex>,
    faces: VecMap<FaceHandle, Face>,
    half_edges: VecMap<HalfEdgeHandle, HalfEdge>,
}

/// Handle to refer to half edges.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct HalfEdgeHandle(hsize);

impl HalfEdgeHandle {
    /// Returns the handle of the half edge twin (the half edge right next to
    /// this half edge, but pointing in the opposite direction).
    ///
    /// This method only works due to some assumptions about the data
    /// structure, so this is only valid together with data structure in this
    /// module! In particular, it assumes that two half edge twins are always
    /// stored right next to each other and that the handles start counting at
    /// an even number (0 in our case). Thus, we can simply flip the last bit
    /// of the handle id to get the twin handle.
    fn twin(self) -> HalfEdgeHandle {
        Self::new(self.idx() ^ 1)
    }

    /// Returns one (arbitrary) half-edge of the given edge.
    ///
    /// Again, due to our assumptions on how edges are stored, we just have to
    /// multiply the edges handle with 2 to get a corresponding half edge
    /// handle. This method does not check if the half edge actually exists.
    fn one_half_of(edge: EdgeHandle) -> Self {
        Self(edge.idx() * 2)
    }

    /// Returns the full edge this half-edge belongs to.
    ///
    /// This works only because we know we store the half edges adjacent to one
    /// another. Of one edge, the half edge with the smaller index always has
    /// an even index, while the other one has an odd one. This means we can
    /// just integer divide by 2 and get the edge index.
    fn full_edge(self) -> EdgeHandle {
        EdgeHandle::new(self.0 / 2)
    }
}

impl Handle for HalfEdgeHandle {
    fn new(id: hsize) -> Self {
        HalfEdgeHandle(id)
    }
    fn idx(&self) -> hsize {
        self.0
    }
}

impl fmt::Debug for HalfEdgeHandle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "HE{}", self.idx())
    }
}

/// Data stored per `Face`.
#[derive(Debug, Clone, Copy)]
struct Face {
    /// Handle of one (arbitrary) half edge adjacent to the face.
    edge: HalfEdgeHandle,
}

/// Data stored per `Vertex`.
#[derive(Debug, Clone, Copy)]
struct Vertex {
    /// Handle of one (arbitrary) outgoing half edge.
    outgoing: Opt<HalfEdgeHandle>,
}

/// Data stored per half edge.
#[derive(Debug, Clone, Copy)]
struct HalfEdge {
    /// The adjacent face, if one exists.
    face: Opt<FaceHandle>,

    /// The vertex this half edge points to.
    target: VertexHandle,

    /// The next half edge around the face or hole this half edge is adjacent
    /// to (going counter clock wise).
    next: HalfEdgeHandle,
}


// ===============================================================================================
// ===== Internal helper methods
// ===============================================================================================

impl HalfEdgeMesh {
    /// Returns an iterator the circulates around the vertex `center`. The
    /// iterator yields outgoing half edges.
    fn circulate_around(&self, center: VertexHandle) -> CirculatorAroundVertex<'_> {
        match self.vertices[center].outgoing.to_option() {
            None => CirculatorAroundVertex::Empty,
            Some(start_he) => CirculatorAroundVertex::NonEmpty {
                mesh: self,
                current_he: start_he,
                start_he,
            }
        }
    }

    /// Returns an iterator the circulates around the vertex `start_he` is
    /// coming out of (i.e. `start_he.twin.target` is the center vertex). The
    /// iterator yields outgoing half edges, starting with `start_he`.
    fn circulate_around_from(&self, start_he: HalfEdgeHandle) -> CirculatorAroundVertex<'_> {
        CirculatorAroundVertex::NonEmpty {
            mesh: self,
            current_he: start_he,
            start_he,
        }
    }

    /// Tries to find the half edge from `from` to `to`. Returns `None` if
    /// there is no edge between the two vertices.
    fn he_between(&self, from: VertexHandle, to: VertexHandle) -> Option<HalfEdgeHandle> {
        self.circulate_around(from)
            .find(|&outgoing| self.half_edges[outgoing].target == to)
    }

    /// Returns the half edge whose `next` points to `he`.
    ///
    /// If `prev` handles would be stored, this would be easy. But since we
    /// don't store them, we have to circulate around the whole vertex.
    fn prev(&self, he: HalfEdgeHandle) -> HalfEdgeHandle {
        self.circulate_around(self.half_edges[he.twin()].target)
            .map(|outgoing| outgoing.twin())
            .find(|&incoming| self.half_edges[incoming].next == he)
            .expect("internal HEM error: could not find `prev` half edge")
    }

    /// Adds two half edges between `from` and `to`, partially filled with
    /// dummy values. Returns the handle of the halfedge pointing to `to`.
    ///
    /// This function:
    /// - Correctly sets the `target` field of the half edges.
    /// - Always sets the `face` field of the half edges to `None`.
    /// - Sets the `next` field of the half edges to a dummy value. You
    ///   have to overwrite this value!
    /// - Does not set the `outgoing` fields of the vertices.
    fn add_edge_partially(&mut self, from: VertexHandle, to: VertexHandle) -> HalfEdgeHandle {
        let face = Opt::none();
        let next = HalfEdgeHandle::new(0);

        self.half_edges.push(HalfEdge { target: from, face, next });
        self.half_edges.push(HalfEdge { target: to, face, next })
    }
}


// ===============================================================================================
// ===== Mesh trait implementations
// ===============================================================================================

impl Mesh for HalfEdgeMesh {
    type FaceKind = TriFaces; // TODO

    fn num_vertices(&self) -> hsize {
        self.vertices.num_elements()
    }

    fn vertex_handles(&self) -> Box<Iterator<Item = VertexHandle> + '_> {
        Box::new(self.vertices.handles())
    }

    fn contains_vertex(&self, vertex: VertexHandle) -> bool {
        self.vertices.contains_handle(vertex)
    }

    fn num_faces(&self) -> hsize {
        self.faces.num_elements()
    }

    fn face_handles(&self) -> Box<Iterator<Item = FaceHandle> + '_> {
        Box::new(self.faces.handles())
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

    fn add_triangle(&mut self, [a, b, c]: [VertexHandle; 3]) -> FaceHandle {
        assert_ne!(a, b, "vertices of new face are not unique");
        assert_ne!(a, c, "vertices of new face are not unique");

        // ===================================================================
        // ===== Step 1: Find or add edges with dummy/old `next` handles
        // ===================================================================
        // We are interested in the three inner half edges (the ones of the new
        // face). We are trying to find them or insert them if they don't
        // already exist.
        //
        // In this step, the `next` and `face` handle of the three inner half
        // edges is still potentially or definetely incorrect. Additionally,
        // the `outgoing` handle of vertices is not updated. We do this at the
        // very end in order to use the vertex circulator (to iterate around
        // the vertex in the state before `add_face` was called).

        /// Tries to find the half edge from `from` to `to`. If found, it is
        /// asserted that it is a boundary edge. If not found, a new edge is
        /// created via `add_edge`.
        fn find_or_add_edge(
            mesh: &mut HalfEdgeMesh,
            from: VertexHandle,
            to: VertexHandle,
        ) -> HalfEdgeHandle {
            match mesh.he_between(from, to ) {
                Some(he) => {
                    assert!(mesh.half_edges[he].face.is_none(), NON_MANIFOLD_EDGE_ERR);
                    he
                }
                None => mesh.add_edge_partially(from, to),
            }
        }

        // Try to find existing edges or insert a new edge pair, if it doesn't
        // exist yet.
        let inner_ab = find_or_add_edge(self, a, b);
        let inner_bc = find_or_add_edge(self, b, c);
        let inner_ca = find_or_add_edge(self, c, a);

        // ===================================================================
        // ===== Step 2: Add face and fix `face` handle of inner edges
        // ===================================================================
        // Insert new face
        let new_face = self.faces.push(Face {
            edge: inner_ab, // just an arbitrary edge
        });

        // Set the `face` handle of the inner edges.
        for &he in &[inner_ab, inner_bc, inner_ca] {
            self.half_edges[he].face = Opt::some(new_face);
        }


        // ===================================================================
        // ===== Step 3: Fix `next` handles
        // ===================================================================
        let corners = [
            (inner_ab.twin(), a, inner_ca.twin()),
            (inner_bc.twin(), b, inner_ab.twin()),
            (inner_ca.twin(), c, inner_bc.twin()),
        ];

        // This fixes the next handles of the outer three edges plus additional
        // edges not adjacent to this face, as necessary. We handle each corner
        // seperately.
        //
        // So for each corner, we have this situation (the corner vertex `v`,
        // the new face `F`, the two outer edges `incoming and `outgoing` and
        // we don't yet know what `v` is also connected too):
        //
        //                 ?
        //           ?           ?
        //
        //                (v)
        //               ^/ ^\
        //              //   \\
        //   incoming  //     \\  outgoing
        //            //   F   \\
        //           /v         \v
        //          ( ) ------> ( )
        //              <------
        //
        // This is difficult in particular, because there can be multiple fan
        // blades around the vertex. Here is an example: there are three
        // fan-blades around the central vertex X. One fan blade consists of
        // two faces, the other two of only one face.
        //
        //
        //               o---------o
        //                \       /
        //                 \     /
        //                  \   /
        //                   \ /
        //          o---------X---------o
        //          |       ╱ | ╲       |
        //          |     ╱   |   ╲     |
        //          |   ╱     |     ╲   |
        //          | ╱       |       ╲ |
        //          o         o---------o
        //
        // The order of fan blades is ambigious. When inserting a new fan
        // blade, we do not know where in the cycle to insert it. So we have to
        // accept a bit of chaos while multiple blades still exist. But often,
        // blades are reconnected (this is the `(true, true)` case below) in
        // which case we need to take special care.
        for &(incoming, vh, outgoing) in &corners {
            let v = &self.vertices[vh];
            let incoming_face = self.half_edges[incoming].face;
            let outgoing_face = self.half_edges[outgoing].face;

            // We have four different cases: it just depends whether incoming
            // and/or outgoing are already adjacent to a face.
            match (incoming_face.is_some(), outgoing_face.is_some()) {
                // Both edge pairs were newly inserted. This is usually easy,
                // but it can be a bit tricky when there are other edges (and
                // thus a face) connected to that vertex.
                (false, false) => {
                    if v.outgoing.is_some() {
                        // More difficult case: we are creating a multi
                        // fan-blade vertex here. In order to correctly set the
                        // `next` handles, we need to find the start of some
                        // blade and the end of some blade. We will insert the
                        // new blade between the two.
                        //
                        //
                        //
                        //           ^  ?     ?  /
                        //            \         /
                        //      start  \   ?   /  end
                        //              \     /
                        //               \   v
                        //                (v)
                        //               ^/ ^\
                        //              //   \\
                        //             //     \\
                        //            //   F   \\
                        //           /v         \v
                        //          ( )         ( )
                        //

                        // Find the end edge of some blade.
                        let end = self.circulate_around(vh)
                            .map(|outgoing| outgoing.twin())
                            .find(|&incoming| self.half_edges[incoming].face.is_none())
                            .expect(NON_MANIFOLD_VERTEX_ERR);

                        // The start of another blade.
                        let start = self.half_edges[end].next;

                        // Insert new blade in between.
                        self.half_edges[incoming].next = start;
                        self.half_edges[end].next = outgoing;
                    } else {
                        // This is the easy case: `incoming` and `outgoing` are
                        // the only edges adjacent to `v`.
                        self.half_edges[incoming].next = outgoing;
                    }
                }

                // The incoming edge is adjacent to another face (OF), but the
                // outgoing is not. We have to find the edge `before_new` whose
                // `next` handle points to `incoming`'s twin (a soon to be
                // inner edge of our new face). Because that `next` handle now
                // needs to point to `outgoing`.
                //
                //                      /
                //      ?         ?    /
                //           ?        /  before_new
                //                   /
                //                  v
                //      <-------- (v)
                //               ^/ ^\
                //         OF   //   \\
                //             //     \\
                //            //   F   \\
                //           /v         \v
                //          ( )         ( )
                //
                //                 ^-- this face and
                //                       ^-- this edge are new in the cycle
                //
                (true, false) => {
                    let before_new = self.prev(incoming.twin());
                    self.half_edges[before_new].next = outgoing;
                }

                // The outgoing edge is adjacent to another face (OF), but the
                // incoming is not. This is fairly easy: the twin of outgoing
                // points to some edge. The incoming edge just needs to point
                // that edge now. The `next` of the outgoing twin will be set
                // later (since it's an inner edge of our new face).
                //
                //            ^
                //             \   ?
                //              \           ?
                //               \   ?
                //                \
                //                (v)<---------
                //               ^/ ^\
                //              //   \\  OF
                //             //     \\
                //            //   F   \\
                //           /v         \v
                //          ( )         ( )
                //
                //                 ^-- this face and
                //           ^-- this edge are new in the cycle
                //
                (false, true) => {
                    self.half_edges[incoming].next = self.half_edges[outgoing.twin()].next;
                }

                // This can be easy or the ugliest case. The incoming and
                // outgoing edge are both adjacent to a face. That means we are
                // connecting two fan blades. If the fan blade of `incoming` is
                // already directly after the fan blade of `outgoing` (speaking
                // about the "circulate around vertex" order), then everything
                // is fine.
                //
                //                 ?
                //           ?           ?
                //
                //      <-------- (v) -------->
                //               ^/ ^\
                //         IF   //   \\   OF
                //             //     \\
                //            //   F   \\
                //           /v         \v
                //          ( )         ( )
                //
                //                 ^-- this face is new,
                //                     the edges already exist
                //
                // BUT, if that is not the case, we need to change the order of
                // fan blades to match the "good" situation described above.
                (true, true) => {
                    if self.half_edges[outgoing.twin()].next != incoming.twin() {
                        // Here we need to conceptually delete one fan blade
                        // from the `next` circle around `v` and re-insert it
                        // into the right position. We choose to "move" the fan
                        // blade starting with `incoming`.
                        let extra_blade_end = self.prev(incoming.twin());
                        let incoming_blade_end = self.circulate_around_from(incoming.twin())
                            .map(|outgoing| outgoing.twin())
                            .find(|&incoming| self.half_edges[incoming].face.is_none())
                            .expect("internal HEM error: cannot find `incoming_blade_end`");

                        // Here we remove the "incoming blade" from the cycle.
                        self.half_edges[extra_blade_end].next
                            = self.half_edges[incoming_blade_end].next;

                        // Now we reinsert it again, right after the "outgoing
                        // blade". After this, the cycle is still a bit broken,
                        // but that doesn't matter, because (a) the cycle will
                        // be repaired by setting the `next` link of the inner
                        // edges below, and (b) the broken cycle won't be
                        // accessed (in this direction) before it is repaired.
                        self.half_edges[incoming_blade_end].next
                            = self.half_edges[outgoing.twin()].next;
                    }
                }
            }
        }

        // Now we only need to set the `next` handles of the inner half edges.
        // This is easy.
        self.half_edges[inner_ab].next = inner_bc;
        self.half_edges[inner_bc].next = inner_ca;
        self.half_edges[inner_ca].next = inner_ab;


        // ===================================================================
        // ===== Step 4: Set `outgoing` handles of vertices where necessary
        // ===================================================================
        if self.vertices[a].outgoing.is_none() {
            self.vertices[a].outgoing = Opt::some(inner_ab);
        }
        if self.vertices[b].outgoing.is_none() {
            self.vertices[b].outgoing = Opt::some(inner_bc);
        }
        if self.vertices[c].outgoing.is_none() {
            self.vertices[c].outgoing = Opt::some(inner_ca);
        }


        new_face
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
        self.half_edges.clear();
        self.faces.clear();
        for v in self.vertices.values_mut() {
            v.outgoing = Opt::none();
        }
    }

    fn split_face(&mut self, f: FaceHandle) -> VertexHandle {
        // Assuming the face `f` has N adjacent edges, then we need to add:
        // - N - 1 new faces
        // - N new edges (2N new half edges)
        // - 1 new vertex (the "midpoint")
        //
        // Let's visualize what are are about to do (for the N=3 case, but it
        // works for all Ns). On the left is the current situation, on the
        // right what it looks like after this method is done. The outer half
        // edges of the original faces are not shown.
        //
        //               (A)                |               (A)
        //              /   ^               |             / ^ | ^
        //             /     \              |            /  | |  \
        //            /       \             |           /   | |   \
        //           /         \            |          /    | v    \
        //          /           \           |         /     (M)     \
        //         /             \          |        /    ↗⟋   ↖⟍    \
        //        /               \         |       /   ⟋⟋      ⟍⟍   \
        //       /                 \        |      /  ⟋⟋          ⟍⟍  \
        //      v                   \       |     v ⟋↙              ⟍↘ \
        //    (B) ----------------> (C)     |    (B) ----------------> (C)
        //
        //
        // In the beginning, we add the `midpoint` vertex. We also add the
        // first edge: between `midpoint` and one vertex of `f` (let's say it's
        // (A), but it doesn't matter).
        //
        // In the loop, we iterate N - 1 times counter click-wise around (M).
        // In each iteration, we will add one edge and one face. Example for
        // the triangle case above:
        // - 1st iteration: add edge between (B) and (M), add face [M, A, B].
        // - 2nd iteration: add edge between (C and (M), add face [M, B, C].
        //
        // The last face is "added" outside of the loop. That's because (a), we
        // don't have to add a new edge (we use the one added at the very
        // start) and (b) because we can then reuse the old face.
        //
        // For half edge handles, this method uses the following naming scheme:
        // half edges that existed before the function was called are called
        // `_ohe` (old half edge). Half edges that are inserted by this
        // function are called `_nhe` (new half edge).

        // Add new vertex "in the middle".
        let midpoint = self.add_vertex();

        // Pick arbitrary start edge and vertex. In the example above that are
        // `(A) -> (B)` and (A), respectively.
        let start_ohe = self.faces[f].edge;
        let start_vertex = self.half_edges[start_ohe.twin()].target;

        // Add first edge and set midpoint's `outgoing` to that edge.
        let start_nhe = self.add_edge_partially(midpoint, start_vertex);
        self.vertices[midpoint].outgoing = Opt::some(start_nhe);

        // `border_ohe` is one if the inner half edges of the original face.
        // Each loop iteration changes this value to its `next` half edge. That
        // way we circulate around the face/midpoint. This is `(A) -> (B)` in
        // the example above in the first loop iteration.
        //
        // `last_nhe`: The edge that was last added. The variable stores the
        // half edge pointing away from `midpoint`. This is also changed in
        // each loop iteration as each loop iteration creates a new edge. This
        // is `(M) -> (A)` in the example above in the first loop iteration.
        let mut border_ohe = start_ohe;
        let mut last_nhe = start_nhe;

        // We iterate N - 1 times. We handle the last face manually.
        while self.half_edges[border_ohe].target != start_vertex {
            // We store the next iteration edge now already, because we will
            // overwrite the `next` value below.
            let next_border_ohe = self.half_edges[border_ohe].next;

            // Add the a edge
            let next_vertex = self.half_edges[border_ohe].target;
            let next_nhe = self.add_edge_partially(midpoint, next_vertex);

            // Add the new face. This includes setting the `next` and `face`
            // fields of the edges of the new triangle. These are:
            // `border_ohe`, `last_nhe` and `next_nhe.twin()`.
            let inner_new = next_nhe.twin();
            let new_face = self.faces.push(Face { edge: inner_new });

            self.half_edges[inner_new].next = last_nhe;
            self.half_edges[last_nhe].next = border_ohe;
            self.half_edges[border_ohe].next = inner_new;

            self.half_edges[inner_new].face = Opt::some(new_face);
            self.half_edges[last_nhe].face = Opt::some(new_face);
            self.half_edges[border_ohe].face = Opt::some(new_face);


            // Advance the iteration edges
            last_nhe = next_nhe;
            border_ohe = next_border_ohe;
        }

        // "Add" the last face. We are reusing the existing one.
        let start_inner_nhe = start_nhe.twin();
        self.faces[f].edge = start_inner_nhe;

        self.half_edges[start_inner_nhe].next = last_nhe;
        self.half_edges[last_nhe].next = border_ohe;
        self.half_edges[border_ohe].next = start_inner_nhe;

        self.half_edges[start_inner_nhe].face = Opt::some(f);
        self.half_edges[last_nhe].face = Opt::some(f);
        // `border_ohe.face` is already `f`


        midpoint
    }
}

impl EdgeMesh for HalfEdgeMesh {
    fn num_edges(&self) -> hsize {
        // There are always exactly twice as many half edge as there are edges
        self.half_edges.num_elements() / 2
    }

    fn edge_handles(&self) -> Box<dyn Iterator<Item = EdgeHandle> + '_> {
        // This, again, only works because of how we store data. We always
        // store both half edges of an edge right next to each other in
        // memory. The half edge with lower index has an even index (we start
        // at 0). We map half edges to edges by always using the half edge
        // with the lower index and divide its index by 2.
        Box::new(
            self.half_edges.handles()
                .filter(|he| he.idx() % 2 == 0)
                .map(|he| he.full_edge())
        )
    }

    fn contains_edge(&self, edge: EdgeHandle) -> bool {
        let he = HalfEdgeHandle::one_half_of(edge);
        self.half_edges.contains_handle(he)
    }
}

impl TriEdgeMeshMut for HalfEdgeMesh {
    fn flip_edge(&mut self, edge: EdgeHandle) {
        //                                  |
        //            Before                |                After
        //            ------                |                -----
        //                                  |
        //                                  |
        //              [C]                 |                 [C]
        //                                  |
        //          ^  /   ^  \             |             ^  /   ^  \
        //         /  /     \  \            |            /  / ^ | \  \
        //        /  /       \  \           |           /  /  | |  \  \
        //       /  /         \  \          |          /  /   | |   \  \
        //      /  /c         d\  \         |         /  /c   | |   d\  \
        //     /  /     (X)     \  \        |        /  /     | |     \  \
        //    /  /               \  \       |       /  /      | |      \  \
        //   /  /                 \  \      |      /  /       | |       \  \
        //  /  v         a         \  v     |     /  v        | |        \  v
        //       --------------->           |                 | |
        //  [A]                     [B]     |     [A]   (Y)  b| |a  (X)   [B]
        //       <---------------           |                 | |
        //  ^  \         b         ^  /     |     ^  \        | |        ^  /
        //   \  \                 /  /      |      \  \       | |       /  /
        //    \  \               /  /       |       \  \      | |      /  /
        //     \  \     (Y)     /  /        |        \  \     | |     /  /
        //      \  \e         f/  /         |         \  \e   | |   f/  /
        //       \  \         /  /          |          \  \   | |   /  /
        //        \  \       /  /           |           \  \  | |  /  /
        //         \  \     /  /            |            \  \ | v /  /
        //          \  v   /  v             |             \  v   /  v
        //                                  |
        //              [D]                 |                 [D]
        //
        //
        // ### A mapping from graphic names to variable names:
        //
        //  Edges                      Vertices               Faces
        //  -----                      --------               -----
        //  a: center_above            [A]: v_left             (X): f_above
        //  b: center_below            [B]: v_right            (Y): f_below
        //  c: above_left              [C]: v_above
        //  d: above_right             [D]: v_below
        //  e: below_left
        //  f: below_right
        //
        //
        // We just imagine that the "random" half-edge we get from
        // `one_half_of()` is the edge `a` in the drawing.

        assert!(
            self.faces_of_edge(edge).len() == 2,
            "`flip_edge` called on boundary edge {:?}",
            edge,
        );

        // First, let's just obtain all handles
        let center_above = HalfEdgeHandle::one_half_of(edge);
        let center_below = center_above.twin();
        let above_right = self.half_edges[center_above].next;
        let above_left = self.half_edges[above_right].next;
        let below_left = self.half_edges[center_below].next;
        let below_right = self.half_edges[below_left].next;

        let f_above = self.half_edges[center_above].face.unwrap();
        let f_below = self.half_edges[center_below].face.unwrap();

        let v_right = self.half_edges[center_above].target;
        let v_left = self.half_edges[center_below].target;
        let v_above = self.half_edges[above_right].target;
        let v_below = self.half_edges[below_left].target;


        // Update all fields
        self.vertices[v_left].outgoing = Opt::some(below_left);
        self.vertices[v_right].outgoing = Opt::some(above_right);

        self.faces[f_above].edge = center_above;
        self.faces[f_below].edge = center_below;

        self.half_edges[center_above].target = v_below;
        self.half_edges[center_above].next = below_right;
        self.half_edges[center_below].target = v_above;
        self.half_edges[center_below].next = above_left;

        self.half_edges[below_right].face = Opt::some(f_above);
        self.half_edges[below_right].next = above_right;
        self.half_edges[above_right].next = center_above;

        self.half_edges[above_left].face = Opt::some(f_below);
        self.half_edges[above_left].next = below_left;
        self.half_edges[below_left].next = center_below;
    }
}

impl SupportsMultiBlade for HalfEdgeMesh {}

// ===============================================================================================
// ===== Internal circulators
// ===============================================================================================

/// An iterator that circulates around a vertex in clockwise order, yielding
/// the outgoing edge.
enum CirculatorAroundVertex<'a> {
    Empty,
    NonEmpty {
        mesh: &'a HalfEdgeMesh,
        current_he: HalfEdgeHandle,
        start_he: HalfEdgeHandle,
    },
}

impl Iterator for CirculatorAroundVertex<'_> {
    type Item = HalfEdgeHandle;

    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            CirculatorAroundVertex::Empty => None,
            CirculatorAroundVertex::NonEmpty { mesh, ref mut current_he, start_he } => {
                let out = *current_he;

                // Advance iterator
                let next = mesh.half_edges[out.twin()].next;
                if next == start_he {
                    // If we reached the start edge again, we are done and set
                    // the iterator to `Empty`.
                    *self = CirculatorAroundVertex::Empty;
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

impl VerticesAroundFace for HalfEdgeMesh {
    fn vertices_around_triangle(&self, face: FaceHandle) -> [VertexHandle; 3]
    where
        Self: TriMesh,
    {
        let he0 = self.faces[face].edge;
        let he1 = self.half_edges[he0].next;
        let he2 = self.half_edges[he1].next;

        [he0, he1, he2].map(|he| self.half_edges[he].target)
    }

    fn vertices_around_face(&self, face: FaceHandle) -> DynList<'_, VertexHandle> {
        // TODO: change to support polygons
        Box::new(self.vertices_around_triangle(face).owned_iter())
    }
}

impl FacesAroundFace for HalfEdgeMesh {
    fn faces_around_triangle(&self, face: FaceHandle) -> TriList<FaceHandle>
    where
        Self: TriMesh,
    {
        let he0 = self.faces[face].edge;
        let he1 = self.half_edges[he0].next;
        let he2 = self.half_edges[he1].next;

        TriList::new(
            [he0, he1, he2].map(|he| self.half_edges[he.twin()].face.to_option())
        )
    }

    fn faces_around_face(&self, face: FaceHandle) -> DynList<'_, FaceHandle> {
        // TODO: change to support polygons
        Box::new(self.faces_around_triangle(face).into_iter())
    }
}

impl FacesAroundVertex for HalfEdgeMesh {
    fn faces_around_vertex(
        &self,
        vh: VertexHandle,
    ) -> DynList<'_, FaceHandle> {
        Box::new(FaceCirculator {
            it: self.circulate_around(vh),
            mesh: self,
        })
    }
}

impl EToV for HalfEdgeMesh {
    fn endpoints_of_edge(&self, edge: EdgeHandle) -> [VertexHandle; 2] {
        let a = HalfEdgeHandle::one_half_of(edge);
        let b = a.twin();
        [self.half_edges[a].target, self.half_edges[b].target]
    }
}

impl EToF for HalfEdgeMesh {
    fn faces_of_edge(&self, edge: EdgeHandle) -> DiList<FaceHandle> {
        let a = HalfEdgeHandle::one_half_of(edge);
        let b = a.twin();
        DiList::from_options(
            self.half_edges[a].face.to_option(),
            self.half_edges[b].face.to_option(),
        )
    }
}


/// Iterator over all faces of a vertex. Is returned by `faces_around_vertex`.
struct FaceCirculator<'a> {
    it: CirculatorAroundVertex<'a>,
    mesh: &'a HalfEdgeMesh,
}

impl Iterator for FaceCirculator<'_> {
    type Item = FaceHandle;

    fn next(&mut self) -> Option<Self::Item> {
        // We simply skip the edge that don't have a face adjacent to them.
        let mesh = self.mesh;
        self.it.by_ref()
            .filter_map(|outgoing| mesh.half_edges[outgoing].face.to_option())
            .next()
    }
}

impl VerticesAroundVertex for HalfEdgeMesh {
    fn vertices_around_vertex(
        &self,
        vh: VertexHandle,
    ) -> DynList<'_, VertexHandle> {
        Box::new(VertexCirculator {
            it: self.circulate_around(vh),
            mesh: self,
        })
    }
}

/// Iterator over all neighbor vertices of a vertex. Is returned by
/// `vertices_around_vertex`.
struct VertexCirculator<'a> {
    it: CirculatorAroundVertex<'a>,
    mesh: &'a HalfEdgeMesh,
}

impl Iterator for VertexCirculator<'_> {
    type Item = VertexHandle;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|outgoing| self.mesh.half_edges[outgoing].target)
    }
}


// ===============================================================================================
// ===== Tests
// ===============================================================================================
#[cfg(test)]
mod test {
    use super::*;

    gen_tri_mesh_tests!(HalfEdgeMesh: [
        TriMesh,
        EdgeMesh,
        VerticesAroundFace,
        VerticesAroundVertex,
        FacesAroundFace,
        FacesAroundVertex,
        EToF,
        EToV,
        Manifold,
        SupportsMultiBlade
    ]);
}
