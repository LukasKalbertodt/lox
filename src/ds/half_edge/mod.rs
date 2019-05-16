//! Everything related to the `HalfEdgeMesh`.

// # Some notes for developers about this implementation
//
// - The twin half edges are stored implicitly: twins are always stored next to
//   one another in the underlying vector and thus always have handle indices
//   only one apart. Furthermore, since we start with the handle index 0, the
//   indices of two twins are always 2k and 2k + 1 where k is an integer.
// - We map edge handles to half edge handles by multiplying by two. Half edge
//   to edge is integer division by two. This works out very nicely: the edge
//   handle space is contiguous and the conversion operations are a simple
//   shift.

use std::{
    fmt,
    marker::PhantomData,
    ops,
    slice,
};

use crate::{
    prelude::*,
    handle::{hsize, Opt, Handle},
    map::VecMap,
    mesh::SplitEdgeWithFacesResult,
    traits::marker::{TriFaces, FaceKind, PolyFaces},
};
use super::Checked;
use self::adj::{CwVertexCirculator, FaceCirculator};


mod adj;
#[cfg(test)]
mod tests;



const NON_MANIFOLD_VERTEX_ERR: &str =
    "new face would add a non-manifold vertex (no hole found in cycle)";
const NON_MANIFOLD_EDGE_ERR: &str =
    "new face would add a non-manifold edge";



// ===============================================================================================
// ===== Compile time configuration of HalfEdgeMesh
// ===============================================================================================

/// Compile-time configuration for [`HalfEdgeMesh`].
///
/// To configure a half edge mesh, either use one of the existing types
/// implementing this trait, or create your own (preferably inhabitable) type
/// and implement this trait.
pub trait Config: 'static {
    /// What kind of faces are allowed in this half edge mesh.
    ///
    /// The data structure supports poly meshes, but if you only need triangle
    /// faces (and you really need a half edge mesh), restricting the faces to
    /// triangles here can speed up a few operations.
    ///
    /// The `HalfEdgeMesh` will forward this type to `Mesh::FaceKind` in the
    /// `Mesh` implementation.
    type FaceKind: FaceKind;

    // TODO:
    // - prev handles
    // - allow multi fan blades?
}

/// The standard configuration for the half edge mesh. Poly faces are
/// supported.
#[allow(missing_debug_implementations)]
pub enum PolyConfig {}
impl Config for PolyConfig {
    type FaceKind = PolyFaces;
}

#[allow(missing_debug_implementations)]
pub enum TriConfig {}
impl Config for TriConfig {
    type FaceKind = TriFaces;
}


// ===============================================================================================
// ===== HalfEdgeHandle
// ===============================================================================================

/// Handle to refer to half edges.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct HalfEdgeHandle(hsize);

impl HalfEdgeHandle {
    /// Returns the half-edge of the given edge with the lower index value.
    ///
    /// Again, due to our assumptions on how edges are stored, we just have to
    /// multiply the edges handle with 2 to get a corresponding half edge
    /// handle. This method does not check if the half edge actually exists.
    #[inline(always)]
    fn lower_half_of(edge: EdgeHandle) -> Self {
        Self(edge.idx() * 2)
    }

    /// Returns the full edge this half-edge belongs to.
    ///
    /// This works only because we know we store the half edges adjacent to one
    /// another. Of one edge, the half edge with the smaller index always has
    /// an even index, while the other one has an odd one. This means we can
    /// just integer divide by 2 and get the edge index.
    #[inline(always)]
    fn full_edge(self) -> EdgeHandle {
        EdgeHandle::new(self.0 / 2)
    }
}

impl Handle for HalfEdgeHandle {
    #[inline(always)]
    fn new(id: hsize) -> Self {
        HalfEdgeHandle(id)
    }

    #[inline(always)]
    fn idx(&self) -> hsize {
        self.0
    }
}

impl Checked<HalfEdgeHandle> {
    /// Returns the handle of the half edge twin (the half edge right next to
    /// this half edge, but pointing in the opposite direction).
    ///
    /// This method only works due to some assumptions about the data
    /// structure, so this is only valid together with data structure in this
    /// module! In particular, it assumes that two half edge twins are always
    /// stored right next to each other and that the handles start counting at
    /// an even number (0 in our case). Thus, we can simply flip the last bit
    /// of the handle id to get the twin handle.
    #[inline(always)]
    fn twin(self) -> Checked<HalfEdgeHandle> {
        Self::new(self.idx() ^ 1)
    }
}

impl fmt::Debug for HalfEdgeHandle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "HE{}", self.idx())
    }
}



// ===============================================================================================
// ===== Definition of types stored inside the data structure
// ===============================================================================================

/// TODO
#[derive(Empty)]
pub struct HalfEdgeMesh<C: Config = PolyConfig> {
    vertices: VecMap<VertexHandle, Vertex>,
    faces: VecMap<FaceHandle, Face>,
    half_edges: VecMap<HalfEdgeHandle, HalfEdge>,
    _config: PhantomData<C>,
}

/// Data stored per `Face`.
#[derive(Clone, Copy)]
pub(crate) struct Face {
    /// Handle of one (arbitrary) half edge adjacent to the face.
    edge: Checked<HalfEdgeHandle>,
}

/// Data stored per `Vertex`.
#[derive(Clone, Copy)]
pub(crate) struct Vertex {
    /// Handle of one outgoing half edge.
    ///
    /// - If the vertex is isolated, this is `None`.
    /// - If the vertex is a boundary vertex, this stores one arbitrary of the
    ///   boundary half edges. There only exists one such half edge per fan
    ///   blade.
    /// - If the vertex is not on the boundary, the half edge is completely
    ///   arbitrary.
    outgoing: Opt<Checked<HalfEdgeHandle>>,
}

/// Data stored per half edge.
#[derive(Clone, Copy)]
struct HalfEdge {
    /// The adjacent face, if one exists.
    face: Opt<Checked<FaceHandle>>,

    /// The vertex this half edge points to.
    target: Checked<VertexHandle>,

    /// The next half edge around the face or hole this half edge is adjacent
    /// to (going counter clock wise).
    next: Checked<HalfEdgeHandle>,
}

impl<C: Config> fmt::Debug for HalfEdgeMesh<C> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("HalfEdgeMesh")
            .field("vertices", &self.vertices)
            .field("faces", &self.faces)
            .field("half_edges", &self.half_edges)
            .finish()
    }
}

impl<C: Config> Clone for HalfEdgeMesh<C> {
    fn clone(&self) -> Self {
        Self {
            vertices: self.vertices.clone(),
            faces: self.faces.clone(),
            half_edges: self.half_edges.clone(),
            _config: PhantomData,
        }
    }
}

impl fmt::Debug for Vertex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Vertex {{ outgoing: {:?} }}", self.outgoing)
    }
}

impl fmt::Debug for Face {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Face {{ edge: {:?} }}", self.edge)
    }
}

impl fmt::Debug for HalfEdge {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "HalfEdge {{ target: {:5} next: {:6} face: {:?} }}",
            format!("{:?},", self.target),
            format!("{:?},", self.next),
            self.face,
        )
    }
}


// ===============================================================================================
// ===== Internal helper methods
// ===============================================================================================

impl<C: Config> HalfEdgeMesh<C> {
    /// Makes sure the given handle points to an existing element. If that's
    /// not the case, this method panics.
    fn check_face(&self, fh: FaceHandle) -> Checked<FaceHandle> {
        if self.faces.contains_handle(fh) {
            Checked(fh)
        } else {
            panic!(
                "{:?} was passed to a half edge mesh, but this face does not exist in this mesh",
                fh,
            );
        }
    }

    /// Makes sure the given handle points to an existing element. If that's
    /// not the case, this method panics.
    fn check_vertex(&self, vh: VertexHandle) -> Checked<VertexHandle> {
        if self.vertices.contains_handle(vh) {
            Checked(vh)
        } else {
            panic!(
                "{:?} was passed to a half edge mesh, but this vertex does not exist in this mesh",
                vh,
            );
        }
    }

    /// Makes sure the given handle points to an existing element. If that's
    /// not the case, this method panics. Otherwise, the half edge with the
    /// lower index is returned.
    fn checked_half_of(&self, eh: EdgeHandle) -> Checked<HalfEdgeHandle> {
        let heh = HalfEdgeHandle::lower_half_of(eh);
        if self.half_edges.contains_handle(heh) {
            Checked(heh)
        } else {
            panic!(
                "{:?} was passed to a half edge mesh, but this edge does not exist in this mesh",
                eh,
            );
        }
    }

    /// Returns an iterator the circulates around the face `center`. The
    /// iterator yields inner half edges.
    fn circulate_around_face(&self, center: Checked<FaceHandle>) -> FaceCirculator<'_, C> {
        // TODO: optimize for tri mesh
        let start_he = self[center].edge;
        FaceCirculator::NonEmpty {
            mesh: self,
            current_he: start_he,
            start_he,
        }
    }

    /// Returns an iterator the circulates around the vertex `center`. The
    /// iterator yields outgoing half edges.
    fn circulate_around_vertex(&self, center: Checked<VertexHandle>) -> CwVertexCirculator<'_, C> {
        match self[center].outgoing.to_option() {
            None => CwVertexCirculator::Empty,
            Some(start_he) => CwVertexCirculator::NonEmpty {
                mesh: self,
                current_he: start_he,
                start_he,
            }
        }
    }

    /// Tries to find the half edge from `from` to `to`. Returns `None` if
    /// there is no edge between the two vertices.
    fn he_between(
        &self,
        from: Checked<VertexHandle>,
        to: Checked<VertexHandle>,
    ) -> Option<Checked<HalfEdgeHandle>> {
        self.circulate_around_vertex(from)
            .find(|&outgoing| self[outgoing].target == to)
    }

    /// Returns the half edge whose `next` points to `he`.
    ///
    /// If `prev` handles would be stored, this would be easy. But since we
    /// don't store them, we have to circulate around the whole vertex.
    fn prev(&self, he: Checked<HalfEdgeHandle>) -> Checked<HalfEdgeHandle> {
        self.find_incoming_he(he.twin(), |incoming| self[incoming].next == he)
            .expect("internal HEM error: could not find `prev` half edge")
    }

    /// Tries to find a half edge pointing towards `start_edge.target` that
    /// satisfies the given predicate. Returns `None` if no edge around
    /// `start_edge.target` satisfying `predicate` is found.
    #[inline(always)]
    fn find_incoming_he(
        &self,
        start_edge: Checked<HalfEdgeHandle>,
        mut predicate: impl FnMut(Checked<HalfEdgeHandle>) -> bool,
    ) -> Option<Checked<HalfEdgeHandle>> {
        let mut incoming = start_edge;
        loop {
            if predicate(incoming) {
                return Some(incoming);
            }

            let next = self[incoming].next.twin();
            if next == start_edge {
                return None;
            }

            incoming = next;
        }
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
    fn add_edge_partially(
        &mut self,
        from: Checked<VertexHandle>,
        to: Checked<VertexHandle>,
    ) -> Checked<HalfEdgeHandle> {
        // Of course, wrapping a dummy handle into `Checked` is a bad idea.
        // Unfortunately, this is necessary. All code using this method has to
        // pay special attention anyway.
        let face = Opt::none();
        let next = Checked(HalfEdgeHandle::new(0));

        self.half_edges.push(HalfEdge { target: from, face, next });
        let out = self.half_edges.push(HalfEdge { target: to, face, next });

        Checked(out)
    }

    /// Adds a face defined by the given `vertices`.
    ///
    /// The function works pretty much like `MeshMut::add_face`. The
    /// `inner_half_edges` is just some storage this function can use. It is
    /// completely overwritten before it is read, so it can (should) be
    /// initialized with dummy values. It has to be the same length as
    /// `vertices`!
    fn add_face_impl<'a>(
        &mut self,
        vertices: &'a [VertexHandle],
        inner_half_edges: &mut [Checked<HalfEdgeHandle>],
    ) -> FaceHandle {
        // Check that all vertices are valid (the handles are referring to
        // existing vertices).
        for &vh in vertices {
            self.check_vertex(vh);
        }

        // We want to reflect our checks in the type system. Therefore we
        // change the type of `vertices` to `&[Checked<VertexHandle>]`. We
        // could copy everything into a `Vec`, but that allocates memory and is
        // a bit wasteful. Instead, we just reinterpret cast. This is actually
        // safe because `VertexHandle` and `Checked<VertexHandle>` have the
        // same memory layout, alignment requirements and everything (due to
        // `repr(transparent)`).
        let vertices = unsafe {
            slice::from_raw_parts::<'a>(
                vertices.as_ptr() as *const Checked<VertexHandle>,
                vertices.len(),
            )
        };

        // ===================================================================
        // ===== Find edges between vertices
        // ===================================================================
        // In this step, we find the inner edges of the new face. If some edges
        // are missing, we will add them in an incomplete form (e.g. `next` and
        // `outgoing` handles won't be changed anywhere).
        //
        // We could do this step by simply circulating around each vertex and
        // finding the connecting edges that way, but this is often not
        // optimal. We want to avoid unnecessary cache misses at all cost. If
        // we have found an edge between the previous pair of vertices already,
        // we can simply check its `next` handle: it's not unlikely that it is
        // already the edge we are looking for. If it's not the edge, we at
        // least already have an edge that we can use to circulate around the
        // vertex. That way we don't have to check the `outgoing` value of the
        // vertex, avoiding one memory access of potentially cold memory.
        //
        // `inner_half_edges` will store the half edges between the vertices of
        // the new face. The half edge at index `i` goes from `vertices[i]` to
        // `vertices[(i + 1) % len]]`.

        // If in the last iteration of the loop, an edge between the vertices
        // was found, it's stored here.
        let mut last_edge: Option<Checked<HalfEdgeHandle>> = None;

        for vi in 0..vertices.len() {
            let from = vertices[vi];
            let to = vertices[(vi + 1) % vertices.len()];

            let he = if let Some(last_edge) = last_edge {
                // We know the edge going from `vertices[i - 1]` to `from`.
                // That means we don't have to lookup `from` anymore to get an
                // adjacent edge. Furthermore, `last_edge.next` is probably
                // already the edge we are looking for!

                // TODO: if we do not allow multi-blades we don't have to loop!
                // Then the `next` has to be the edge we are looking for or we
                // know that the edge does not exist.

                // Edge starting at `from`.
                let mut outgoing = self[last_edge].next;

                loop {
                    // Check if we have found the edge we are looking for.
                    if self[outgoing].target == to {
                        break Some(outgoing);
                    }

                    // Check if we reached the starting point again (meaning
                    // there is no edge from `from` to `to`).
                    let ingoing = outgoing.twin();
                    if ingoing == last_edge {
                        break None;
                    }

                    outgoing = self[ingoing].next;
                }
            } else {
                // We have no previous edge, so we have to start at the vertex.
                self.he_between(from, to)
            };

            // Update the last edge
            last_edge = he;

            // Make sure the half edge we found is not connected to a face
            // already. This would mean that we would create a non-manifold
            // edge.
            if let Some(he) = he {
                assert!(self[he].face.is_none(), NON_MANIFOLD_EDGE_ERR);
            }

            let he = he.unwrap_or_else(|| self.add_edge_partially(from, to));
            inner_half_edges[vi] = he;
        }


        // ===================================================================
        // ===== Add face and fix `face` handle of inner edges
        // ===================================================================
        // Insert new face (it is `Checked` because we just added it).
        let new_face = self.faces.push(Face {
            edge: inner_half_edges[0], // just an arbitrary edge
        });
        let new_face = Checked(new_face);

        // Set the `face` handle of the inner edges.
        for he in &*inner_half_edges {
            self[*he].face = Opt::some(new_face);
        }


        // ===================================================================
        // ===== Fix `next` handles
        // ===================================================================
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
        for vi in 0..vertices.len() {
            let prev_idx = vi.checked_sub(1).unwrap_or(vertices.len() - 1);

            let vh = vertices[vi];
            let incoming = inner_half_edges[vi].twin();
            let outgoing = inner_half_edges[prev_idx].twin();

            let v = &self[vh];
            let incoming_face = self[incoming].face;
            let outgoing_face = self[outgoing].face;

            // We have four different cases: it just depends whether incoming
            // and/or outgoing are already adjacent to a face.
            match (incoming_face.is_some(), outgoing_face.is_some()) {
                // Both edge pairs were newly inserted. This is usually easy,
                // but it can be a bit tricky when there are other edges (and
                // thus a face) connected to that vertex.
                (false, false) => {
                    if let Some(outgoing_from_v) = v.outgoing.to_option() {
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

                        // TODO: if we have `prev` pointer, we can simplify
                        // this. Since we know `outgoing_from_v` is a boundary
                        // half edge, we can use it as `start` and
                        // `prev(start)` as `end`.

                        // Find the end edge of some blade.
                        let end = self.find_incoming_he(
                            outgoing_from_v.twin(),
                            |incoming| self[incoming].face.is_none(),
                        ).expect(NON_MANIFOLD_VERTEX_ERR);

                        // The start of another blade.
                        let start = self[end].next;

                        // Insert new blade in between.
                        self[incoming].next = start;
                        self[end].next = outgoing;

                        // Regarding the `outgoing` field of `v`: before adding
                        // this face, it was a boundary half edge. Since we
                        // didn't add a face adjacent to it, it still is. So we
                        // can keep it unchanged.
                    } else {
                        // This is the easy case: `incoming` and `outgoing` are
                        // the only edges adjacent to `v`. This also means that
                        // `v` was isolated before and we now need to set its
                        // `outgoing` handle.
                        self[incoming].next = outgoing;
                        self[vh].outgoing = Opt::some(outgoing);
                    }
                }

                // The incoming edge is adjacent to another face (IF), but the
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
                //         IF   //   \\
                //             //     \\
                //            //   F   \\
                //           /v         \v
                //          ( )         ( )
                //
                //                 ^-- this face and
                //                       ^-- this edge are new in the cycle
                //
                (true, false) => {
                    // TODO: should we rather iterate around the vertex if the
                    // `prev` point is not stored?
                    let before_new = self.prev(incoming.twin());
                    self[before_new].next = outgoing;

                    // The half edge `incoming.twin()` might have been
                    // `v.outgoing`. But this is bad because it's not a
                    // boundary half edge anymore (which we require). Therefore
                    // we update it to `outgoing` which is certainly a boundary
                    // half edge.
                    self[vh].outgoing = Opt::some(outgoing);
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
                    self[incoming].next = self[outgoing.twin()].next;

                    // We don't need to update `v.outgoing` here because the
                    // only old half edge that won't be boundary anymore is
                    // `outgoing.twin()`. But this is not an outgoing edge for
                    // `v`.
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
                //      <-------- (v) <--------
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
                //
                // Additionally, we might need to update `v.outgoing` because
                // it might have been `incoming.twin()` which is not a boundary
                // half edge anymore (after this method).
                (true, true) => {
                    // Find the end of the fan blade "IB". See below for
                    // explanation of the important fan blades.
                    //
                    // TODO: this should always find an edge or else the new
                    // face would introduce a non-manifold edge. Right?
                    let ib_end_opt = self.find_incoming_he(
                        incoming,
                        |incoming| self[incoming].face.is_none(),
                    );

                    if self[outgoing.twin()].next != incoming.twin() {
                        // Here we need to conceptually delete one fan blade
                        // from the `next` circle around `v` and re-insert it
                        // into the right position. We choose to "move" the fan
                        // blade starting with `incoming` (IB).
                        //
                        // We have to deal with four fan blades:
                        // - IB: the blade containing `incoming` (where
                        //   `incoming.twin()` is its start).
                        // - OB: the blade containing `outgoing` (where
                        //   `outgoing.twin()` is its end)
                        // - BIB (before incoming blade): the blade before IB
                        // - AOB (after outgoing blade): the blade after OB
                        //   (outgoing.twin.next is its start).
                        //
                        // Current situation:
                        //
                        //       ┌────┐    ┌─────┐         ┌─────┐    ┌────┐
                        //  +--> │ OB │ -> │ AOB │ -> ? -> │ BIB │ -> │ IB │ -> ?
                        //  |    └────┘    └─────┘         └─────┘    └────┘    |
                        //  +---------------------------------------------------+
                        //

                        // TODO: if we have `prev` pointer, we want to say
                        // `incoming.prev`. If not, however, we want to
                        // circulate around `v` to find the `bib_end` to avoid
                        // the worst case of finding the `prev` by walking
                        // around a huge part of the mesh.

                        // Find the end half edges of the blades BIB and IB.
                        let ib_end = ib_end_opt.expect("internal HEM error: cannot find `ib_end`");
                        let bib_end = self.prev(incoming.twin());

                        // Here we remove the "incoming blade" from the cycle.
                        // Situation after this assignment:
                        //
                        //                                  ┌────┐
                        //                                  │ IB │ -------+
                        //                                  └────┘        |
                        //                                                v
                        //       ┌────┐    ┌─────┐         ┌─────┐
                        //  +--> │ OB │ -> │ AOB │ -> ? -> │ BIB │ -----> ?
                        //  |    └────┘    └─────┘         └─────┘        |
                        //  +---------------------------------------------+
                        //
                        self[bib_end].next = self[ib_end].next;

                        // Now we reinsert it again, right after the "outgoing
                        // blade". Situation after assignment:
                        //
                        //
                        //       ┌────┐
                        //       │ IB │ ------+
                        //       └────┘       |
                        //                    v
                        //       ┌────┐    ┌─────┐         ┌─────┐
                        //  +--> │ OB │ -> │ AOB │ -> ? -> │ BIB │ -----> ?
                        //  |    └────┘    └─────┘         └─────┘        |
                        //  +---------------------------------------------+
                        //
                        let aob_start = self[outgoing.twin()].next;
                        self[ib_end].next = aob_start;

                        // Right now, the cycle is still a bit broken, but that
                        // doesn't matter, because (a) the cycle will be
                        // repaired by setting the `next` link of the inner
                        // edges below, and (b) the broken cycle won't be
                        // accessed (in this direction) before it is repaired.

                        // To update `v.outgoing`, we luckily already know a
                        // boundary outgoing half edge of `v`: it's the start
                        // of AOB.
                        self[vh].outgoing = Opt::some(aob_start);
                    } else {
                        // The order of fan blades around the vertex is fine,
                        // but we might need to update `v.outgoing`. To do
                        // that, we try to find the end of IB. Its `next` half
                        // edge is the start of the next blade (which is an
                        // outgoing edge). However, we might not find the end
                        // of that blade because there might only be one blade
                        // left. In that case, we don't update `outgoing`
                        // because it can be an arbitrary half edge in that
                        // case (the vertex won't be boundary anymore after
                        // this method call).
                        if let Some(ib_end) = ib_end_opt {
                            let new_outgoing = self[ib_end].next;
                            self[vh].outgoing = Opt::some(new_outgoing);
                        }
                    }
                }
            }
        }

        // Now we only need to set the `next` handles of the inner half edges.
        // This is easy.
        for he_i in 0..inner_half_edges.len() {
            let curr = inner_half_edges[he_i];
            let next = inner_half_edges[(he_i + 1) % inner_half_edges.len()];
            self[curr].next = next;
        }

        *new_face
    }
}

macro_rules! impl_index {
    ($handle:ident, $field:ident, $out:ident) => {
        impl<C: Config> ops::Index<Checked<$handle>> for HalfEdgeMesh<C> {
            type Output = $out;

            #[inline(always)]
            fn index(&self, idx: Checked<$handle>) -> &Self::Output {
                // &self.$field[*idx]
                unsafe { self.$field.get_unchecked(*idx) }
            }
        }

        impl<C: Config> ops::IndexMut<Checked<$handle>> for HalfEdgeMesh<C> {
            #[inline(always)]
            fn index_mut(&mut self, idx: Checked<$handle>) -> &mut Self::Output {
                // &mut self.$field[*idx]
                unsafe { self.$field.get_unchecked_mut(*idx) }
            }
        }
    }
}

impl_index!(VertexHandle, vertices, Vertex);
impl_index!(FaceHandle, faces, Face);
impl_index!(HalfEdgeHandle, half_edges, HalfEdge);


// ===============================================================================================
// ===== Mesh trait implementations
// ===============================================================================================

impl<C: Config> Mesh for HalfEdgeMesh<C> {
    type FaceKind = C::FaceKind;

    fn num_vertices(&self) -> hsize {
        self.vertices.num_elements()
    }

    fn next_vertex_handle_from(&self, start: VertexHandle) -> Option<VertexHandle> {
        // TODO: optimize
        (start.idx()..self.vertices.next_push_handle().idx())
            .map(VertexHandle::new)
            .find(|&vh| self.vertices.contains_handle(vh))
    }

    fn next_face_handle_from(&self, start: FaceHandle) -> Option<FaceHandle> {
        // TODO: optimize
        (start.idx()..self.faces.next_push_handle().idx())
            .map(FaceHandle::new)
            .find(|&fh| self.faces.contains_handle(fh))
    }

    fn last_vertex_handle(&self) -> Option<VertexHandle> {
        self.vertices.last_handle()
    }
    fn last_face_handle(&self) -> Option<FaceHandle> {
        self.faces.last_handle()
    }

    fn contains_vertex(&self, vertex: VertexHandle) -> bool {
        self.vertices.contains_handle(vertex)
    }

    fn num_faces(&self) -> hsize {
        self.faces.num_elements()
    }

    fn contains_face(&self, face: FaceHandle) -> bool {
        self.faces.contains_handle(face)
    }
}

impl<C: Config> MeshMut for HalfEdgeMesh<C> {
    fn add_vertex(&mut self) -> VertexHandle {
        self.vertices.push(Vertex {
            outgoing: Opt::none()
        })
    }

    fn add_triangle(&mut self, [a, b, c]: [VertexHandle; 3]) -> FaceHandle {
        assert_ne!(a, b, "vertices of new face are not unique");
        assert_ne!(a, c, "vertices of new face are not unique");

        self.add_face_impl(&[a, b, c], &mut [Checked::<HalfEdgeHandle>::new(0); 3])
    }

    #[inline(never)]
    fn reserve_for_vertices(&mut self, count: hsize) {
        self.vertices.reserve(count);
    }

    #[inline(never)]
    fn reserve_for_faces(&mut self, count: hsize) {
        // We have at least three half edges per face
        self.half_edges.reserve(count * 3);
        self.faces.reserve(count);
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
        let f = self.check_face(f);

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

        // TODO: reserve memory/pre alloc all new elements. Currently this
        // function generates suboptimal assembly, because each `push` of a new
        // element goes through the whole `push()` code with check for capacity
        // overflow and realloc call. This should be improved.

        // Add new vertex "in the middle".
        let midpoint = Checked(self.add_vertex());

        // Pick arbitrary start edge and vertex. In the example above that are
        // `(A) -> (B)` and (A), respectively.
        let start_ohe = self[f].edge;
        let start_vertex = self[start_ohe.twin()].target;

        // Add first edge and set midpoint's `outgoing` to that edge. The
        // midpoint is not a boundary vertex, we don't have to pay attention to
        // the `outgoing` edge.
        let start_nhe = self.add_edge_partially(midpoint, start_vertex);
        self[midpoint].outgoing = Opt::some(start_nhe);

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
        while self[border_ohe].target != start_vertex {
            // We store the next iteration edge now already, because we will
            // overwrite the `next` value below.
            let next_border_ohe = self[border_ohe].next;

            // Add the a edge
            let next_vertex = self[border_ohe].target;
            let next_nhe = self.add_edge_partially(midpoint, next_vertex);

            // Add the new face. This includes setting the `next` and `face`
            // fields of the edges of the new triangle. These are:
            // `border_ohe`, `last_nhe` and `next_nhe.twin()`.
            let inner_new = next_nhe.twin();
            let new_face = Checked(self.faces.push(Face { edge: inner_new }));

            self[inner_new].next = last_nhe;
            self[last_nhe].next = border_ohe;
            self[border_ohe].next = inner_new;

            self[inner_new].face = Opt::some(new_face);
            self[last_nhe].face = Opt::some(new_face);
            self[border_ohe].face = Opt::some(new_face);


            // Advance the iteration edges
            last_nhe = next_nhe;
            border_ohe = next_border_ohe;
        }

        // "Add" the last face. We are reusing the existing one.
        let start_inner_nhe = start_nhe.twin();
        self[f].edge = start_inner_nhe;

        self[start_inner_nhe].next = last_nhe;
        self[last_nhe].next = border_ohe;
        self[border_ohe].next = start_inner_nhe;

        self[start_inner_nhe].face = Opt::some(f);
        self[last_nhe].face = Opt::some(f);
        // `border_ohe.face` is already `f`


        *midpoint
    }
}

impl<C: Config> EdgeMesh for HalfEdgeMesh<C> {
    fn num_edges(&self) -> hsize {
        // There are always exactly twice as many half edge as there are edges
        self.half_edges.num_elements() / 2
    }

    fn next_edge_handle_from(&self, start: EdgeHandle) -> Option<EdgeHandle> {
        // TODO: optimize
        let mut idx = HalfEdgeHandle::lower_half_of(start).idx();
        let end = self.half_edges.next_push_handle().idx();
        while idx < end {
            let he = HalfEdgeHandle::new(idx);
            if self.half_edges.contains_handle(he) {
                return Some(he.full_edge());
            }

            idx += 2;
        }

        None
    }

    fn last_edge_handle(&self) -> Option<EdgeHandle> {
        self.half_edges.last_handle().map(|he| he.full_edge())
    }

    fn contains_edge(&self, edge: EdgeHandle) -> bool {
        let he = HalfEdgeHandle::lower_half_of(edge);
        self.half_edges.contains_handle(he)
    }
}

impl<C> PolyMeshMut for HalfEdgeMesh<C>
where
    C: Config<FaceKind = PolyFaces>,
{
    fn add_face(&mut self, vertices: &[VertexHandle]) -> FaceHandle {
        assert!(
            vertices.len() >= 3,
            "attempt to add a face with only {} vertices",
            vertices.len(),
        );
        // TODO: check uniqueness of vertices?

        let mut inner_half_edges = vec![Checked(HalfEdgeHandle::new(0)); vertices.len()];
        self.add_face_impl(vertices, &mut inner_half_edges)
    }
}

impl<C> TriEdgeMeshMut for HalfEdgeMesh<C>
where
    C: Config<FaceKind = TriFaces>,
{
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
        //  a: he_center_above            [A]: v_left             (X): f_above
        //  b: he_center_below            [B]: v_right            (Y): f_below
        //  c: he_above_left              [C]: v_above
        //  d: he_above_right             [D]: v_below
        //  e: he_below_left
        //  f: he_below_right
        //
        //
        // We just imagine that the "random" half-edge we get from
        // `lower_half_of()` is the edge `a` in the drawing.

        // First, let's just obtain all handles
        let he_center_above = self.checked_half_of(edge);
        let he_center_below = he_center_above.twin();
        let he_above_right = self[he_center_above].next;
        let he_above_left = self[he_above_right].next;
        let he_below_left = self[he_center_below].next;
        let he_below_right = self[he_below_left].next;

        let faces = [
            self[he_center_above].face.to_option(),
            self[he_center_below].face.to_option(),
        ];
        let (f_above, f_below) = match faces {
            [Some(above), Some(below)] => (above, below),
            _ => {
                panic!("`flip_edge` called on boundary edge {:?}", edge);
            }
        };

        let v_right = self[he_center_above].target;
        let v_left = self[he_center_below].target;
        let v_above = self[he_above_right].target;
        let v_below = self[he_below_left].target;


        // Update all fields.
        //
        // We have to pay extra attention to the `outgoing` fields of the
        // vertices (they have to point to an adjacent boundary edge if there
        // exists one). We only update them if we need to. And if we need to,
        // their old `outgoing` edge was a center one, which is not boundary,
        // which means that the vertex is also not a boundary vertex. So we can
        // simply set the new one.
        if self[v_left].outgoing == Opt::some(he_center_above) {
            self[v_left].outgoing = Opt::some(he_below_left);
        }
        if self[v_right].outgoing == Opt::some(he_center_below) {
            self[v_right].outgoing = Opt::some(he_above_right);
        }

        self[f_above].edge = he_center_above;
        self[f_below].edge = he_center_below;

        self[he_center_above].target = v_below;
        self[he_center_above].next = he_below_right;
        self[he_center_below].target = v_above;
        self[he_center_below].next = he_above_left;

        self[he_below_right].face = Opt::some(f_above);
        self[he_below_right].next = he_above_right;
        self[he_above_right].next = he_center_above;

        self[he_above_left].face = Opt::some(f_below);
        self[he_above_left].next = he_below_left;
        self[he_below_left].next = he_center_below;
    }

    fn split_edge_with_faces(&mut self, edge: EdgeHandle) -> SplitEdgeWithFacesResult {
        // ===================================================================
        // ===== Split just the edge
        // ===================================================================

        // Situation now (ignoring faces):
        //
        //                      above
        //   (left) -----------------------------> (right)
        //          <-----------------------------
        //                      below
        //
        // Of course, these names don't fit to any real property of the mesh
        // elements. We just call them like that to fit to the ASCII art.
        let he_above = self.checked_half_of(edge);
        let he_below = he_above.twin();
        let v_right = self[he_above].target;

        let he_below_prev = self.prev(he_below);

        // We need to insert a new vertex and two new half edge. Goal:
        //
        //             above           new_above
        //   (left) ----------> (mid) -----------> (right)
        //          <---------- (mid) <-----------
        //             below           new_below
        //
        let v_mid = Checked(self.add_vertex());
        let he_new_above = self.add_edge_partially(v_mid, v_right);
        let he_new_below = he_new_above.twin();

        // Fix next handles
        self[he_new_above].next = self[he_above].next;
        self[he_above].next = he_new_above;
        self[he_above].target = v_mid;
        self[he_new_below].next = he_below;
        self[he_below_prev].next = he_new_below;

        // If the `outgoing` handle of `right` was `he_below`, we have to
        // update it to `he_new_below`. Otherwise it should stay as it is
        // (because of the boundary-condition).
        if self[v_right].outgoing == Opt::some(he_below) {
            self[v_right].outgoing = Opt::some(he_new_below)
        }

        // Set a fitting `outgoing` edge for mid-vertex
        let face_above = self[he_above].face.to_option();
        let face_below = self[he_below].face.to_option();
        let outgoing = match (face_above.is_some(), face_below.is_some()) {
            // No face above but below, `he_new_above` is boundary edge, we
            // need to use that edge.
            (false, true) => he_new_above,

            // No face below but above, `he_below` is boundary edge, we need to
            // use this.
            (true, false) => he_below,

            // Either both edges are boundary edges or `mid` is not a boundary
            // vertex. In either case, it doesn't matter.
            (false, false) | (true, true) => he_new_above,
        };
        self[v_mid].outgoing = Opt::some(outgoing);


        // ===================================================================
        // ===== Split the (up to two) faces
        // ===================================================================
        let split_face = |
            mesh: &mut Self,
            old_face: Checked<FaceHandle>,
            he_bottom_left: Checked<HalfEdgeHandle>,
            he_bottom_right: Checked<HalfEdgeHandle>,
        | {
            // Current situation:
            //
            //                      (top)
            //                    ⟋      ↖
            //                  ⟋          ⟍
            //      top_left  ⟋              ⟍  top_right
            //              ⟋                  ⟍
            //            ⟋                      ⟍
            //          ↙                           ⟍
            //   (left) ----------> (mid) -----------> (right)
            //          bottom_left       bottom_right
            //
            // Goal:
            //
            //                      (top)
            //                    ⟋  ^ |  ↖
            //                  ⟋    | |    ⟍                      1: mid_right
            //      top_left  ⟋      | |      ⟍  top_right         2: mid_left
            //              ⟋      2 | | 1      ⟍
            //            ⟋          | |          ⟍
            //          ↙            | v             ⟍
            //   (left) ----------> (mid) -----------> (right)
            //          bottom_left       bottom_right
            //
            // The old face will be the left one.

            let he_top_right = mesh[he_bottom_right].next;
            let v_top = mesh[he_top_right].target;
            let he_top_left = mesh[he_top_right].next;

            let he_mid_left = mesh.add_edge_partially(v_mid, v_top);
            let he_mid_right = he_mid_left.twin();

            // Fix left face
            mesh[he_bottom_left].next = he_mid_left;
            mesh[he_bottom_left].face = Opt::some(old_face);
            mesh[he_mid_left].next = he_top_left;
            mesh[he_mid_left].face = Opt::some(old_face);
            mesh[old_face].edge = he_bottom_left;

            // Create and fix right face
            let right_face = Checked(mesh.faces.push(Face { edge: he_bottom_right }));
            mesh[he_mid_right].face = Opt::some(right_face);
            mesh[he_bottom_right].face = Opt::some(right_face);
            mesh[he_top_right].face = Opt::some(right_face);
            mesh[he_top_right].next = he_mid_right;
            mesh[he_mid_right].next = he_bottom_right;
        };

        if let Some(face) = face_above {
            split_face(self, face, he_above, he_new_above);
        }
        if let Some(face) = face_below {
            split_face(self, face, he_new_below, he_below);
        }


        SplitEdgeWithFacesResult {
            vertex: *v_mid,
            replacement_edges: [he_above.full_edge(), he_new_above.full_edge()],
        }
    }
}

impl<C: Config> SupportsMultiBlade for HalfEdgeMesh<C> {}
