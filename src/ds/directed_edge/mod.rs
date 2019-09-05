//! Everything related to the `DirectedEdgeMesh`.

#![allow(unused_imports)] // TODO

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

use optional::Optioned as Opt;

use crate::{
    prelude::*,
    handle::{hsize, Handle},
    map::VecMap,
    mesh::SplitEdgeWithFacesResult,
    traits::marker::{Bool, False, TriFaces},
    traits::adj::{HandleIterFamily},
    util::{DiList, TriList},
};
use super::{Checked, TypeOrVoid};
use self::adj::{CwVertexCirculator, CwVertexCirculatorState};


mod adj;
#[cfg(test)]
mod tests;



// const NON_MANIFOLD_VERTEX_ERR: &str =
//     "new face would add a non-manifold vertex (no hole found in cycle)";
const NON_MANIFOLD_EDGE_ERR: &str =
    "new face would add a non-manifold edge";



// ===============================================================================================
// ===== Compile time configuration of HalfEdgeMesh
// ===============================================================================================

/// Compile-time configuration for [`DirectedEdgeMesh`].
///
/// To configure a half edge mesh, either use one of the existing types
/// implementing this trait, or create your own (preferably inhabitable) type
/// and implement this trait.
pub trait Config: 'static {
    type StoreNext: Bool;
    type StorePrev: Bool;

    // TODO:
    // - allow multi fan blades?
    // - source/target vertex
}

/// The standard configuration for the half edge mesh. Poly faces are
/// supported.
#[allow(missing_debug_implementations)]
pub enum DefaultConfig {}
impl Config for DefaultConfig {
    type StoreNext = False;
    type StorePrev = False;
}



// ===============================================================================================
// ===== HalfEdgeHandle and handle helper
// ===============================================================================================

/// Handle to refer to half edges.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct HalfEdgeHandle(hsize);

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

impl HalfEdgeHandle {
    #[inline(always)]
    fn first_around(fh: FaceHandle) -> Self {
        Self::new(fh.idx() * 3)
    }

    #[inline(always)]
    fn face(&self) -> FaceHandle {
        FaceHandle::new(self.idx() / 3)
    }
}

impl fmt::Debug for HalfEdgeHandle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "HE{}", self.idx())
    }
}



/// TODO
#[derive(Empty)]
pub struct DirectedEdgeMesh<C: Config = DefaultConfig> {
    vertices: VecMap<VertexHandle, Vertex>,
    half_edges: VecMap<HalfEdgeHandle, HalfEdge<C>>,
    _config: PhantomData<C>,
}

/// Data stored per `Vertex`.
#[derive(Clone, Copy)]
pub(crate) struct Vertex {
    /// Handle of one outgoing half edge. If the vertex is a boundary vertex,
    /// this points to one of the boundary half edges.
    outgoing: Opt<Checked<HalfEdgeHandle>>,
}


/// The type of the `twin` field, which has a special encoding.
///
/// If the edge a half edge belongs to is interior, its `twin` field simply
/// points to the other half edge of that edge. If however, the edge is a
/// boundary edge, things get complicated.
///
/// The original paper suggest to store -1 to indicate a boundary edge and -2
/// to indicate a non-manifold edge. Since this library does not support
/// non-manifold edges anyway, we don't need the -2 sentinal value. The
/// original paper also does not describe a way to iterate along the boundary
/// of a mesh. We can improve this by using this field to store the next
/// boundary edge (kind of).
///
/// Let's take a look at the following mesh: edges a to d are boundary
/// edges.
///
/// ```text
///    a       b       c       d
///  ----- o ----- o ----- o -----
///       / \     / \     / \
///      /   \   /   \   /   \
///     /     \ /     \ /     \
///            o       o
///               ...
/// ```
///
/// Boundary *half* edges are never stored, but the inner halves of a to d
/// are stored. Since they belong to a boundary edge, their `twin` field
/// does not point to a twin! Instead, the handle's most significant bit
/// (sign bit) is set and the remaining bits form a handle which points to
/// the half edge of the "next" boundary edge. Here, "next" is defined like
/// the next around faces: counter clock-wise. Note that "counter
/// clock-wise" makes intuitive sense when talking about holes in the mesh,
/// but is "intuitively" inverted when talking about outer boundaries.
///
/// In this case, `a.twin` has its sign bit set and points to `b`. `b` to
/// `c` and so on.
///
/// As this type deals with a lot of `Checked<HalfEdgeHandle>`s, many functions
/// contain `unsafe`. This is always fine as long as the user creates instances
/// of this type only from valid handles. But as the constructors already take
/// `Checked` handles, we know those are valid.
///
/// TODO: Think about maybe not using the first bit, but instead comparing this
/// number to the length of the half edge vector. That way we can utilize the
/// whole handle space.
#[derive(Clone, Copy, PartialEq)]
struct EncodedTwin(hsize);

const TWIN_MASK: hsize = 1 << (std::mem::size_of::<hsize>() * 8 - 1);

impl EncodedTwin {
    /// Returns a dummy value that has to be overwritten!
    #[inline(always)]
    unsafe fn dummy() -> Self {
        Self(0)
    }

    #[inline(always)]
    fn next_boundary_he(he: Checked<HalfEdgeHandle>) -> Self {
        Self(he.idx() | TWIN_MASK)
    }

    #[inline(always)]
    fn twin(he: Checked<HalfEdgeHandle>) -> Self {
        Self(he.idx())
    }

    /// Decode it into the easier to use form.
    fn decode(&self) -> Twin {
        if self.is_real_twin() {
            // See type documentation on `unsafe`
            unsafe { Twin::Twin(Checked::new(HalfEdgeHandle::new(self.0))) }
        } else {
            // See type documentation on `unsafe`
            unsafe { Twin::NextBoundaryHe(Checked::new(HalfEdgeHandle::new(self.0 & !TWIN_MASK))) }
        }
    }

    fn or_next_boundary_he(&self) -> Checked<HalfEdgeHandle> {
        // See type documentation on `unsafe`
        unsafe { Checked::new(HalfEdgeHandle::new(self.0 & !TWIN_MASK)) }
    }

    /// Create a new encoded twin that is always a "next boundary half edge"
    /// and uses the half edge that is stored in `self`. The purpose of the
    /// half edge in `self` is ignored.
    fn to_next_boundary_he(&self) -> EncodedTwin {
        Self(self.0 | TWIN_MASK)
    }

    #[inline(always)]
    fn is_real_twin(&self) -> bool {
        self.0 & TWIN_MASK == 0
    }

    fn as_real_twin(&self) -> Option<Checked<HalfEdgeHandle>> {
        if self.is_real_twin() {
            // See type documentation on `unsafe`
            unsafe { Some(Checked::new(HalfEdgeHandle::new(self.0))) }
        } else {
            None
        }
    }

    fn as_next_boundary_he(&self) -> Option<Checked<HalfEdgeHandle>> {
        if self.is_real_twin() {
            None
        } else {
            // See type documentation on `unsafe`
            unsafe { Some(Checked::new(HalfEdgeHandle::new(self.0 & !TWIN_MASK))) }
        }
    }
}

#[derive(Clone, Copy)]
enum Twin {
    Twin(Checked<HalfEdgeHandle>),
    NextBoundaryHe(Checked<HalfEdgeHandle>),
}

type NextHandle<C> = <<C as Config>::StoreNext as TypeOrVoid<Checked<HalfEdgeHandle>>>::Output;
type PrevHandle<C> = <<C as Config>::StorePrev as TypeOrVoid<Checked<HalfEdgeHandle>>>::Output;

fn new_next_handle<C: Config>(h: Checked<HalfEdgeHandle>) -> NextHandle<C> {
    <C::StoreNext as TypeOrVoid<Checked<HalfEdgeHandle>>>::new(h)
}
fn new_prev_handle<C: Config>(h: Checked<HalfEdgeHandle>) -> PrevHandle<C> {
    <C::StorePrev as TypeOrVoid<Checked<HalfEdgeHandle>>>::new(h)
}


/// Data stored per half edge.
pub(crate) struct HalfEdge<C: Config> {
    /// The specially encoded twin. See [`EncodedTwin`] for more information.
    twin: EncodedTwin,

    /// The vertex this half edge points to.
    target: Checked<VertexHandle>,

    /// TODO
    next: NextHandle<C>,

    /// TODO
    prev: PrevHandle<C>,
}

impl<C: Config> HalfEdge<C> {
    /// Returns an instance with only `target` initialized to the given value
    /// while all other fields contain dummy values. These fields need to be
    /// overwritten since they contain bogus information.
    unsafe fn dummy_to(target: Checked<VertexHandle>) -> Self {
        Self {
            twin: EncodedTwin::dummy(),
            target,
            next: new_next_handle::<C>(Checked::new(HalfEdgeHandle::new(0))),
            prev: new_prev_handle::<C>(Checked::new(HalfEdgeHandle::new(0))),
        }
    }

    fn is_boundary(&self) -> bool {
        !self.twin.is_real_twin()
    }
}


impl<C: Config> fmt::Debug for DirectedEdgeMesh<C> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("DirectedEdgeMesh")
            .field("vertices", &self.vertices)
            .field("half_edges", &self.half_edges)
            .finish()
    }
}

impl<C: Config> Clone for DirectedEdgeMesh<C> {
    fn clone(&self) -> Self {
        Self {
            vertices: self.vertices.clone(),
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

impl fmt::Debug for EncodedTwin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.decode().fmt(f)
    }
}

impl fmt::Debug for Twin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Twin::Twin(h) => write!(f, "Twin({:?})", h),
            Twin::NextBoundaryHe(h) => write!(f, "NextB({:?})", h),
        }
    }
}

impl<C: Config> Copy for HalfEdge<C> {}
impl<C: Config> Clone for HalfEdge<C> {
    fn clone(&self) -> Self {
        Self {
            twin: self.twin.clone(),
            target: self.target.clone(),
            next: self.next.clone(),
            prev: self.prev.clone(),
        }
    }
}

impl<C: Config> fmt::Debug for HalfEdge<C> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let next = match C::StoreNext::VALUE {
            true => format!(" next: {:6}", format!("{:?},", self.next)),
            false => format!(""),
        };
        let prev = match C::StorePrev::VALUE {
            true => format!(" prev: {:?}", self.prev),
            false => format!(""),
        };

        write!(
            f,
            "HalfEdge {{ target: {:5} twin: {:13}{}{} }}",
            format!("{:?},", self.target),
            format!("{:?},", self.twin),
            next,
            prev,
        )
    }
}

// ===============================================================================================
// ===== Internal helper methods
// ===============================================================================================
impl<C: Config> DirectedEdgeMesh<C> {
    /// Makes sure the given handle points to an existing element. If that's
    /// not the case, this method panics.
    fn check_vertex(&self, vh: VertexHandle) -> Checked<VertexHandle> {
        if self.vertices.contains_handle(vh) {
            // We just checked `vh` is valid, so `Checked::new` is correct
            unsafe { Checked::new(vh) }
        } else {
            panic!(
                "{:?} was passed to a half edge mesh, but this vertex does not exist in this mesh",
                vh,
            );
        }
    }

    fn check_face(&self, fh: FaceHandle) {
        let heh = HalfEdgeHandle::first_around(fh);
        if !self.half_edges.contains_handle(heh) {
            panic!(
                "{:?} was passed to a directed edge mesh, but this face does not \
                    exist in this mesh",
                fh,
            );
        }
    }

    /// Makes sure the given handle points to an existing element. If that's
    /// not the case, this method panics.
    fn checked_half_edges_around(&self, fh: FaceHandle) -> [Checked<HalfEdgeHandle>; 3] {
        let heh = HalfEdgeHandle::first_around(fh);
        if self.half_edges.contains_handle(heh) {
            // These handles actually exist due to the basic way this data
            // structure works. For each face three half edges are stored.
            unsafe { [
                Checked::new(heh),
                Checked::new(HalfEdgeHandle::new(heh.idx() + 1)),
                Checked::new(HalfEdgeHandle::new(heh.idx() + 2)),
            ] }
        } else {
            panic!(
                "{:?} was passed to a directed edge mesh, but this face does not \
                    exist in this mesh",
                fh,
            );
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

    /// Returns an iterator the circulates around the vertex `center`. The
    /// iterator yields outgoing half edges.
    fn circulate_around_vertex(&self, center: Checked<VertexHandle>) -> CwVertexCirculator<'_, C> {
        let state = match self[center].outgoing.into_option() {
            None => CwVertexCirculatorState::Empty,
            Some(start_he) => CwVertexCirculatorState::NonEmpty {
                current_he: start_he,
                start_he,
            }
        };

        CwVertexCirculator { mesh: self, state }
    }

    fn next_he(&self, he: Checked<HalfEdgeHandle>) -> Checked<HalfEdgeHandle> {
        // TODO: use `next` value if available

        // The first HE of the face is `(he.idx() / 3) * 3`. If the given HE is
        // the first or second half edge of the face, we just need to add one.
        // If it's the third HE of the face, we need to subtract 2. But how to
        // check if it's the third HE? `(id + 1) % 3 == 0` does that.
        let idx = he.idx() + 1;
        let next = if is_divisible_by_3(idx) {
            idx - 3
        } else {
            idx
        };

        // This handle actually exists by the logic above. Three half edges
        // always exist together.
        unsafe { Checked::new(HalfEdgeHandle::new(next)) }
    }

    fn prev_he(&self, he: Checked<HalfEdgeHandle>) -> Checked<HalfEdgeHandle> {
        // TODO: use `prev` value if available

        // See `next_he` for explanation on this code.
        let prev = if is_divisible_by_3(he.idx()) {
            he.idx() + 2
        } else {
            he.idx() - 1
        };

        // This handle actually exists by the logic above. Three half edges
        // always exist together.
        unsafe { Checked::new(HalfEdgeHandle::new(prev)) }
    }

    /// Sets the `twin` handle of both half edges to each other.
    fn set_twins(&mut self, a: Checked<HalfEdgeHandle>, b: Checked<HalfEdgeHandle>) {
        self[a].twin = EncodedTwin::twin(b);
        self[b].twin = EncodedTwin::twin(a);
    }

    /// Pushes the three given half edges (in order) and returns their handles.
    fn push_half_edge_triple(
        &mut self,
        [a, b, c]: [HalfEdge<C>; 3],
    ) -> [Checked<HalfEdgeHandle>; 3] {
        let ah = self.half_edges.push(a);
        let bh = self.half_edges.push(b);
        let ch = self.half_edges.push(c);

        // The handles are clearly valid as we just pushed them.
        unsafe { [Checked::new(ah), Checked::new(bh), Checked::new(ch)] }
    }
}

/// Returns `true` if and only if `idx` is divisible by 3. Basically `(id + 1)
/// % 3 == 0` but hand-optimized.
///
/// Unfortunately, LLVM is not smart enough to correctly optimize that code.
/// That's why this is hand-micro-optimized. The divisibility-check is
/// well-known and described for example here:
/// http://clomont.com/efficient-divisibility-testing/
#[inline(always)]
fn is_divisible_by_3(idx: hsize) -> bool {
    // We have to do different things depending on the handle size. The
    // argument types of the inner functions are `u32` and `u64` to assure this
    // function is correct.

    #[cfg(not(feature = "large-handle"))]
    #[inline(always)]
    fn check(idx: u32) -> bool {
        idx.wrapping_mul(0xaaaaaaab) <= 0x55555555
    }

    #[cfg(feature = "large-handle")]
    #[inline(always)]
    fn check(idx: u64) -> bool {
        idx.wrapping_mul(0xaaaaaaaaaaaaaaab) <= 0x5555555555555555
    }

    check(idx)
}

macro_rules! impl_index {
    ($handle:ident, $field:ident, $c:ident, $out:ty) => {
        impl<$c: Config> ops::Index<Checked<$handle>> for DirectedEdgeMesh<$c> {
            type Output = $out;

            #[inline(always)]
            fn index(&self, idx: Checked<$handle>) -> &Self::Output {
                // &self.$field[*idx]
                unsafe { self.$field.get_unchecked(*idx) }
            }
        }

        impl<$c: Config> ops::IndexMut<Checked<$handle>> for DirectedEdgeMesh<$c> {
            #[inline(always)]
            fn index_mut(&mut self, idx: Checked<$handle>) -> &mut Self::Output {
                // &mut self.$field[*idx]
                unsafe { self.$field.get_unchecked_mut(*idx) }
            }
        }
    }
}

impl_index!(VertexHandle, vertices, C, Vertex);
impl_index!(HalfEdgeHandle, half_edges, C, HalfEdge<C>);


// ===============================================================================================
// ===== Mesh trait implementations
// ===============================================================================================

impl<C: Config> Mesh for DirectedEdgeMesh<C> {
    type FaceKind = TriFaces;

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
        (start.idx()..self.half_edges.next_push_handle().idx() / 3)
            .map(|i| HalfEdgeHandle::new(i * 3))
            .find(|&heh| self.half_edges.contains_handle(heh))
            .map(|he| he.face())
    }

    fn last_vertex_handle(&self) -> Option<VertexHandle> {
        self.vertices.last_handle()
    }
    fn last_face_handle(&self) -> Option<FaceHandle> {
        self.half_edges.last_handle().map(|he| he.face())
    }

    fn contains_vertex(&self, vertex: VertexHandle) -> bool {
        self.vertices.contains_handle(vertex)
    }

    fn num_faces(&self) -> hsize {
        self.half_edges.num_elements() / 3
    }

    fn contains_face(&self, face: FaceHandle) -> bool {
        self.half_edges.contains_handle(HalfEdgeHandle::first_around(face))
    }

    fn num_edges(&self) -> hsize
    where
        Self: EdgeMesh
    {
        unreachable!()
    }

    fn next_edge_handle_from(&self, _: EdgeHandle) -> Option<EdgeHandle>
    where
        Self: EdgeMesh
    {
        unreachable!()
    }

    fn last_edge_handle(&self) -> Option<EdgeHandle>
    where
        Self: EdgeMesh
    {
        unreachable!()
    }

    fn check_integrity(&self) {
        if self.half_edges.num_elements() % 3 != 0 {
            panic!("bug: number of half edges not divisible by 3");
        }

        // Check `outgoing` handle of vertices
        for (vh, v) in self.vertices.iter() {
            if let Some(outgoing) = v.outgoing.into_option() {
                // Make sure outgoing handles are valid
                if !self.half_edges.contains_handle(*outgoing) {
                    panic!(
                        "bug (broken reference): [{:?}].outgoing = Some({:?}), but that \
                            half edge does not exist!",
                        vh,
                        outgoing,
                    );
                }

                // Check `outgoing <-> target` connection
                if let Some(incoming) = self[outgoing].twin.as_real_twin() {
                    if *self[incoming].target != vh {
                        panic!(
                            "bug: [{:?}].outgoing = Some({:?}), but [{:?}].twin = {:?} \
                                and [{:?}].target = {:?} (should be {:?})",
                            vh,
                            *outgoing,
                            *outgoing,
                            *incoming,
                            *incoming,
                            *self[incoming].target,
                            vh,
                        );
                    }
                }
            }
        }

        // Check half edges
        for fh in self.face_handles() {
            let [heh0, heh1, heh2] = self.checked_half_edges_around(fh);

            for &heh in &[heh0, heh1, heh2] {
                // Make sure all exist
                if !self.half_edges.contains_handle(*heh) {
                    panic!(
                        "bug: {:?} (returned by `checked_half_edges_around({:?})`) \
                            does not exist in `self.half_edges`",
                        heh,
                        fh,
                    );
                }

                let he = self[heh];

                // Make sure target vertex is not isolated
                if self[he.target].outgoing.is_none() {
                    panic!(
                        "bug: [{:?}].target = {:?}, but [{:?}].outgoing = None",
                        heh,
                        he.target,
                        he.target,
                    );
                }

                // Make sure `twin` handles match
                match he.twin.decode() {
                    Twin::Twin(twin) => {
                        if self[twin].twin != EncodedTwin::twin(heh) {
                            panic!(
                                "bug: [{:?}].twin = {:?}, but [{:?}].twin = {:?}",
                                heh,
                                he.twin,
                                twin,
                                self[twin].twin,
                            );
                        }
                    }
                    Twin::NextBoundaryHe(next) => {
                        if !self.half_edges.contains_handle(*next) {
                            panic!(
                                "bug: [{:?}].twin = {:?}, but {:?} does not exist",
                                heh,
                                he.twin,
                                next,
                            );
                        }
                    }
                }
            }

            // TODO: test prev/next
        }

        // Walk around boundaries.
        let mut visited = VecMap::with_capacity(self.half_edges.num_elements());
        for (start, he) in self.half_edges.iter() {
            if visited.contains_handle(start) {
                continue;
            }

            if he.is_boundary() {
                let mut heh = start;
                loop {
                    // All half edges in this cycles should be not visited yet!
                    if visited.insert(heh, ()).is_some() {
                        panic!(
                            "bug: encountered {:?} while iterating on boundary starting \
                                from {:?}, but we already visited it!",
                            heh,
                            start,
                        );
                    }

                    heh = match self.half_edges[heh].twin.decode() {
                        Twin::Twin(_) => {
                            panic!(
                                "bug: encountered {:?} while iterating on boundary starting \
                                    from {:?}, but [{:?}].twin = {:?} (should be pointing \
                                    to the next boundary edge)!",
                                heh,
                                start,
                                heh,
                                self.half_edges[heh].twin,
                            );
                        }
                        Twin::NextBoundaryHe(next) => *next,
                    };

                    if heh == start {
                        break;
                    }
                }
            }
        }
    }
}

impl<C: Config> MeshMut for DirectedEdgeMesh<C> {
    fn add_vertex(&mut self) -> VertexHandle {
        self.vertices.push(Vertex {
            outgoing: Opt::none()
        })
    }

    fn add_triangle(&mut self, [a, b, c]: [VertexHandle; 3]) -> FaceHandle {
        assert_ne!(a, b, "vertices of new face are not unique");
        assert_ne!(a, c, "vertices of new face are not unique");

        let vertices = [self.check_vertex(a), self.check_vertex(b), self.check_vertex(c)];
        let outer_hes = [
            self.he_between(vertices[1], vertices[0]),
            self.he_between(vertices[2], vertices[1]),
            self.he_between(vertices[0], vertices[2]),
        ];
        // dbg!(outer_hes);

        // Add three new half edges.
        //
        // The `dummy_to` is only safe if we override the `twin`, `next` and
        // `prev` handles and never use them to index the storage. Sadly, this
        // is not easy to see. All fields are certainly overwritten, but it's
        // more difficult to show that they are not accessed before. Well, the
        // correctness of this whole function is based on that.
        let inner_hes = self.push_half_edge_triple([
            unsafe { HalfEdge::dummy_to(vertices[1]) },
            unsafe { HalfEdge::dummy_to(vertices[2]) },
            unsafe { HalfEdge::dummy_to(vertices[0]) },
        ]);


        // Iterate over all corners of the new triangle
        for idx in 0..vertices.len() {
            let prev_idx = idx.checked_sub(1).unwrap_or(2);
            let vh = vertices[idx];
            let incoming_inner = inner_hes[prev_idx];
            let outgoing_inner = inner_hes[idx];
            let incoming_outer = outer_hes[idx];
            let outgoing_outer = outer_hes[prev_idx];

            // Set `next` and `prev` handles. We can do that here, they are not
            // read anywhere by this method.
            self[incoming_inner].next = new_next_handle::<C>(outgoing_inner);
            self[outgoing_inner].prev = new_prev_handle::<C>(incoming_inner);

            let v = &self[vh];

            match (incoming_outer, outgoing_outer) {
                // None of the half edges exists: both are boundary edges.
                (None, None) => {
                    if let Some(outgoing_from_v) = v.outgoing.into_option() {
                        // More difficult case: we are creating a multi
                        // fan-blade vertex here. In order to correctly set the
                        // encoded twin handles, we need to find the start of
                        // some blade and the end of some blade. We will insert
                        // the new blade between the two.
                        //
                        //
                        //
                        //           \  ?     ?  ^
                        //            \         /
                        //      start  \   ?   /  outgoing_from_v
                        //              \     /
                        //               v   /
                        //                (v)
                        //                / ^
                        //               /   \
                        //              /     \
                        //             /   F   \
                        //            v         \
                        //          ( )         ( )
                        //

                        // Find the end edge of some blade. This is easy
                        // because if a vertex is boundary, its `outgoing` edge
                        // is a boundary edge. And since it's *outgoing* it is
                        // the end of a blade. We need to make sure that it's a
                        // boundary vertex, though!
                        if let Some(start) = self[outgoing_from_v].twin.as_next_boundary_he() {
                            // Insert new blade in between.
                            self[outgoing_inner].twin
                                = EncodedTwin::next_boundary_he(start);
                            self[outgoing_from_v].twin
                                = EncodedTwin::next_boundary_he(incoming_inner);

                            // Regarding the `outgoing` field of `v`: before
                            // adding this face, it was a boundary half edge.
                            // Since we didn't add a face adjacent to it, it
                            // still is. So we can keep it unchanged.
                        } else {
                            // In this case, `outgoing_from_v` was not a
                            // boundary edge, meaning that the cycle around `v`
                            // is already closed. That's not allowed!
                            panic!(
                                "new triangle {:?} would create non-manifold vertex (cycle \
                                    around vertex {:?} already closed)",
                                [a, b, c],
                                v,
                            );
                        }
                    } else {
                        // This is the easy case: `incoming` and `outgoing` are
                        // the only edges adjacent to `v`. This also means that
                        // `v` was isolated before and we now need to set its
                        // `outgoing` handle.
                        //
                        //                (v)
                        //                / ^
                        //               /   \
                        //              /     \
                        //             /   F   \
                        //            v         \
                        //          ( )         ( )
                        self[outgoing_inner].twin
                            = EncodedTwin::next_boundary_he(incoming_inner);
                        self[vh].outgoing = Opt::some(outgoing_inner);
                    }
                }

                // The incoming half edge exists (adjacent to the face IF), but
                // the outgoing does not. We have to find the half edge
                // `before_new` whose `twin` handle points to `incoming_outer`.
                // Because that `twin` handle now needs to point to
                // `incoming_inner`. We also need to set the twin handles of
                // `incoming_outer` and `outgoing_inner`.
                //
                //                      ^
                //      ?         ?    /
                //           ?        /  before_new
                //                   /
                //                  /
                //      <-------- (v)
                //               ^/ ^
                //         IF   //   \
                //             //     \
                //            //   F   \
                //           /v         \
                //          ( )         ( )
                //
                //                 ^-- this face and
                //             ^--     ^-- these edges are new
                //
                (Some(incoming_outer), None) => {
                    // Find `before_new`
                    let before_new = self.circulate_around_vertex(vh).find(|&outgoing| {
                        self[outgoing].twin.as_next_boundary_he() == Some(incoming_outer)
                    }).expect(NON_MANIFOLD_EDGE_ERR);

                    // Update next boundary edge
                    self[before_new].twin = EncodedTwin::next_boundary_he(incoming_inner);

                    // Regarding the `outgoing` handle of the vertex: the only
                    // old boundary edge that is not boundary anymore is
                    // `incoming_outer`. But since this is an incoming edge, it
                    // could not have been the `outgoing` one of the vertex. So
                    // we don't need to change anything.
                }

                // The outgoing half edge exists (adjacent to the face OF), but
                // the incoming does not. The twin handle of `outgoing_outer`
                // points to some half edge `after_new`. `outgoing_inner.twin`
                // needs to point to that half edge now. Additional, the two
                // new real twin handles need to be set.
                //
                //            \
                //             \   ?
                //  after_new   \           ?
                //               \   ?
                //                v
                //                (v)<---------
                //                / ^\
                //               /   \\  OF
                //              /     \\
                //             /   F   \\
                //            v         \v
                //          ( )         ( )
                //
                //                 ^-- this face and
                //            ^--      ^-- these half edges are new
                //
                (None, Some(outgoing_outer)) => {
                    // Move the boundary-next to the new half edge.
                    self[outgoing_inner].twin = self[outgoing_outer].twin;

                    // We need to update the `outgoing` handle of the vertex
                    // here. It might have been `outgoing_outer` which is now
                    // not a boundary edge anymore.
                    self[vh].outgoing = Opt::some(outgoing_inner);
                }

                // This can be easy or the ugliest case. The outer incoming and
                // outgoing half edge both exist, meaning there are faces on
                // either side. That means we are connecting two fan blades. If
                // the fan blade of `incoming` is already directly after the
                // fan blade of `outgoing` (speaking about the "circulate
                // around vertex" order), then everything is fine.
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
                //
                // BUT, if that is not the case, we need to change the order of
                // fan blades to match the "good" situation described above.
                //
                // Additionally, we might need to update `v.outgoing` because
                // it might have been `incoming.twin()` which is not a boundary
                // half edge anymore (after this method).
                (Some(incoming_outer), Some(outgoing_outer)) => {
                    // Find the end of the blade containing IF. If there is
                    // only one blade, the end is `outgoing_outer`.
                    let ib_end = {
                        let start = self.next_he(incoming_outer);
                        let mut e = start;
                        while let Some(twin) = self[e].twin.as_real_twin() {
                            e = self.next_he(twin);
                            if e == start {
                                panic!("{}", NON_MANIFOLD_EDGE_ERR);
                            }
                        }

                        e
                    };

                    if self[outgoing_outer].twin.as_next_boundary_he() != Some(incoming_outer) {
                        // Here we need to conceptually delete one fan blade
                        // from the `next` circle around `v` and re-insert it
                        // into the right position. We choose to "move" the fan
                        // blade starting with `incoming_outer` (IB).
                        //
                        // We have to deal with four fan blades:
                        // - IB: the blade containing `incoming_outer` (being
                        //   its start).
                        // - OB: the blade containing `outgoing_outer` (being
                        //   its end)
                        // - BIB (before incoming blade): the blade before IB
                        // - AOB (after outgoing blade): the blade after OB
                        //   (`outgoing_outer.twin<as next>` is its start).
                        //
                        // Current situation:
                        //
                        //       ┌────┐    ┌─────┐         ┌─────┐    ┌────┐
                        //  +--> │ OB │ -> │ AOB │ -> ? -> │ BIB │ -> │ IB │ -> ?
                        //  |    └────┘    └─────┘         └─────┘    └────┘    |
                        //  +---------------------------------------------------+
                        //

                        // Find the end half edges of the blades BIB.
                        let bib_end = self.circulate_around_vertex(vh).find(|&outgoing| {
                            self[outgoing].twin.as_next_boundary_he() == Some(incoming_outer)
                        }).expect("internal DEM bug: couldn't find `bib_end`");

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
                        self[bib_end].twin = self[ib_end].twin;

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
                        self[ib_end].twin = self[outgoing_outer].twin;

                        // Right now, the cycle is still a bit broken, but that
                        // doesn't matter, because (a) the cycle will be
                        // repaired by combining IB and OB into one blade
                        // below, and (b) the broken cycle won't be accessed
                        // (in this direction) before it is repaired.

                        // To update `v.outgoing`, we luckily already know a
                        // boundary outgoing half edge of `v`: it's the end of
                        // BIB.
                        self[vh].outgoing = Opt::some(bib_end);
                    } else {
                        // We actually don't need to do a lot here if the fan
                        // blades are in the right order. The twin handles are
                        // set below this large loop, so we only need to update
                        // the `outgoing` handle of v, as it might have been
                        // invalidated.
                        self[vh].outgoing = Opt::some(ib_end);
                    }
                }
            }
        }

        // Now we set the twins of the outer half edges. We couldn't before,
        // because code needed to read the old values.
        for (&outer, &inner) in outer_hes.iter().zip(&inner_hes) {
            if let Some(outer) = outer {
                self[outer].twin = EncodedTwin::twin(inner);
                self[inner].twin = EncodedTwin::twin(outer);
            }
        }


        inner_hes[0].face()
    }

    #[inline(never)]
    fn reserve_for_vertices(&mut self, count: hsize) {
        self.vertices.reserve(count);
    }

    #[inline(never)]
    fn reserve_for_faces(&mut self, count: hsize) {
        // We have three half edges per face
        self.half_edges.reserve(count * 3);
    }

    fn remove_all_vertices(&mut self) {
        assert!(
            self.num_faces() == 0,
            "call to `remove_all_vertices`, but there are faces in the mesh!",
        );

        self.vertices.clear();
    }

    fn remove_all_faces(&mut self) {
        self.half_edges.clear();
        for v in self.vertices.values_mut() {
            v.outgoing = Opt::none();
        }
    }

    fn split_face(&mut self, f: FaceHandle) -> VertexHandle {
        // We need to add:
        // - 2 new faces (so 6 new half edges)
        // - 1 new vertex (the "midpoint")
        //
        // Let's visualize what are are about to do. On the left is the current
        // situation, on the right what it looks like after this method is
        // done.
        //
        //               (A)                |               (A)
        //              /   ^               |             / ^ | ^
        //             /     \              |            /  | |  \
        //            /       \             |           /   | |   \
        //           /         \            |          / Z  | v  Y \
        //          /           \           |         /     (M)     \
        //         /             \          |        /    ↗⟋   ↖⟍   \
        //        /       X       \         |       /   ⟋⟋      ⟍⟍  \
        //       /                 \        |      /  ⟋⟋    X     ⟍⟍ \
        //      v                   \       |     v ⟋↙              ⟍↘ \
        //    (B) ----------------> (C)     |    (B) ----------------> (C)
        //
        //

        // Obtain handles of outer half edges and vertices. Two of these half
        // edges will be repurposed: `a -> b` and `c -> a`. They are changed to
        // be `m -> b` and `c -> m` instead.
        let [he_ab_orig, he_bc, he_ca_orig] = self.checked_half_edges_around(f);
        let vb = self[he_ab_orig].target;
        let vc = self[he_bc].target;
        let va = self[he_ca_orig].target;

        // Find other half edges that point to the edges that will be
        // repurposed. We need to change their references later. But we need to
        // obtain these edges already know as we haven't changed anything about
        // the mesh yet.
        let has_ab_as_next = self.circulate_around_vertex(vb)
            .find(|&outgoing| self[outgoing].twin == EncodedTwin::next_boundary_he(he_ab_orig));
        let has_ca_as_next = self.circulate_around_vertex(va)
            .find(|&outgoing| self[outgoing].twin == EncodedTwin::next_boundary_he(he_ca_orig));
        let has_ab_as_twin = self[he_ab_orig].twin.as_real_twin();
        let has_ca_as_twin = self[he_ca_orig].twin.as_real_twin();

        // Add new vertex "in the middle". The `Checked::new` is correct as the
        // handle returned by `add_vertex` is obviously valid.
        let vm = unsafe { Checked::new(self.add_vertex()) };

        // Add new half edges for face Y. `unsafe` is fine as we will override
        // the twins and the handle returned by `push` is obviously valid.
        let [he_am, he_mc, he_ca] = self.push_half_edge_triple([
            unsafe { HalfEdge::dummy_to(vm) },
            unsafe { HalfEdge::dummy_to(vc) },
            self[he_ca_orig],
        ]);

        // Add new half edges for face Z. `unsafe` is fine as we will override
        // the twins and the handle returned by `push` is obviously valid`
        let [he_bm, he_ma, he_ab] = self.push_half_edge_triple([
            unsafe { HalfEdge::dummy_to(vm) },
            unsafe { HalfEdge::dummy_to(va) },
            self[he_ab_orig],
        ]);

        // Overwrite half edges of face X. `unsafe` is fine as we will override
        // the twins.
        self[he_ca_orig] = unsafe { HalfEdge::dummy_to(vm) };
        self[he_ab_orig] = unsafe { HalfEdge::dummy_to(vb) };
        let he_cm = he_ca_orig;
        let he_mb = he_ab_orig;

        // Set midpoint's `outgoing` to that edge. The midpoint is not a
        // boundary vertex, we don't have to pay attention to the `outgoing`
        // edge.
        self[vm].outgoing = Opt::some(he_ma);

        // Set twins of all inner half edges.
        self.set_twins(he_am, he_ma);
        self.set_twins(he_bm, he_mb);
        self.set_twins(he_cm, he_mc);

        // Fix links of edges that pointed to the repurposed edges.
        if let Some(he) = has_ab_as_next {
            self[he].twin = EncodedTwin::next_boundary_he(he_ab);
        }
        if let Some(he) = has_ca_as_next {
            // There is a special case here: the edge that referred to
            // `he_ca_orig` could be `he_ab_orig`! In that case we do not want
            // to modify the original edge, but the new one, that's actually
            // between `va` and `vb`.
            let he = if he == he_ab_orig { he_ab } else { he };
            self[he].twin = EncodedTwin::next_boundary_he(he_ca);
        }
        if let Some(he) = has_ab_as_twin {
            self[he].twin = EncodedTwin::twin(he_ab);
        }
        if let Some(he) = has_ca_as_twin {
            self[he].twin = EncodedTwin::twin(he_ca);
        }

        // For two of the outer vertices we have to check if they were
        // referring to one of the repurposed edges before. If so, we need to
        // update the `outgoing` handle.
        if self[va].outgoing == Opt::some(he_ab_orig) {
            self[va].outgoing = Opt::some(he_ab);
        }
        if self[vc].outgoing == Opt::some(he_ca_orig) {
            self[vc].outgoing = Opt::some(he_ca);
        }

        *vm
    }

    fn remove_face(&mut self, f: FaceHandle) {
        let [he0, he1, he2] = self.checked_half_edges_around(f);

        // We iterate over the three corners of `f` and look at each corner
        // individually. We have the half edges of `f` as shown here:
        //
        //                    ?
        //              ?           ?
        //                   (v)
        //                   / ^
        //         outgoing /   \ incoming
        //                 /  F  \
        //                v       \
        //              ( ) ----> ( )
        //              opposite_of_v
        //
        let corners = [[he0, he1, he2], [he1, he2, he0], [he2, he0, he1]];
        for &[incoming, outgoing, opposite_of_v] in &corners {
            let v = self[incoming].target;

            // If we have a face on the left, we always have to set a new
            // `twin` value for the twin of `outgoing`.
            //
            //           ?         ?
            //
            //      <-------- (v)
            //               ^/ ^
            //   some face  //   \      ?
            //             //     \
            //            //   F   \
            //           /v         \
            //          ( )         ( )
            //
            if let Twin::Twin(incoming_outer) = self[outgoing].twin.decode() {
                // We need to change the `twin` handle of `incoming_outer` as
                // `outgoing` will be removed. We have basically two
                // possibilities: `opposite_of_v` has a real twin (left) or not
                // (right).
                //
                //    x --- v            |          x --- v
                //     \   / \    ?      |           \   / \    ?
                //   ?  \ / f \          |         ?  \ / f \
                //       x --- x         |             x --- x
                //        \   /          |         ?  /
                //      ?  \ /    ?      |           /
                //          x            |          x
                //
                // In either way, `opposite_of_v.twin` is what
                // `incoming_outer.twin` needs to pointer to.
                self[incoming_outer].twin = self[opposite_of_v].twin.to_next_boundary_he();
            }

            // We handle the rest on a per case basis
            match (self[outgoing].twin.decode(), self[incoming].twin.decode()) {
                //
                //                (v)
                //                / ^
                //               /   \
                //              /  F  \
                //             v       \
                //           ( )       ( )
                //
                (Twin::NextBoundaryHe(next), Twin::NextBoundaryHe(_)) if next == incoming => {
                    self[v].outgoing = Opt::none();
                }

                //                      ^
                //      ?         ?    /            |         \  ?   ?  ^
                //           ?        /  end        |          \       /
                //                   /              |    start  \  ?  /  end
                //                  /               |            v   /
                //      <-------- (v)               |             (v)
                //               ^/ ^               |             / ^
                //   some face  //   \              |            /   \
                //             //     \             |           /  F  \
                //            //   F   \            |          v       \
                //           /v         \           |        ( )       ( )
                //          ( )         ( )
                //
                (_, Twin::NextBoundaryHe(_)) => {
                    // Obtain `end`, the half edge ending the blade before our
                    // blade.
                    let end = CwVertexCirculator::new(self, outgoing).find(|&outgoing| {
                        self[outgoing].twin.as_next_boundary_he() == Some(incoming)
                    }).expect("DEM bug: invalid cycle around vertex");

                    self[end].twin = self[outgoing].twin.to_next_boundary_he();

                    // We need to overwrite `v.outgoing` if it is currently
                    // pointing to `outgoing` (which can only happen if
                    // `outgoing` is a boundary edge). TODO: this can only
                    // happen if we allow multi blade vertices.
                    if self[v].outgoing == Opt::some(outgoing) {
                        self[v].outgoing = Opt::some(end);
                    }
                }

                //            ?         ?
                //
                //                (v)<---------
                //        ?       / ^\
                //               /   \\  some face
                //              /     \\
                //             /   F   \\
                //            v         \v
                //          ( )         ( )
                //
                (_, Twin::Twin(outgoing_outer)) => {
                    // `outgoing` could have a real twin or not. But in either
                    // case, that half edge stored in `outgoing.twin` will be
                    // the next boundary half edge of `outgoing_outer`.
                    self[outgoing_outer].twin = self[outgoing].twin.to_next_boundary_he();

                    // We just overwrite `v.outgoing` as it might have pointed
                    // to `outgoing`.
                    self[v].outgoing = Opt::some(outgoing_outer);
                }
            }
        }

        // Actually remove the half edges from the vector.
        self.half_edges.remove(*he0);
        self.half_edges.remove(*he1);
        self.half_edges.remove(*he2);
    }

    fn add_face(&mut self, _: &[VertexHandle]) -> FaceHandle
    where
        Self: PolyMesh
    {
        unreachable!()
    }

    fn flip_edge(&mut self, _: EdgeHandle)
    where
        Self: EdgeMesh + TriMesh
    {
        unreachable!()
    }

    fn split_edge_with_faces(&mut self, _: EdgeHandle) -> SplitEdgeWithFacesResult
    where
        Self: EdgeMesh + TriMesh
    {
        unreachable!()
    }
}

impl<C: Config> SupportsMultiBlade for DirectedEdgeMesh<C> {}

// TODO: Think about `EdgeMesh`.
//
// Exposing edges is pretty trecky for this data structure. First idea: use the
// lower half edge handle as edge handle. This works fine for the most part,
// but can go boom when the mesh is mutated:
// - `delete_face`: the lower half edge might get deleted, while the upper
//   remains (the full edge also logically remains in the mesh).
// - `flip_edge`, ...: if edge pairs are changed, that's gonna be problematic.
//
// Several ideas to work around the problems:
// - Be fairly restrictive about the validity of edge handles (i.e. if a face
//   around an edge changes, the edge handle might be invalidated). Not sure if
//   that's a viable solution. Maybe people need those handles to remain valid?
// - Assuming a `stable-vec` implementation that retains removed elements, one
//   could store some data inside deleted elements. For half edges, we could
//   store the twin, even if that half edge is deleted. That way, an edge
//   handle pointing to a removed half edge could still access the other half.
//
// Also, a notable disadvantage is that edge handles are not really consecutive
// this way. One can assume that the two halves of an edge are approximately
// created at the same time. Meaning that highest edge handle has an index
// approximately twice the number of edges. This makes storing edge attributes
// in arrays rather inefficient and wasteful.
//
// One completely different approach is to store additional data. For example,
// a simple edge array which points to its two half edges. This would be fairly
// memory inefficient though. Maybe there is some smart way to encode this?
// This could of course be configured via `Config`.
