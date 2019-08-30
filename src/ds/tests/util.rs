use std::{
    collections::{BTreeMap, BTreeSet},
    cmp::PartialEq,
    fmt::Debug,
};

use crate::{
    prelude::*,
    handle::hsize,
};


// ===============================================================================================
// ===== Small helper functions and macros
// ===============================================================================================

/// Takes two iterators, collects both into sets and compares those sets for
/// equality.
#[allow(unused_macros)]
macro_rules! assert_eq_set {
    ($left:expr, $right:expr $(,)?) => {
        crate::ds::tests::util::assert_eq_set_fn(
            $left,
            $right,
            stringify!($left),
            stringify!($right),
        );
    }
}

/// Internal helper function for `assert_eq_set`.
#[allow(dead_code)]
pub fn assert_eq_set_fn<T: Debug + Clone + Eq + Ord>(
    left: impl IntoIterator<Item = T>,
    right: impl IntoIterator<Item = T>,
    left_str: &str,
    right_str: &str,
)
{
    let left = set(left);
    let right = set(right);
    if left != right {
        panic!(
            "assert_eq_set!({}, {}) failed:\n|  left: {:?}\n| right: {:?} ",
            left_str,
            right_str,
            left,
            right,
        );
    }
}

/// Helper function to create a set from the given iterator.
fn set<T>(src: impl IntoIterator<Item = T>) -> BTreeSet<T>
where
    T: Clone + Eq + Ord,
{
    src.into_iter().collect()
}

/// Compares `actual` and `expected`. This function checks if both slices are
/// equal when treating them like a "ring". This means that if we can rotate
/// one slice so that it equals the other slice, we consider them equal.
/// `[a, b, c, d]` and `[b, c, d, a]` and `[d, a, b, c]` are all equal.
///
/// If the slices are equal, `Ok(())` is returned. Otherwise, `Err(rotated)` is
/// returned, where `rotated` is `expected` but potentially rotated by some
/// amount. This can be used to print in the error message as the returned
/// vector looks more similar to the `actual` value.
pub(crate) fn cmp_rotated<T: Debug + PartialEq + Clone>(
    actual: &[T],
    expected: &[T],
) -> Result<(), Vec<T>> {
    let mut rotated = expected.to_vec();

    if actual.len() != expected.len() {
        return Err(rotated);
    }

    if actual.len() > 0 {
        // Find the rotate-offset
        let pos = match actual.iter().position(|e| e == &expected[0]) {
            Some(pos) => pos,
            None => return Err(rotated),
        };

        // Align my rotating back
        rotated.rotate_right(pos);

        if actual != &rotated[..] {
            return Err(rotated);
        }
    }

    Ok(())
}


// ===============================================================================================
// ===== Data types to define a mesh check
// ===============================================================================================

/// Holds everything that should be checked about the mesh.
#[derive(Debug)]
pub(crate) struct MeshCheck {
    pub(crate) vertices: ElementCheck<ElementInfo<VertexHandle>>,
    pub(crate) faces: ElementCheck<ElementInfo<FaceHandle>>,
    pub(crate) edges: ElementCheck<EdgeInfo>,
}

/// Specifies how elements of a mesh (face, vertex, edge) should be checked.
#[derive(Debug)]
#[allow(dead_code)]
pub(crate) enum ElementCheck<E> {
    /// The elements are given as a set, i.e. order will not be checked.
    Full(Vec<E>),

    /// Not all elements were specified, but these given ones have to be part
    /// of the mesh. Additionally, the number of elements might be specified
    /// and will be checked.
    Partial {
        elements: Vec<E>,
        len: Option<usize>,
    },

    /// The elements should not be checked.
    NoCheck,
}

/// Contains information about a specific element (face, vertex).
#[derive(Debug)]
pub(crate) struct ElementInfo<H: Handle> {
    pub(crate) handle: H,
    pub(crate) boundary: Option<bool>,
    pub(crate) adjacent_faces: NeighborCheck<FaceHandle>,
    pub(crate) adjacent_vertices: NeighborCheck<VertexHandle>,
}

/// Contains information about a specific edge.
#[derive(Debug)]
pub(crate) struct EdgeInfo {
    pub(crate) vertices: [VertexHandle; 2],
    pub(crate) handle: Option<EdgeHandle>,
    pub(crate) boundary: Option<bool>,
    pub(crate) adjacent_faces: NeighborCheck<FaceHandle>,
}

/// Specifies how neighbors of a specific element should be checked.
#[derive(Debug)]
pub(crate) enum NeighborCheck<H: Handle> {
    /// Neighbors are given in a fixed order. The vector can still be rotated
    /// (as the first element is not fixed).
    OrderDefined(Vec<H>),

    /// The neighbors are given as a set, i.e. order will not be checked.
    OrderUndefined(Vec<H>),

    /// Not all neighbors were specified, but these given ones have to be a
    /// neighbor. Additionally, the number of neighbors might be specified and
    /// will be checked.
    Partial {
        partial_neighbors: Vec<H>,
        len: Option<usize>,
    },

    /// The neighbors should not be checked.
    NoCheck,
}

impl<E> ElementCheck<E> {
    /// If the number of elements is specified, return it, `None` otherwise.
    fn len(&self) -> Option<usize> {
        match self {
            ElementCheck::Full(v) => Some(v.len()),
            ElementCheck::Partial { len, .. } => *len,
            ElementCheck::NoCheck => None,
        }
    }

    /// Returns all elements stored in this check. The returned value might not
    /// be complete. This is only useful for "checking something for each
    /// element" and not comparing the returned slice as a whole to something.
    fn elements(&self) -> &[E] {
        match self {
            ElementCheck::Full(v) => v,
            ElementCheck::Partial { elements, .. } => elements,
            ElementCheck::NoCheck => &[],
        }
    }
}


// ===============================================================================================
// ===== The actual mesh checks
// ===============================================================================================

/// Helper trait to abstract over faces and vertices.
trait ElementHandle: Handle {
    /// "face" or "vertex"
    const SINGULAR: &'static str;
    /// "faces" or "vertices"
    const PLURAL: &'static str;

    /// Calls `visit` for each face/vertex handle in the given `mesh`.
    fn for_all<M: Mesh>(mesh: &M, visit: impl FnMut(Self));

    /// "are_faces_adjacent" or "are_vertices_adjacent"
    const ARE_ADJACENT_FN: &'static str;
    /// Checks if two elements are adjacent.
    fn are_adjacent<M: FullAdj>(mesh: &M, a: Self, b: Self) -> bool;

    /// "is_boundary_face" or "is_boundary_vertex"
    const IS_BOUNDARY_FN: &'static str;
    /// Checks if the given element is a boundary face/vertex
    fn is_boundary<M: FullAdj>(mesh: &M, h: Self) -> bool;

    /// Returns the `adjacent_faces` or `adjacent_vertices` field.
    fn neighbors<H: Handle>(info: &ElementInfo<H>) -> &NeighborCheck<Self>;
}

impl ElementHandle for VertexHandle {
    const SINGULAR: &'static str = "vertex";
    const PLURAL: &'static str = "vertices";

    fn for_all<M: Mesh>(mesh: &M, visit: impl FnMut(Self)) {
        mesh.vertex_handles().for_each(visit)
    }

    const ARE_ADJACENT_FN: &'static str = "are_vertices_adjacent";
    fn are_adjacent<M: FullAdj>(mesh: &M, a: Self, b: Self) -> bool {
        mesh.are_vertices_adjacent(a, b)
    }

    const IS_BOUNDARY_FN: &'static str = "is_boundary_vertex";
    fn is_boundary<M: FullAdj>(mesh: &M, h: Self) -> bool {
        mesh.is_boundary_vertex(h)
    }

    fn neighbors<H: Handle>(info: &ElementInfo<H>) -> &NeighborCheck<Self> {
        &info.adjacent_vertices
    }
}

impl ElementHandle for FaceHandle {
    const SINGULAR: &'static str = "face";
    const PLURAL: &'static str = "faces";

    fn for_all<M: Mesh>(mesh: &M, visit: impl FnMut(Self)) {
        mesh.face_handles().for_each(visit)
    }

    const ARE_ADJACENT_FN: &'static str = "are_faces_adjacent";
    fn are_adjacent<M: FullAdj>(mesh: &M, a: Self, b: Self) -> bool {
        mesh.are_faces_adjacent(a, b)
    }

    const IS_BOUNDARY_FN: &'static str = "is_boundary_face";
    fn is_boundary<M: FullAdj>(mesh: &M, h: Self) -> bool {
        mesh.is_boundary_face(h)
    }

    fn neighbors<H: Handle>(info: &ElementInfo<H>) -> &NeighborCheck<Self> {
        &info.adjacent_faces
    }
}


impl<H: Handle> ElementInfo<H> {
    /// Performs a number of checks for the given element.
    fn check_full_adj<M: FullAdj>(&self, mesh: &M)
    where
        H: ElementHandle,
    {
        // Check `is_boundary_*` method
        if let Some(expected_boundary) = self.boundary {
            if expected_boundary != H::is_boundary(mesh, self.handle) {
                panic!(
                    "mesh says {:?} is {}a boundary {}, but it is{}",
                    self.handle,
                    if expected_boundary { "not " } else { "" },
                    H::SINGULAR,
                    if expected_boundary { "" } else { " not" },
                );
            }
        }

        // Check `are_*_adjacent` method
        match H::neighbors(&self) {
            NeighborCheck::OrderDefined(neighbors) | NeighborCheck::OrderUndefined(neighbors) => {
                H::for_all(mesh, |other| {
                    let actual_adjacent = H::are_adjacent(mesh, self.handle, other);
                    let expected_adjacent = neighbors.contains(&other);
                    if actual_adjacent != expected_adjacent {
                        panic!(
                            "{}({:?}, {:?}) returned {}, but those {} \
                                are expected to{} be adjacent",
                            H::ARE_ADJACENT_FN,
                            self.handle,
                            other,
                            actual_adjacent,
                            H::PLURAL,
                            if expected_adjacent { "" } else { " not" },
                        );
                    }
                })

            }
            _ =>  {},
        }
    }
}

impl<H: Handle> NeighborCheck<H> {
    /// Returns a list of neighbors that is potentially incomplete. This is
    /// useful whenever you want to do something for each neighbor, but not if
    /// you want to compare the full set of neighbors.
    fn potentially_partial(&self) -> &[H] {
        match self {
            NeighborCheck::NoCheck => &[],
            NeighborCheck::Partial { partial_neighbors, .. }=> &partial_neighbors,
            NeighborCheck::OrderDefined(neighbors) => &neighbors,
            NeighborCheck::OrderUndefined(neighbors) => &neighbors,
        }
    }

    /// Checks whether the `actual` neighbors equal the neighbors specified in
    /// `self`. The `actual_str` is for a better error output: it is the code
    /// snippet that resulted in `actual`.
    fn check(&self, actual: &[H], actual_str: &str) {
        match self {
            NeighborCheck::NoCheck => {}
            NeighborCheck::OrderDefined(expected) => {
                if let Err(rotated) = cmp_rotated(actual, expected) {
                    panic!(
                        "wrong neighbors returned by `{}` (order respecting comparison)\n\
                            | expected: {:?} (original: {:?})\n\
                            |   actual: {:?}\n",
                        actual_str,
                        rotated,
                        expected,
                        actual,
                    );
                }
            }
            NeighborCheck::OrderUndefined(expected) => {
                if set(expected) != set(actual) {
                    panic!(
                        "wrong neighbors returned by `{}` (set comparison)\n\
                            | expected: {:?}\n\
                            |   actual: {:?}\n",
                        actual_str,
                        set(expected),
                        set(actual),
                    );
                }
            }
            NeighborCheck::Partial { partial_neighbors, len } => {
                for expected in partial_neighbors {
                    if !actual.contains(&expected) {
                        panic!(
                            "the result of `{}` does not contain {:?}, but that was expected! \n\
                                | expected in result: {:?}\n\
                                |  actually returned: {:?}\n",
                            actual_str,
                            expected,
                            partial_neighbors,
                            actual,
                        );
                    }
                }

                if let Some(len) = len {
                    if actual.len() != *len {
                        panic!(
                            "the result of`{}` was expected to have the length {}, but \
                                it actually has the length {} ({:?})",
                            actual_str,
                            len,
                            actual.len(),
                            actual,
                        );
                    }
                }
            }
        }
    }
}

impl MeshCheck {
    /// Checks basic properties, without any adjacency info.
    pub(crate) fn check_basic<M: Mesh>(&self, mesh: &M) {
        macro_rules! gen_check {
            (
                $field:ident, $num_fn:ident, $handles_fn:ident, $contains_fn:ident,
                $next_from_fn:ident, $last_fn:ident,
            ) => {
                // Check length
                if let Some(len) = self.$field.len() {
                    if mesh.$num_fn() != len as hsize {
                        panic!(
                            "`{}()` returned {}, but is supposed to return {}",
                            stringify!($num_fn),
                            mesh.$num_fn(),
                            len,
                        );
                    }
                }

                // Check if expected vertices/faces equal the actual ones
                let actual_handles = set(mesh.$handles_fn());
                match &self.$field {
                    ElementCheck::Full(expected) => {
                        let expected = set(expected.iter().map(|v| v.handle));

                        if actual_handles != expected {
                            panic!(
                                "unexpected {} in mesh (check for set equality).\n\
                                    | expected: {:?}\n\
                                    |   actual: {:?} (via `mesh.{}()`)\n",
                                stringify!($field),
                                expected,
                                actual_handles,
                                stringify!($handles_fn),
                            );
                        }

                        let max_idx = expected.iter().map(|h| h.idx()).max().unwrap_or(0);
                        for idx in max_idx + 1..max_idx + 5 {
                            let handle = Handle::new(idx);
                            if mesh.$contains_fn(handle) {
                                panic!(
                                    "mesh.{}({:?}) returned true, but should return false",
                                    stringify!($contains_fn),
                                    handle,
                                );
                            }
                        }
                    }
                    ElementCheck::Partial { elements, .. } => {
                        for e in elements {
                            if !actual_handles.contains(&e.handle) {
                                panic!(
                                    "{:?} not part of mesh, but should be.\n\
                                        | expected: {:?} (partial!)\n\
                                        |   actual: {:?} (via `mesh.{}()`)\n",
                                    e.handle,
                                    set(elements.iter().map(|e| e.handle)),
                                    actual_handles,
                                    stringify!($handles_fn),
                                );
                            }
                        }
                    },
                    ElementCheck::NoCheck => {},
                }

                // Check `contains_*`, `next_*_handle_from` and `last_*_handle`.
                for e in self.$field.elements() {
                    if !mesh.$contains_fn(e.handle) {
                        panic!(
                            "mesh.{}({:?}) returned false, should return true",
                            stringify!($contains_fn),
                            e.handle,
                        );
                    }

                    let next = mesh.$next_from_fn(e.handle);
                    if next != Some(e.handle) {
                        panic!(
                            "mesh.{}({:?}) returned {:?}, should return {:?}",
                            stringify!($next_from_fn),
                            e.handle,
                            next,
                            e.handle,
                        );
                    }

                    let last_handle = mesh.$last_fn()
                        .expect(concat!("`", stringify!($last_fn), "` returned `None`"));
                    if e.handle.idx() > last_handle.idx() {
                        panic!(
                            "mesh.{}() returned {:?} which has a smaller index than {:?} \
                                (which is part of the mesh)",
                            stringify!($last_fn),
                            last_handle,
                            e.handle,
                        );
                    }
                }
            };
        }

        mesh.check_integrity();
        gen_check!(
            vertices, num_vertices, vertex_handles, contains_vertex,
            next_vertex_handle_from, last_vertex_handle,
        );
        gen_check!(
            faces, num_faces, face_handles, contains_face,
            next_face_handle_from, last_face_handle,
        );
    }

    /// Checks basic properties of edges, without adjacency info.
    pub(crate) fn check_basic_edge<M: EdgeMesh>(&self, mesh: &M) {
        // Check length
        if let Some(len) = self.edges.len() {
            if mesh.num_edges() != len as hsize {
                panic!(
                    "`num_edges()` returned {}, but is supposed to return {}",
                    mesh.num_edges(),
                    len,
                );
            }
        }

        enum SetCheck {
            Equality(BTreeSet<EdgeHandle>),
            Subset(BTreeSet<EdgeHandle>),
            None,
        }

        // Check if expected edge handles equal the actual ones. We can only
        // check for set equality if all edge handles were actually specified.
        let actual_handles = set(mesh.edge_handles());
        let expected_handles = match &self.edges {
            ElementCheck::Full(expected) => {
                let expected_handles = set(expected.iter().filter_map(|e| e.handle));
                if expected_handles.len() == expected.len() {
                    SetCheck::Equality(expected_handles)
                } else {
                    SetCheck::Subset(expected_handles)
                }
            }
            ElementCheck::Partial { elements, .. } => {
                SetCheck::Subset(set(elements.iter().filter_map(|e| e.handle)))
            }
            ElementCheck::NoCheck => SetCheck::None,
        };

        match expected_handles {
            SetCheck::Equality(expected) => {
                if actual_handles != expected {
                    panic!(
                        "unexpected edges in mesh (check for set equality).\n\
                            | expected: {:?}\n\
                            |   actual: {:?} (via `mesh.edge_handles()`)\n",
                        expected,
                        actual_handles,
                    );
                }

                let max_idx = expected.iter().map(|h| h.idx()).max().unwrap_or(0);
                for idx in max_idx + 1..max_idx + 5 {
                    let handle = Handle::new(idx);
                    if mesh.contains_edge(handle) {
                        panic!(
                            "mesh.contains_edge({:?}) returned true, but should return false",
                            handle,
                        );
                    }
                }
            }
            SetCheck::Subset(expected) => {
                for eh in &expected {
                    if !actual_handles.contains(eh) {
                        panic!(
                            "{:?} not part of mesh, but should be.\n\
                                | expected: {:?} (partial!)\n\
                                |   actual: {:?} (via `mesh.edge_handles()`)\n",
                            eh,
                            expected,
                            actual_handles,
                        );
                    }
                }
            },
            SetCheck::None => {},
        }

        // Check `contains_*`, `next_*_handle_from` and `last_*_handle`.
        for eh in self.edges.elements().iter().filter_map(|e| e.handle) {
            if !mesh.contains_edge(eh) {
                panic!(
                    "mesh.contains_edge({:?}) returned false, should return true",
                    eh,
                );
            }

            let next = mesh.next_edge_handle_from(eh);
            if next != Some(eh) {
                panic!(
                    "mesh.next_edge_handle_from({:?}) returned {:?}, should return {:?}",
                    eh,
                    next,
                    eh,
                );
            }

            let last_handle = mesh.last_edge_handle()
                .expect(concat!("`last_edge_handle` returned `None`"));
            if eh.idx() > last_handle.idx() {
                panic!(
                    "mesh.last_edge_handle() returned {:?} which has a smaller index than {:?} \
                        (which is part of the mesh)",
                    last_handle,
                    eh,
                );
            }
        }
    }

    /// Checks properties of `BasicAdj` trait.
    pub(crate) fn check_basic_adj<M: BasicAdj>(&self, mesh: &M) {
        for f in self.faces.elements() {
            f.adjacent_vertices.check(
                &mesh.vertices_around_face(f.handle).into_vec(),
                &format!("mesh.vertices_around_face({:?})", f.handle),
            );
        }
    }

    /// Checks properties of `BasicAdj` trait for triangle meshes.
    pub(crate) fn check_basic_adj_tri<M: BasicAdj + TriMesh>(&self, mesh: &M) {
        for f in self.faces.elements() {
            f.adjacent_vertices.check(
                &mesh.vertices_around_triangle(f.handle),
                &format!("mesh.vertices_around_triangle({:?})", f.handle),
            );
        }
    }

    /// Checks properties of `FulLAdj` trait.
    pub(crate) fn check_full_adj<M: FullAdj>(&self, mesh: &M) {
        // ===== FACES ======
        for f in self.faces.elements() {
            f.check_full_adj(mesh);

            f.adjacent_faces.check(
                &mesh.faces_around_face(f.handle).into_vec(),
                &format!("mesh.faces_around_face({:?})", f.handle),
            );
        }

        // ===== VERTICES =====
        for v in self.vertices.elements() {
            v.check_full_adj(mesh);

            v.adjacent_faces.check(
                &mesh.faces_around_vertex(v.handle).into_vec(),
                &format!("mesh.faces_around_vertex({:?})", v.handle),
            );
            v.adjacent_vertices.check(
                &mesh.vertices_around_vertex(v.handle).into_vec(),
                &format!("mesh.vertices_around_vertex({:?})", v.handle),
            );

            match &v.adjacent_vertices {
                NeighborCheck::OrderDefined(n) | NeighborCheck::OrderUndefined(n) => {
                    if mesh.is_isolated_vertex(v.handle) != n.is_empty() {
                        panic!(
                            "mesh says {:?} is {}an isolated vertex, but it is{}",
                            v.handle,
                            if n.is_empty() { "not " }  else { "" },
                            if n.is_empty() { "" }  else { " not" },
                        );
                    }
                }
                _ => {}
            }
        }
    }

    /// Checks properties of `FulLAdj` trait for triangle meshes.
    pub(crate) fn check_full_adj_tri<M: FullAdj + TriMesh>(&self, mesh: &M) {
        for f in self.faces.elements() {
            f.adjacent_faces.check(
                &mesh.faces_around_triangle(f.handle).into_vec(),
                &format!("mesh.faces_around_triangle({:?})", f.handle),
            );
        }
    }

    /// Checks several properties about `EdgeAdj` methods.
    pub(crate) fn check_edge_adj<M: EdgeAdj>(&self, mesh: &M) {
        for e in self.edges.elements() {
            let handle = match mesh.edge_between_vertices(e.vertices[0], e.vertices[1]) {
                Some(h) => h,
                None => {
                    panic!(
                        "`mesh.edge_between_vertices({:?}, {:?})` returned `None` but {} \
                            between those vertices was expected",
                        e.vertices[0],
                        e.vertices[1],
                        e.handle.map(|h| format!("{:?}", h)).unwrap_or("an edge".into()),
                    );
                }
            };

            // Check if the given handle equals the handle of the edge between
            // the vertices.
            if let Some(expected_handle) = e.handle {
                if expected_handle != handle {
                    panic!(
                        "`mesh.edge_between_vertices({:?}, {:?})` returned unexpected edge.\n\
                            | expected: {:?}\n\
                            |   actual: {:?}\n",
                        e.vertices[0],
                        e.vertices[1],
                        expected_handle,
                        handle,
                    );
                }
            }

            let edge_id = format!("{:?} ({:?} -- {:?})", handle, e.vertices[0], e.vertices[1]);

            // Check if edge is boundary
            // Check `is_boundary_*` method
            if let Some(expected_boundary) = e.boundary {
                if expected_boundary != mesh.is_boundary_edge(handle) {
                    panic!(
                        "mesh says {} is {}a boundary edge, but the opposite was expected",
                        edge_id,
                        if expected_boundary { "not " } else { "" },
                    );
                }
            }

            // Check `E -> F` adjacency
            e.adjacent_faces.check(
                &mesh.faces_of_edge(handle).into_vec(),
                &format!("mesh.faces_of_edge({})", edge_id),
            );

            // Check `E -> V` adjacency
            let expected_vertices = set(e.vertices.iter().cloned());
            let actual_vertices = set(mesh.endpoints_of_edge(handle).iter().cloned());
            if actual_vertices != expected_vertices {
                panic!(
                    "wrong neighbors returned by `endpoints_of_edge({})` (set comparison)\n\
                        | expected: {:?}\n\
                        |   actual: {:?}\n",
                    edge_id,
                    expected_vertices,
                    actual_vertices,
                );
            }
        }

        // Checking `F -> E` adjacency by obtaining edges from the order
        // adjacent vertices.
        for f in self.faces.elements() {
            if let NeighborCheck::OrderDefined(vertices) = &f.adjacent_vertices {
                let actual_edges = mesh.edges_around_face(f.handle).collect::<Vec<_>>();
                let expected_edges = (0..vertices.len()).map(|i| {
                    let va = vertices[i];
                    let vb = vertices[(i + 1) % vertices.len()];

                    mesh.edge_between_vertices(va, vb).unwrap_or_else(|| {
                        panic!(
                            "`mesh.edge_between_vertices({:?}, {:?})` returned `None`, but an \
                                edge was expected, as both vertices are part of {:?}",
                            va,
                            vb,
                            f.handle,
                        );
                    })
                }).collect::<Vec<_>>();

                if let Err(rotated) = cmp_rotated(&actual_edges, &expected_edges) {
                    panic!(
                        "wrong edges returned by `edges_around_face({:?})` \
                                (order respecting comparison)\n\
                            | expected: {:?} (original: {:?}, obtained from ordered \
                                vertices around face)\n\
                            |   actual: {:?}\n",
                        f.handle,
                        rotated,
                        expected_edges,
                        actual_edges,
                    );
                }
            }
        }

        // For each vertex, we compare the edges returned by
        // `edges_around_vertex` with the edges obtained by using
        // `edge_between_vertices` on all adjacent vertices. Making sure that
        // both results are compatible.
        for center in self.vertices.elements() {
            // Obtain a list of edges that are connected to `center` and the
            // vertices adjacent to `center`.
            let edges_to_vertices = center.adjacent_vertices.potentially_partial()
                .iter()
                .map(|other| {
                    mesh.edge_between_vertices(center.handle, *other).unwrap_or_else(|| {
                        panic!(
                            "`mesh.edge_between_vertices({:?}, {:?})` returned `None`, but \
                                an edge was expected, as both vertices are adjacent",
                            center.handle,
                            other,
                        );
                    })
                })
                .collect::<Vec<_>>();

            // Obtain a list of edges the normal way: via adjacency method.
            let adjacent_edges = mesh.edges_around_vertex(center.handle).collect::<Vec<_>>();

            // Compare the two lists.
            match &center.adjacent_vertices {
                NeighborCheck::OrderDefined(_) => {
                    if let Err(rotated) = cmp_rotated(&edges_to_vertices, &adjacent_edges) {
                        panic!(
                            "edges returned by `edges_around_vertex({:?})` differ from \
                                edges returned from `edge_between_vertices` called with \
                                {:?} and its neighbor vertices (order respecting comparison)\n\
                                |   from `edges_around_vertex`: {:?} (original: {:?})\n\
                                | from `edge_between_vertices`: {:?}\n",
                            center.handle,
                            center.handle,
                            rotated,
                            adjacent_edges,
                            edges_to_vertices,
                        );
                    }
                }
                NeighborCheck::OrderUndefined(_) => {
                    if set(&edges_to_vertices) != set(&adjacent_edges) {
                        panic!(
                            "edges returned by `edges_around_vertex({:?})` differ from \
                                edges returned from `edge_between_vertices` called with \
                                {:?} and its neighbor vertices (set comparison)\n\
                                |   from `edges_around_vertex`: {:?}\n\
                                | from `edge_between_vertices`: {:?}\n",
                            center.handle,
                            center.handle,
                            set(&adjacent_edges),
                            set(&edges_to_vertices),
                        );
                    }
                }
                NeighborCheck::Partial { .. } => {
                    for (i, eh) in edges_to_vertices.iter().enumerate() {
                        if !adjacent_edges.contains(&eh) {
                            panic!(
                                "{:?} is not in the result of edges_around_vertex({:?}), but it\
                                    should be, as it's the edge between {:?} and {:?}.\n\
                                    | actually returned by edges_around_vertex: {:?}\n",
                                eh,
                                center.handle,
                                center.handle,
                                center.adjacent_vertices.potentially_partial()[i],
                                adjacent_edges,
                            );
                        }
                    }
                }
                NeighborCheck::NoCheck => {}
            }
        }
    }
}


// ===============================================================================================
// ===== Symbol table
// ===============================================================================================

#[derive(Debug, Empty)]
pub(crate) struct Symbols {
    vertices: BTreeMap<VertexHandle, &'static str>,
    faces: BTreeMap<FaceHandle, &'static str>,
    edges: BTreeMap<EdgeHandle, &'static str>,
}

pub(crate) trait SymbolHelper<H: Handle> {
    fn add(&mut self, h: H, ident: &'static str);
}

impl SymbolHelper<VertexHandle> for Symbols {
    fn add(&mut self, h: VertexHandle, ident: &'static str) {
        self.vertices.insert(h, ident);
    }
}
impl SymbolHelper<FaceHandle> for Symbols {
    fn add(&mut self, h: FaceHandle, ident: &'static str) {
        self.faces.insert(h, ident);
    }
}
impl SymbolHelper<EdgeHandle> for Symbols {
    fn add(&mut self, h: EdgeHandle, ident: &'static str) {
        self.edges.insert(h, ident);
    }
}


// ===============================================================================================
// ===== The public macros
// ===============================================================================================

/// Custom syntax to completely check a mesh.
///
/// This macro is fairly elaborate. The basic structure:
///
/// ```
/// check_mesh!(m; [BasicAdj, FullAdj]; {
///     vertices: {},
///     faces: {},
///     edges: {},
/// });
/// ```
///
/// `m` is an identifier referring to the mesh variable. The list of
/// identifiers in `[]` are used to conditionally add more checks. You should
/// add all traits your mesh implements. TODO: explain this more
///
/// Each `{}` for the three element types is usually replace with one of the
/// following syntax. Here, `<elem_list>` is a placeholder for a (potentially
/// empty) semicolon-separated, list of element specifications, as explained
/// below.
///
/// - `{<elem_list>}`: specifies a set of elements. The mesh is checked to
///   contain exactly these elements.
/// - `{<elem_list> ...}`: also a set of elements, but this set can be
///   incomplete. It is only checked that all elements specified here are in
///   fact part of the mesh, but the mesh is allowed to have additional
///   elements.
/// - `{<elem_list> ...; <len>}`: like the previous, but with a known length.
///   So in addition to the previous, the number of elements in the mesh are
///   compared to the given length.
///
/// For faces and vertices, an element specification has the following syntax:
///
/// ```
/// handle => <neighbor_faces>, <neighbor_vertices>, <is_boundary>;
/// ```
///
/// The `handle` is an identifier for the handle of that element.
/// `<is_boundary>` is either `boundary` or `interior`, which will be checked.
///
/// The `<neighbor_*>` placeholder hold a neighbor check specification. This
/// specification can have one of the following syntax. Here `<ns>` is a
/// placeholder for a (potentially empty) comma-separated list of identifiers
/// which are the variables holding handles of the neighbors.
///
/// - `[<ns>]`: an ordered list of neighbors. It is compared to the actual
///   neighbors, also checking order. See `cmp_rotated` for more information.
/// - `{<ns>}`: a set of neighbors. The actual neighbors are checked to be
///   exactly equal to this set.
/// - `{<ns> ...}`: as above, this is an potentially incomplete set (so a
///   relaxed check compared with the previous one).
/// - `{<ns> ...; <len>}`: as the previous syntax, but also checks the given
///   number of neighbors.
///
/// With that, here is a full example:
///
/// ```
/// let mut m = HalfEdgeMesh::empty();
/// let va = m.add_vertex();
/// let vb = m.add_vertex();
/// let vc = m.add_vertex();
/// let f = m.add_triangle([va, vb, vc]);
///
/// check_mesh!(m; $extras; {
///     vertices: {
///         va => [f], [vc, vb], boundary;
///         vb => [f], [va, vc], boundary;
///         vc => [f], [vb, va], boundary;
///     },
///     faces: {
///         f => [], [va, vb, vc], boundary;
///     },
///     edges: no_check,
/// });
/// ```
macro_rules! check_mesh {
    ($mesh:ident; $extras:tt; {
        vertices: $vertex_checks:tt,
        faces: $face_checks:tt,
        edges: $edge_checks:tt $(,)?
    }) => {{
        #[allow(unused_imports)] // Because of conditional code
        use crate::ds::tests::util::{
            MeshCheck, EdgeInfo, ElementCheck, ElementInfo, NeighborCheck, Symbols, SymbolHelper,
        };

        // Build check description
        #[allow(unused_mut)]
        let mut symbols = Symbols::empty();
        let checker = MeshCheck {
            vertices: check_mesh!(@element_check symbols; $vertex_checks),
            faces: check_mesh!(@element_check symbols; $face_checks),
            edges: check_mesh!(@edge_check symbols; $edge_checks),
        };

        // Run Checks!
        let res = std::panic::catch_unwind(|| {
            checker.check_basic(&$mesh);
            test_helper!(@if BasicAdj in $extras => {
                checker.check_basic_adj(&$mesh);

                test_helper!(@if TriMesh in $extras => {
                    checker.check_basic_adj_tri(&$mesh);
                });
            });
            test_helper!(@if FullAdj in $extras => {
                checker.check_full_adj(&$mesh);

                test_helper!(@if TriMesh in $extras => {
                    checker.check_full_adj_tri(&$mesh);
                });
            });
            test_helper!(@if EdgeMesh in $extras => {
                checker.check_basic_edge(&$mesh);
                test_helper!(@if EdgeAdj in $extras => {
                    checker.check_edge_adj(&$mesh);
                });
            });
        });

        if let Err(e) = res {
            eprintln!();
            eprintln!("+++++ Additional failure information +++++");
            eprintln!("symbols: {:#?}", symbols);
            eprintln!();
            eprintln!("mesh: {:#?}", $mesh);

            std::panic::resume_unwind(e);
        }
    }};

    // Match on the different syntax for element checks
    (@element_check $sym:ident; no_check) => { ElementCheck::NoCheck };
    (@element_check $sym:ident; {
        $(
            $h:ident => $nf:tt, $nv:tt, $boundary:ident
        );*
        $(;)?
        ... $(; $len:expr)?
    } ) =>  {
        ElementCheck::Partial {
            elements: vec![$(
                check_mesh!(@make_element $sym; $h => $nf, $nv, $boundary)
            ),* ],
            len: [$($len)?].get(0).copied(),
        }
    };
    (@element_check $sym:ident; {
        $(
            $h:ident => $nf:tt, $nv:tt, $boundary:ident
        );*
        $(;)?
    } ) =>  {
        ElementCheck::Full(vec![$(
            check_mesh!(@make_element $sym; $h => $nf, $nv, $boundary)
        ),* ])
    };

    // Helper to construct an `ElementInfo`
    (@make_element $sym:ident; $h:ident => $nf:tt, $nv:tt, $boundary:ident) => {{
        $sym.add($h, stringify!($h));

        ElementInfo {
            handle: $h,
            boundary: check_mesh!(@is_boundary $boundary),
            adjacent_faces: check_mesh!(@neighbor_check $nf),
            adjacent_vertices: check_mesh!(@neighbor_check $nv),
        }
    }};

    // Match on the different syntax for element checks
    (@edge_check $sym:ident; no_check) => { ElementCheck::NoCheck };
    (@edge_check $sym:ident; {
        $(
            $va:ident -- $vb:ident $(@ $eh:ident)? => $nf:tt, $boundary:ident
        );*
        $(;)?
        ... $(; $len:expr)?
    } ) => {
        ElementCheck::Partial {
            elements: vec![$(
                check_mesh!(@make_edge $sym; $va -- $vb $(@ $eh)? => $nf, $boundary)
            ),* ],
            len: [$($len)?].get(0).copied(),
        }
    };
    (@edge_check $sym:ident; {
        $(
            $va:ident -- $vb:ident $(@ $eh:ident)? => $nf:tt, $boundary:ident
        );*
        $(;)?
    } ) =>  {
        ElementCheck::Full(vec![$(
            check_mesh!(@make_edge $sym; $va -- $vb $(@ $eh)? => $nf, $boundary)
        ),* ])
    };

    // Helper to construct an `EdgeInfo`
    (@make_edge $sym:ident;
        $va:ident -- $vb:ident $(@ $eh:ident)? => $nf:tt, $boundary:ident
    ) => {{
        $( $sym.add($eh, stringify!($eh)); )?

        EdgeInfo {
            vertices: [$va, $vb],
            handle: [$($eh)?].get(0).copied(),
            boundary: check_mesh!(@is_boundary $boundary),
            adjacent_faces: check_mesh!(@neighbor_check $nf),
        }
    }};


    // Match on different neighbor check syntax
    (@neighbor_check no_check) => { NeighborCheck::NoCheck };
    (@neighbor_check [$($n:ident),*]) => {
        NeighborCheck::OrderDefined(vec![$($n),*])
    };
    (@neighbor_check {$($n:ident),* ... $(; $len:expr)? }) => {
        NeighborCheck::Partial {
            partial_neighbors: vec![$($n),*],
            len: [$($len)?].get(0).copied(),
        }
    };
    (@neighbor_check {$($n:ident),*}) => {
        NeighborCheck::OrderUndefined(vec![$($n),*])
    };

    // Helper to convert the boundary word into a bool
    (@is_boundary no_check) => { None };
    (@is_boundary boundary) => { Some(true) };
    (@is_boundary interior) => { Some(false) };
}


macro_rules! test_helper {
    () => { compile_error!("internal macro: don't use anywhere else") };

    // These two arms are used to conditionally expand to a given body.
    //
    // If the first ident ($needle) is in the list following it, these arms
    // expand to `$body`, otherwise they expand to an empty expression.
    (@if $needle:ident in [] => $body:tt) => {{
        // The needle was not found in the extra traits. To make sure there
        // wasn't a typo bug in this macro definition, we check that `$needle`
        // is a valid extra trait to begin with.
        test_helper!(@is_valid_extra_trait $needle);
    }};
    (@if $needle:ident in [$head:ident $(, $tail:ident)*] => $body:tt) => {{
        // This is a pretty nifty (or ugly?) trick. To check two idents for
        // equality, we define a helper macro here. Note that the patterns in
        // this macro don't contain any meta variables (the meta variables
        // $meta and $head are interpolated). If the two are not equal, we
        // recursively search through the list.
        macro_rules! __inner_helper {
            ($needle $needle) => { $body };
            ($needle $head) => { test_helper!(@if $needle in [$($tail),*] => $body) }
        };

        __inner_helper!($needle $head)
    }};

    // This is the same as above but for bodies which expand to items (instead
    // of expressions).
    (@if_item [$needle:ident] in [] => { $($body:tt)* }) => {
        test_helper!(@is_valid_extra_trait $needle);
    };
    (@if_item [$needle:ident] in [$head:ident $(, $tail:ident)*] => { $($body:tt)* }) => {
        macro_rules! __inner_helper {
            ($needle $needle) => { $($body)* };
            ($needle $head) => {
                test_helper!(@if_item [$needle] in [$($tail),*] => { $($body)* });
            }
        }

        __inner_helper!($needle $head);
    };
    (@if_item [$head:ident $(, $tail:ident)*] in $extra:tt => $body:tt) => {
        test_helper!(@if_item [$head] in $extra => {
            test_helper!(@if_item [$($tail),*] in $extra => $body);
        });
    };

    // These arms are used to make sure all traits passed into the macro
    // (include the ones used in the definition of the macro) are valid.
    // Otherwise it's too easy to make a typo.
    (@is_valid_extra_trait BasicAdj) => {};
    (@is_valid_extra_trait FullAdj) => {};
    (@is_valid_extra_trait EdgeAdj) => {};
    (@is_valid_extra_trait TriMesh) => {};
    (@is_valid_extra_trait PolyMesh) => {};
    (@is_valid_extra_trait EdgeMesh) => {};
    (@is_valid_extra_trait Manifold) => {}; // this is not a real trait yet...
    (@is_valid_extra_trait SupportsMultiBlade) => {};
    (@is_valid_extra_trait $other:ident) => {
        compile_error!(concat!(
            "`",
            stringify!($other),
            "` is not a valid trait to pass to `gen_mesh_tests`",
        ));
    };
}
