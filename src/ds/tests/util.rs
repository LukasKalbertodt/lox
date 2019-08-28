use std::{
    collections::{BTreeSet, HashSet},
    cmp::PartialEq,
    fmt::{Debug, Write},
};

use crate::{
    prelude::*,
    handle::hsize,
    util::DiList,
};



/// Takes an iterator and a list of elements. Collects both into sets and
/// compares those sets for equality via `assert_eq`.
macro_rules! assert_eq_set {
    ($iter:expr, [$($item:expr),* $(,)*] $(,)?) => {
        crate::ds::tests::util::assert_eq_set_fn(
            $iter,
            &[$($item),*],
            stringify!($iter),
            stringify!([$($item),*]),
        );
    }
}

/// Internal helper function for `assert_eq_set`.
pub fn assert_eq_set_fn<I, T>(actual: I, expected: &[T], left_str: &str, right_str: &str)
where
    I: Iterator<Item = T>,
    T: Debug + Clone + Eq + Ord,
{
    let actual = set(actual);
    let expected = set(expected.iter().cloned());
    if actual != expected {
        panic!(
            "assert_eq_set({}, {}) failed:\n  left: {:?}\n right: {:?} ",
            left_str,
            right_str,
            actual,
            expected,
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


/// Takes three edges `$e0`, `$e1` and `$e2` that all belong to one face `$f`
/// and makes sure:
/// - Each edge is adjacent to the face `$f`
/// - The set of the other adjacent faces equals the set of neighbor faces
///   `$neighbor`, where neighborfaces are an `Option`.
macro_rules! assert_face_edges {
    ($mesh:ident, [$e0:expr, $e1:expr, $e2:expr], $f:expr, [$($neighbor:expr),* $(,)*]) => {{
        let edge_faces = [
            $mesh.faces_of_edge($e0),
            $mesh.faces_of_edge($e1),
            $mesh.faces_of_edge($e2),
        ];
        crate::ds::tests::util::assert_face_edges_fn($f, [$e0, $e1, $e2], edge_faces, &[$($neighbor),*])
    }}
}

/// Internal helper function for `assert_face_edges`.
pub fn assert_face_edges_fn(
    face: FaceHandle,
    edges: [EdgeHandle; 3],
    edge_faces: [DiList<FaceHandle>; 3],
    neighbors: &[FaceHandle],
) -> Vec<EdgeHandle> {
    let mut actual = HashSet::new();
    let mut corresponding_edges = [None; 3];
    for (&edge, adjacent_faces) in edges.iter().zip(&edge_faces) {
        let adjacent_faces = adjacent_faces.into_vec();

        match adjacent_faces.len() {
            0 => {
                panic!(
                    "assert_face_edges failed: {:?} is supposed to be an edge of face {:?}, \
                        but the list returned by faces_of_edge({:?}) is empty",
                    edge,
                    face,
                    edge,
                );
            }
            1 => {
                assert!(
                    adjacent_faces[0] == face,
                    "assert_face_edges failed: {:?} is supposed to be an edge of face {:?}, \
                        but the list returned by faces_of_edge({:?}) does not contain {:1?} \
                        (it just contains {:?})",
                    edge,
                    face,
                    edge,
                    face,
                    adjacent_faces[0],
                );
            }
            2 => {
                match adjacent_faces.iter().position(|&f| f == face) {
                    None => {
                        panic!(
                            "assert_face_edges failed: {:?} is supposed to be an edge of \
                                face {:?}, but the list returned by faces_of_edge({:?}) \
                                does not contain {:?} (actual list: {:?})",
                            edge,
                            face,
                            edge,
                            face,
                            adjacent_faces,
                        );
                    }
                    Some(pos) => {
                        let other_pos = if pos == 0 { 1 } else { 0 };
                        actual.insert(adjacent_faces[other_pos]);

                        // Here we try to find the face this edge corresponds
                        // to. If `position` returns `None`, we already know
                        // that this assert will fail. But we will just ignore
                        // it, because it will be detected below -- with a nice
                        // error message.
                        let pos = neighbors.iter().position(|n| *n == adjacent_faces[other_pos]);
                        if let Some(pos) = pos {
                            corresponding_edges[pos] = Some(edge);
                        }
                    }
                }
            }
            _ => unreachable!(), // guaranteed by `DiList`
        }
    }

    let expected = neighbors.iter().cloned().collect::<HashSet<_>>();
    if actual != expected {
        let mut data = String::new();
        for (&edge, adjacent_faces) in edges.iter().zip(&edge_faces) {
            writeln!(
                data,
                "faces_of_edge({:?}) => {:?}",
                edge,
                adjacent_faces.into_vec(),
            ).unwrap();
        }

        panic!(
            "assert_face_edges failed: the expected neighborfaces ({:?}) of face {:?} with \
                edges {:?} do not equal the actual neighborfaces reported by faces_of_edge \
                ({:?}). Exact data: \n{}",
            expected,
            face,
            edges,
            actual,
            data,
        );
    }

    // We can now just take all the `Some` values from that array and put it
    // into a vector. In fact, if we reach this point, the number of `Some`
    // elements is exactly `neighbors.len()` *and* they are in the beginning
    // of the array.
    corresponding_edges.iter()
        .filter_map(|opt| *opt)
        .collect()
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
    pub(crate) vertices: ElementCheck<VertexHandle>,
    pub(crate) faces: ElementCheck<FaceHandle>,
    pub(crate) edges: ElementCheck<EdgeHandle>,
}

/// Specifies how elements of a mesh (face, vertex, edge) should be checked.
#[derive(Debug)]
pub(crate) enum ElementCheck<H: Handle> {
    /// The elements are given as a set, i.e. order will not be checked.
    Full(Vec<ElementInfo<H>>),

    /// Not all elements were specified, but these given ones have to be part
    /// of the mesh. Additionally, the number of elements might be specified
    /// and will be checked.
    Partial {
        elements: Vec<ElementInfo<H>>,
        len: Option<usize>,
    },

    /// The elements should not be checked.
    NoCheck,
}

/// Contains information about a specific element (face, vertex, edge).
#[derive(Debug)]
pub(crate) struct ElementInfo<H: Handle> {
    pub(crate) handle: H,
    pub(crate) boundary: bool,
    pub(crate) adjacent_faces: NeighborCheck<FaceHandle>,
    pub(crate) adjacent_vertices: NeighborCheck<VertexHandle>,
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

impl<H: Handle> ElementCheck<H> {
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
    fn elements(&self) -> &[ElementInfo<H>] {
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
        if self.boundary != H::is_boundary(mesh, self.handle) {
            panic!(
                "mesh says {:?} is {}a boundary {}, but it is{}",
                self.handle,
                if self.boundary { "not " } else { "" },
                H::SINGULAR,
                if self.boundary { "" } else { " not" },
            );
        }

        // Check `are_*_adjacent` method
        H::for_all(mesh, |other| {
            let actual_adjacent = H::are_adjacent(mesh, self.handle, other);
            let expected_adjacent = H::neighbors(&self).potentially_partial().contains(&other);
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
}




// ===== The public macros =======================================================================

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
        use crate::ds::tests::util::{MeshCheck, ElementCheck, ElementInfo, NeighborCheck};

        // Build check description
        let checker = MeshCheck {
            vertices: check_mesh!(@element_check $vertex_checks),
            faces: check_mesh!(@element_check $face_checks),
            edges: check_mesh!(@element_check $edge_checks),
        };

        // Run Checks!
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
    }};

    // Match on the different syntax for element checks
    (@element_check no_check) => { ElementCheck::NoCheck };
    (@element_check {
        $(
            $h:ident => $nf:tt, $nv:tt, $boundary:ident
        );*
        $(;)?
        ... $(; $len:expr)?
    } ) =>  {
        ElementCheck::Partial {
            elements: vec![$(
                check_mesh!(@make_element $h => $nf, $nv, $boundary)
            ),* ],
            len: [$($len)?].get(0).copied(),
        }
    };
    (@element_check {
        $(
            $h:ident => $nf:tt, $nv:tt, $boundary:ident
        );*
        $(;)?
    } ) =>  {
        ElementCheck::Full(vec![$(
            check_mesh!(@make_element $h => $nf, $nv, $boundary)
        ),* ])
    };

    // Helper to construct an `ElementInfo`
    (@make_element $h:ident => $nf:tt, $nv:tt, $boundary:ident) => {
        ElementInfo {
            handle: $h,
            boundary: check_mesh!(@is_boundary $boundary),
            adjacent_faces: check_mesh!(@neighbor_check $nf),
            adjacent_vertices: check_mesh!(@neighbor_check $nv),
        }
    };

    // Helper to convert the boundary word into a bool
    (@is_boundary boundary) => { true };
    (@is_boundary interior) => { false };

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
            "` is not a valid trait to pass to `gen_tri_mesh_tests`",
        ));
    };
}
