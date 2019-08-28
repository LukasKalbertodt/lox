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
    let actual = actual.collect::<BTreeSet<_>>();
    let expected = expected.iter().cloned().collect::<BTreeSet<_>>();
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

/// Helper function for macro `assert_eq_order`. Function instead of macro to
/// improve test compile times (no inlining!).
#[inline(never)]
pub fn assert_eq_order<T: Debug + PartialEq + Copy>(
    actual: &[T],
    expected: &[T],
    actual_str: &str,
) {
    if actual.len() != expected.len() {
        panic!(
            "assert_eq_order failed (length mismatch): \n  \
                left: `{:?}` (`{}`),\n \
                right: `{:?}`",
            actual,
            actual_str,
            expected,
        );
    }

    if actual.len() == 0 {
        return;
    }

    let pos = actual.iter().position(|&e| e == expected[0]).unwrap_or_else(|| {
        panic!(
            "assert_eq_order failed: {:?} not found in {} (expected {:?})",
            expected[0],
            actual_str,
            expected,
        );
    });

    let mut rotated = expected.to_vec();
    rotated.rotate_right(pos);


    if actual != &rotated[..] {
        panic!(
            "assert_eq_order failed: \n  \
                left: `{:?}` (`{}`),\n \
                right: `{:?}` (original `{:?}`)",
            actual,
            actual_str,
            rotated,
            expected,
        );
    }
}

macro_rules! test_helper {
    () => { compile_error!("internal macro: don't use anywhere else") };
    (@is_boundary boundary) => { true };
    (@is_boundary interior) => { false };
    (@as_neighbors no_check) => { crate::ds::tests::util::Neighbors::NoCheck };
    (@as_neighbors [$($n:ident),*]) => {
        crate::ds::tests::util::Neighbors::OrderDefined(vec![$($n),*])
    };
    (@as_neighbors {... $($n:ident),* ; $len:expr }) => {
        crate::ds::tests::util::Neighbors::Partial(vec![$($n),*], Some($len))
    };
    (@as_neighbors {... $($n:ident),*}) => {
        crate::ds::tests::util::Neighbors::Partial(vec![$($n),*], None)
    };
    (@as_neighbors {$($n:ident),*}) => {
        crate::ds::tests::util::Neighbors::OrderUndefined(vec![$($n),*])
    };

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

pub enum Neighbors<H: Handle> {
    /// Neighbors are given in a fixed order. The vector can still be rotated
    /// (as the first element is not fixed).
    OrderDefined(Vec<H>),

    /// The neighbors are given as a set, i.e. order will not be checked.
    OrderUndefined(Vec<H>),

    /// Not all neighbors were specified, but these given ones have to be a
    /// neighbor. Additionally, the number of neighbors might be specified and
    /// will be checked.
    Partial(Vec<H>, Option<usize>),

    /// The neighbors should not be checked.
    NoCheck,
}

impl<H: Handle> Neighbors<H> {
    fn check(&self, actual: &[H], actual_str: &str) {
        match self {
            Neighbors::NoCheck => {}
            Neighbors::OrderDefined(neighbors) => {
                assert_eq_order(actual, neighbors, actual_str);
            }
            Neighbors::OrderUndefined(neighbors) => {
                assert_eq_set_fn(
                    actual.iter().cloned(),
                    neighbors,
                    actual_str,
                    &format!("{:?}", neighbors),
                );
            }
            Neighbors::Partial(neighbors, len) => {
                for expected in neighbors {
                    if !actual.contains(&expected) {
                        panic!(
                            "{:?} was expected to be in `{}`, but it isn't (actual value: {:?})",
                            expected,
                            actual_str,
                            actual,
                        );
                    }
                }

                if let Some(len) = len {
                    if actual.len() != *len {
                        panic!(
                            "`{}` was expected to has the length {}, but it actually \
                                has the length {} ({:?})",
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

    fn raw_neighbors(&self) -> Option<&[H]> {
        match self {
            Neighbors::NoCheck => None,
            Neighbors::Partial(_, _) => None,
            Neighbors::OrderDefined(neighbors) => Some(neighbors),
            Neighbors::OrderUndefined(neighbors) => Some(neighbors),
        }
    }
}

// ===============================================================================================
// ===== `assert_vertices` and helpers
// ===============================================================================================

macro_rules! assert_vertices {
    (
        $mesh:ident; [$($extra:ident),*]; $(
            $vh:ident => $nf:tt, $nv:tt, $boundary:ident
        );* $(;)?
    ) =>  {
        let vertices = vec![$(
            crate::ds::tests::util::VertexInfo {
                handle: $vh,
                boundary: test_helper!(@is_boundary $boundary),
                adjacent_faces: test_helper!(@as_neighbors $nf),
                adjacent_vertices: test_helper!(@as_neighbors $nv),
            }
        ),*];

        crate::ds::tests::util::assert_vertices_basic(&$mesh, &vertices);

        test_helper!(@if FullAdj in [$($extra),*] => {
            crate::ds::tests::util::assert_vertices_full_adj(&$mesh, &vertices);
        });
    };
}

pub struct VertexInfo {
    pub handle: VertexHandle,
    pub boundary: bool,
    pub adjacent_faces: Neighbors<FaceHandle>,
    pub adjacent_vertices: Neighbors<VertexHandle>,
}

pub fn assert_vertices_basic<M: Mesh>(mesh: &M, vertices: &[VertexInfo]) {
    if mesh.num_vertices() != vertices.len() as hsize {
        panic!(
            "num_vertices() returned {}, but is supposed to return {}",
            mesh.num_vertices(),
            vertices.len(),
        );
    }

    let vertex_handles = vertices.iter().map(|v| v.handle).collect::<Vec<_>>();
    assert_eq_set_fn(
        mesh.vertex_handles(),
        &vertex_handles,
        "mesh.vertex_handles()",
        &format!("{:?}", vertex_handles),
    );

    for v in vertices {
        let vh = v.handle;
        if !mesh.contains_vertex(vh) {
            panic!("mesh.contains_vertex({:?}) returned false, should return true", vh);
        }

        let last_vertex_handle = mesh.last_vertex_handle().unwrap();
        if vh.idx() > last_vertex_handle.idx() {
            panic!(
                "mesh.last_vertex_handle() returned {:?} which has a smaller index than {:?}",
                last_vertex_handle,
                vh,
            );
        }
    }
}

pub fn assert_vertices_full_adj<M: FullAdj>(mesh: &M, vertices: &[VertexInfo]) {
    for v in vertices {
        if v.boundary != mesh.is_boundary_vertex(v.handle) {
            panic!(
                "mesh says {:?} is {}a boundary vertex, but it is{}",
                v.handle,
                if v.boundary { "not " } else { "" },
                if v.boundary { "" } else { " not" },
            );
        }

        match &v.adjacent_vertices {
            Neighbors::OrderDefined(n) | Neighbors::OrderUndefined(n) => {
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

        v.adjacent_faces.check(
            &mesh.faces_around_vertex(v.handle).into_vec(),
            &format!("mesh.faces_around_vertex({:?})", v.handle),
        );
        v.adjacent_vertices.check(
            &mesh.vertices_around_vertex(v.handle).into_vec(),
            &format!("mesh.vertices_around_vertex({:?})", v.handle),
        );

        if let Some(adjacent_vertices) = v.adjacent_vertices.raw_neighbors() {
            for vb in mesh.vertex_handles() {
                let are_adjacent = mesh.are_vertices_adjacent(vb, v.handle);
                if are_adjacent != adjacent_vertices.contains(&vb) {
                    panic!(
                        "are_vertices_adjacent({:?}, {:?}) returned {}, but those vertices \
                            are{} adjacent",
                        vb,
                        v.handle,
                        are_adjacent,
                        if adjacent_vertices.contains(&vb) { "" } else { " not" },
                    );
                }
            }
        }
    }
}


// ===============================================================================================
// ===== `assert_faces` and helpers
// ===============================================================================================

macro_rules! assert_faces {
    (
        $mesh:ident; [$($extra:ident),*]; $(
            $fh:ident => $nf:tt, $nv:tt, $boundary:ident
        );* $(;)?
    ) =>  {
        let faces = vec![$(
            crate::ds::tests::util::FaceInfo {
                handle: $fh,
                boundary: test_helper!(@is_boundary $boundary),
                adjacent_faces: test_helper!(@as_neighbors $nf),
                adjacent_vertices: test_helper!(@as_neighbors $nv),
            }
        ),*];

        crate::ds::tests::util::assert_faces_basic(&$mesh, &faces);

        test_helper!(@if BasicAdj in [$($extra),*] => {
            crate::ds::tests::util::assert_faces_basic_adj(&$mesh, &faces);

            test_helper!(@if TriMesh in [$($extra),*] => {
                crate::ds::tests::util::assert_faces_basic_adj_tri(&$mesh, &faces);
            });
        });

        test_helper!(@if FullAdj in [$($extra),*] => {
            crate::ds::tests::util::assert_faces_full_adj(&$mesh, &faces);

            test_helper!(@if TriMesh in [$($extra),*] => {
                crate::ds::tests::util::assert_faces_full_adj_tri(&$mesh, &faces);
            });
        });
    };
}

pub struct FaceInfo {
    pub handle: FaceHandle,
    pub boundary: bool,
    pub adjacent_faces: Neighbors<FaceHandle>,
    pub adjacent_vertices: Neighbors<VertexHandle>,
}

pub fn assert_faces_basic<M: Mesh>(mesh: &M, faces: &[FaceInfo]) {
    // We do that here because this function is always called when other
    // asserts are checked.
    mesh.check_integrity();

    if mesh.num_faces() != faces.len() as hsize {
        panic!(
            "num_faces() returned {}, but is supposed to return {}",
            mesh.num_faces(),
            faces.len(),
        );
    }

    let face_handles = faces.iter().map(|f| f.handle).collect::<Vec<_>>();
    assert_eq_set_fn(
        mesh.face_handles(),
        &face_handles,
        "mesh.face_handles()",
        &format!("{:?}", face_handles),
    );

    for f in faces {
        let fh = f.handle;
        if !mesh.contains_face(fh) {
            panic!("mesh.contains_face({:?}) returned false, should return true", fh);
        }

        let last_face_handle = mesh.last_face_handle().unwrap();
        if fh.idx() > last_face_handle.idx() {
            panic!(
                "mesh.last_face_handle() returned {:?} which has a smaller index than {:?}",
                last_face_handle,
                fh,
            );
        }
    }
}

pub fn assert_faces_basic_adj<M: BasicAdj>(mesh: &M, faces: &[FaceInfo]) {
    for f in faces {
        f.adjacent_vertices.check(
            &mesh.vertices_around_face(f.handle).into_vec(),
            &format!("mesh.vertices_around_face({:?})", f.handle),
        );
    }
}

pub fn assert_faces_basic_adj_tri<M: BasicAdj + TriMesh>(mesh: &M, faces: &[FaceInfo]) {
    for f in faces {
        f.adjacent_vertices.check(
            &mesh.vertices_around_triangle(f.handle),
            &format!("mesh.vertices_around_triangle({:?})", f.handle),
        );
    }
}

pub fn assert_faces_full_adj<M: FullAdj>(mesh: &M, faces: &[FaceInfo]) {
    for f in faces {
        if f.boundary != mesh.is_boundary_face(f.handle) {
            panic!(
                "mesh says {:?} is {}a boundary face, but it is{}",
                f.handle,
                if f.boundary { "not " } else { "" },
                if f.boundary { "" } else { " not" },
            );
        }

        f.adjacent_faces.check(
            &mesh.faces_around_face(f.handle).into_vec(),
            &format!("mesh.faces_around_face({:?})", f.handle),
        );

        if let Some(adjacent_faces) = f.adjacent_faces.raw_neighbors() {
            for fb in mesh.face_handles() {
                let are_adjacent = mesh.are_faces_adjacent(fb, f.handle);
                if are_adjacent != adjacent_faces.contains(&fb) {
                    panic!(
                        "are_faces_adjacent({:?}, {:?}) returned {}, but those faces \
                            are{} adjacent",
                        fb,
                        f.handle,
                        are_adjacent,
                        if adjacent_faces.contains(&fb) { "" } else { " not" },
                    );
                }
            }
        }
    }
}

pub fn assert_faces_full_adj_tri<M: FullAdj + TriMesh>(mesh: &M, faces: &[FaceInfo]) {
    for f in faces {
        f.adjacent_faces.check(
            &mesh.faces_around_triangle(f.handle).into_vec(),
            &format!("mesh.faces_around_triangle({:?})", f.handle),
        );
    }
}
