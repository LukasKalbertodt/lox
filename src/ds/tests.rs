//! This module contains macros to generate unit tests for mesh data
//! structures.

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
        crate::ds::tests::assert_eq_set_fn(
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
        crate::ds::tests::assert_face_edges_fn($f, [$e0, $e1, $e2], edge_faces, &[$($neighbor),*])
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
    (@as_neighbors no_check) => { crate::ds::tests::Neighbors::NoCheck };
    (@as_neighbors [$($n:ident),*]) => {
        crate::ds::tests::Neighbors::OrderDefined(vec![$($n),*])
    };
    (@as_neighbors {$($n:ident),*}) => {
        crate::ds::tests::Neighbors::OrderUndefined(vec![$($n),*])
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

#[allow(dead_code)]
pub enum Neighbors<H: Handle> {
    OrderDefined(Vec<H>),
    OrderUndefined(Vec<H>),
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
        }
    }

    fn raw_neighbors(&self) -> Option<&[H]> {
        match self {
            Neighbors::NoCheck => None,
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
            crate::ds::tests::VertexInfo {
                handle: $vh,
                boundary: test_helper!(@is_boundary $boundary),
                adjacent_faces: test_helper!(@as_neighbors $nf),
                adjacent_vertices: test_helper!(@as_neighbors $nv),
            }
        ),*];

        crate::ds::tests::assert_vertices_basic(&$mesh, &vertices);

        test_helper!(@if FullAdj in [$($extra),*] => {
            crate::ds::tests::assert_vertices_full_adj(&$mesh, &vertices);
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
            crate::ds::tests::FaceInfo {
                handle: $fh,
                boundary: test_helper!(@is_boundary $boundary),
                adjacent_faces: test_helper!(@as_neighbors $nf),
                adjacent_vertices: test_helper!(@as_neighbors $nv),
            }
        ),*];

        crate::ds::tests::assert_faces_basic(&$mesh, &faces);

        test_helper!(@if BasicAdj in [$($extra),*] => {
            crate::ds::tests::assert_faces_basic_adj(&$mesh, &faces);

            test_helper!(@if TriMesh in [$($extra),*] => {
                crate::ds::tests::assert_faces_basic_adj_tri(&$mesh, &faces);
            });
        });

        test_helper!(@if FullAdj in [$($extra),*] => {
            crate::ds::tests::assert_faces_full_adj(&$mesh, &faces);

            test_helper!(@if TriMesh in [$($extra),*] => {
                crate::ds::tests::assert_faces_full_adj_tri(&$mesh, &faces);
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


// ===============================================================================================
// ===== The main macro containing the test suite
// ===============================================================================================

/// Generates unit tests for the mesh data structure `$name`.
///
/// In the brackets, you should specify additional traits that are implemented
/// for the mesh type. These will generate additional asserts in the tests. The
/// following traits are assumed to be implemented by every mesh type this
/// macro is invoked with:
/// - `TriMesh`
/// - `TriMeshMut`
macro_rules! gen_tri_mesh_tests {
    ($name:ty : [$($extra:ident),*]) => {
        $(
            test_helper!(@is_valid_extra_trait $extra);
        )*

        // TODO: make sure exactly once of `TriMesh` and `PolyMesh` is
        // specified as extra trait.

        #[allow(unused_imports)]
        use crate::{
            prelude::*,
            handle::{Handle, HSizeExt},
        };

        #[test]
        fn empty() {
            let m = <$name>::empty();

            assert_eq!(m.num_faces(), 0);
            assert_eq!(m.num_vertices(), 0);

            assert!(m.faces().next().is_none());
            assert!(m.vertices().next().is_none());

            assert!(!m.contains_vertex(VertexHandle::new(0)));
            assert!(!m.contains_vertex(VertexHandle::new(27)));
            assert!(!m.contains_face(FaceHandle::new(0)));
            assert!(!m.contains_face(FaceHandle::new(27)));

            test_helper!(@if EdgeMesh in [$($extra),*] => {
                assert_eq!(m.num_edges(), 0);
                assert!(m.edges().next().is_none());
                assert!(!m.contains_edge(EdgeHandle::new(0)));
                assert!(!m.contains_edge(EdgeHandle::new(27)));
            });
        }

        #[test]
        fn single_vertex() {
            let mut m = <$name>::empty();
            let v = m.add_vertex();

            assert!(!m.contains_vertex(VertexHandle::new(v.idx().next())));

            assert_faces!(m; [$($extra),*];);

            assert_vertices!(m; [$($extra),*];
                v    => [], [], boundary;
            );

            test_helper!(@if EdgeMesh in [$($extra),*] => {
                assert_eq!(m.num_edges(), 0);
                assert!(m.edges().next().is_none());
            });
        }

        #[test]
        fn single_triangle() {
            //
            //         (C)
            //        /   \
            //       /     \
            //      /       \
            //    (A) ----- (B)
            //
            let mut m = <$name>::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let f = m.add_triangle([va, vb, vc]);

            assert_faces!(m; [$($extra),*];
                f => [], [va, vb, vc], boundary;
            );

            assert_vertices!(m; [$($extra),*];
                va => [f], [vc, vb], boundary;
                vb => [f], [va, vc], boundary;
                vc => [f], [vb, va], boundary;
            );

            assert!(!m.contains_face(FaceHandle::new(f.idx().next())));

            test_helper!(@if EdgeMesh in [$($extra),*] => {
                let e0 = EdgeHandle::new(0);
                let e1 = EdgeHandle::new(1);
                let e2 = EdgeHandle::new(2);

                assert_eq!(m.num_edges(), 3);
                assert_eq_set!(m.edge_handles(), [e0, e1, e2]);
                assert!(m.contains_edge(e0));
                assert!(m.contains_edge(e1));
                assert!(m.contains_edge(e2));

                test_helper!(@if EdgeAdj in [$($extra),*] => {
                    assert_face_edges!(m, [e0, e1, e2], f, []);
                });
            });
        }

        #[test]
        fn tetrahedron() {
            //
            //             (T)
            //            / | \
            //           /  |  \
            //          /   |   \
            //         /   (C)   \
            //        / ⋰     ⋱  \
            //       (A) ------- (B)
            //
            let mut m = <$name>::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let v_top = m.add_vertex();

            let f_bottom = m.add_triangle([va, vc, vb]);
            let f_ab = m.add_triangle([va, vb, v_top]);
            let f_bc = m.add_triangle([vb, vc, v_top]);
            let f_ca = m.add_triangle([vc, va, v_top]);

            assert_faces!(m; [$($extra),*];
                f_bottom => [f_ca, f_bc, f_ab],     [va, vc, vb],    interior;
                f_ab     => [f_bc, f_ca, f_bottom], [va, vb, v_top], interior;
                f_bc     => [f_ca, f_ab, f_bottom], [vb, vc, v_top], interior;
                f_ca     => [f_ab, f_bc, f_bottom], [vc, va, v_top], interior;
            );

            assert_vertices!(m; [$($extra),*];
                va    => [f_bottom, f_ca, f_ab], [v_top, vb, vc], interior;
                vb    => [f_bottom, f_ab, f_bc], [v_top, vc, va], interior;
                vc    => [f_bottom, f_bc, f_ca], [v_top, va, vb], interior;
                v_top => [f_ca, f_bc, f_ab],     [va, vc, vb],    interior;
            );

            test_helper!(@if EdgeMesh in [$($extra),*] => {
                let e0 = EdgeHandle::new(0);
                let e1 = EdgeHandle::new(1);
                let e2 = EdgeHandle::new(2);
                let e3 = EdgeHandle::new(3);
                let e4 = EdgeHandle::new(4);
                let e5 = EdgeHandle::new(5);

                assert_eq!(m.num_edges(), 6);
                assert_eq_set!(m.edge_handles(), [e0, e1, e2, e3, e4, e5]);
                assert!(m.contains_edge(e0));
                assert!(m.contains_edge(e1));
                assert!(m.contains_edge(e2));
                assert!(m.contains_edge(e3));
                assert!(m.contains_edge(e4));
                assert!(m.contains_edge(e5));

                test_helper!(@if EdgeAdj in [$($extra),*] => {
                    let e = assert_face_edges!(m, [e0, e1, e2], f_bottom, [f_ab, f_bc, f_ca]);
                    let e_ab = e[0];
                    let e_bc = e[1];
                    let e_ca = e[2];

                    let e = assert_face_edges!(m, [e_ab, e3, e4], f_ab, [f_bottom, f_bc, f_ca]);
                    let e_bt = e[1];
                    let e_at = e[2];
                    let e_ct = e5;

                    assert_face_edges!(m, [e_bc, e_bt, e_ct], f_bc, [f_ab, f_bottom, f_ca]);
                    assert_face_edges!(m, [e_ca, e_at, e_ct], f_ca, [f_ab, f_bc, f_bottom]);
                });
            });
        }

        #[test]
        fn triangle_strip_build() {
            //
            //    (A)---(D)
            //     | \ Y | \
            //     |  \  |  \
            //     | X \ | Z \
            //     |    \|    \
            //    (B)---(C)---(E)
            //
            let mut m = <$name>::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let fx = m.add_triangle([va, vb, vc]);
            // Everything is correct now, this is checked by `single_triangle`

            // ----- Add second face
            let vd = m.add_vertex();
            let fy = m.add_triangle([va, vc, vd]);

            assert_faces!(m; [$($extra),*];
                fx => [fy], [va, vb, vc], boundary;
                fy => [fx], [va, vc, vd], boundary;
            );

            assert_vertices!(m; [$($extra),*];
                va => [fy, fx], [vd, vc, vb], boundary;
                vb => [fx],     [va, vc],     boundary;
                vc => [fx, fy], [vb, va, vd], boundary;
                vd => [fy],     [vc, va],     boundary;
            );

            test_helper!(@if EdgeMesh in [$($extra),*] => {
                let eh = EdgeHandle::new;
                assert_eq!(m.num_edges(), 5);
                assert_eq_set!(m.edge_handles(), [eh(0), eh(1), eh(2), eh(3), eh(4)]);

                test_helper!(@if EdgeAdj in [$($extra),*] => {
                    let e = assert_face_edges!(m, [eh(0), eh(1), eh(2)], fx, [fy]);
                    let e_ac = e[0];
                    assert_face_edges!(m, [e_ac, eh(3), eh(4)], fy, [fx]);
                });
            });

            // ----- Add third face
            let ve = m.add_vertex();
            let fz = m.add_triangle([vd, vc, ve]);
            assert_faces!(m; [$($extra),*];
                fx => [fy],     [va, vb, vc], boundary;
                fy => [fx, fz], [va, vc, vd], boundary;
                fz => [fy],     [vd, vc, ve], boundary;
            );

            assert_vertices!(m; [$($extra),*];
                va => [fy, fx],     [vd, vc, vb],     boundary;
                vb => [fx],         [va, vc],         boundary;
                vc => [fx, fy, fz], [vb, va, vd, ve], boundary;
                vd => [fz, fy],     [ve, vc, va],     boundary;
                ve => [fz],         [vc, vd],         boundary;
            );

            test_helper!(@if EdgeMesh in [$($extra),*] => {
                let eh = EdgeHandle::new;
                assert_eq!(m.num_edges(), 7);
                assert_eq_set!(
                    m.edge_handles(),
                    [eh(0), eh(1), eh(2), eh(3), eh(4), eh(5), eh(6)],
                );

                test_helper!(@if EdgeAdj in [$($extra),*] => {
                    let e = assert_face_edges!(m, [eh(0), eh(1), eh(2)], fx, [fy]);
                    let e_ac = e[0];
                    let e = assert_face_edges!(m, [e_ac, eh(3), eh(4)], fy, [fx, fz]);
                    let e_cd = e[1];
                    assert_face_edges!(m, [e_cd, eh(5), eh(6)], fz, [fy]);
                });
            });
        }

        #[test]
        fn simple_2d_hole() {
            // There are only six faces. The triangle in the middle is empty.
            //
            //                       (a)
            //                      / | \
            //     (b--c)          /  |  \        (b--e)
            //   u: [a, c, b]     /  (b)  \     w: [a, b, e]
            //   v: [b, c, d]    /  /   \  \    x: [b, f, e]
            //                  /  /     \  \
            //                 /  /       \  \
            //                /  (d)-----(f)  \
            //               / ⟋            ⟍ \
            //              (c)_______________(e)
            //
            //                    (c--f)
            //                  y: [c, f, d]
            //                  z: [c, e, f]
            //

            let mut m = <$name>::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let vd = m.add_vertex();
            let ve = m.add_vertex();
            let vf = m.add_vertex();

            let fu = m.add_triangle([va, vc, vb]);
            let fv = m.add_triangle([vb, vc, vd]);

            let fw = m.add_triangle([va, vb, ve]);
            let fx = m.add_triangle([vb, vf, ve]);

            let fy = m.add_triangle([vc, vf, vd]);
            let fz = m.add_triangle([vc, ve, vf]);


            // ----- Check stuff
            assert_faces!(m; [$($extra),*];
                fu => [fv, fw], [va, vc, vb], boundary;
                fv => [fu, fy], [vb, vc, vd], boundary;
                fw => [fu, fx], [va, vb, ve], boundary;
                fx => [fz, fw], [vb, vf, ve], boundary;
                fy => [fv, fz], [vc, vf, vd], boundary;
                fz => [fx, fy], [vc, ve, vf], boundary;
            );

            assert_vertices!(m; [$($extra),*];
                va => [fu, fw],         [ve, vb, vc],         boundary;
                vb => [fw, fx, fv, fu], [va, ve, vf, vd, vc], boundary;
                vc => [fu, fv, fy, fz], [va, vb, vd, vf, ve], boundary;
                vd => [fv, fy],         [vb, vf, vc],         boundary;
                ve => [fz, fx, fw],     [vc, vf, vb, va],     boundary;
                vf => [fx, fz, fy],     [vb, ve, vc, vd],     boundary;
            );

            test_helper!(@if EdgeMesh in [$($extra),*] => {
                let eh = EdgeHandle::new;
                assert_eq!(m.num_edges(), 12);
                assert_eq_set!(m.edge_handles(), [
                    eh(0), eh(1), eh(2), eh(3), eh(4), eh(5),
                    eh(6), eh(7), eh(8), eh(9), eh(10), eh(11),
                ]);

                test_helper!(@if EdgeAdj in [$($extra),*] => {
                    let e = assert_face_edges!(m, [eh(0), eh(1), eh(2)], fu, [fv, fw]);
                    let e_bc = e[0];
                    let e_ab = e[1];

                    let e = assert_face_edges!(m, [e_bc, eh(3), eh(4)], fv, [fu, fy]);
                    let e_cd = e[1];

                    let e = assert_face_edges!(m, [e_ab, eh(5), eh(6)], fw, [fu, fx]);
                    let e_be = e[1];

                    let e = assert_face_edges!(m, [e_be, eh(7), eh(8)], fx, [fz, fw]);
                    let e_ef = e[0];

                    let e = assert_face_edges!(m, [e_cd, eh(9), eh(10)], fy, [fv, fz]);
                    let e_cf = e[1];

                    assert_face_edges!(m, [e_cf, e_ef, eh(11)], fz, [fx, fy]);
                });
            });
        }

        #[test]
        fn octahedron_with_hole() {
            //
            //                      (b)
            //                      / \
            //                     /   \
            //                    /  U  \
            //                   /       \
            //       (b) ----- (c) ----- (a) ----- (b)
            //         \       / \       / \       /
            //          \  X  /   \  T  /   \  Y  /
            //           \   /  V  \   /  W  \   /
            //            \ /       \ /       \ /
            //            (f) ----- (d) ----- (e)
            //              \       /
            //               \  Z  /
            //                \   /
            //                 \ /
            //                 (e)
            //
            // The face at the opposite site of `T` is missing.

            let mut m = <$name>::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let vd = m.add_vertex();
            let ve = m.add_vertex();
            let vf = m.add_vertex();

            let ft = m.add_triangle([va, vc, vd]);
            let fu = m.add_triangle([vb, vc, va]);
            let fv = m.add_triangle([vc, vf, vd]);
            let fw = m.add_triangle([va, vd, ve]);
            let fx = m.add_triangle([vb, vf, vc]);
            let fy = m.add_triangle([va, ve, vb]);
            let fz = m.add_triangle([vf, ve, vd]);

            // -- check stuff with hole in the middle (real triforce)
            assert_faces!(m; [$($extra),*];
                ft => [fu, fv, fw], [va, vc, vd], interior;
                fu => [fx, ft, fy], [vb, vc, va], interior;
                fv => [fz, ft, fx], [vc, vf, vd], interior;
                fw => [fy, ft, fz], [va, vd, ve], interior;
                fx => [fv, fu],     [vb, vf, vc], boundary;
                fy => [fu, fw],     [va, ve, vb], boundary;
                fz => [fw, fv],     [vf, ve, vd], boundary;
            );

            assert_vertices!(m; [$($extra),*];
                va => [fu, fy, fw, ft], [vb, ve, vd, vc], interior;
                vb => [fy, fu, fx],     [ve, va, vc, vf], boundary;
                vc => [fu, ft, fv, fx], [va, vd, vf, vb], interior;
                vd => [ft, fw, fz, fv], [va, ve, vf, vc], interior;
                ve => [fz, fw, fy],     [vf, vd, va, vb], boundary;
                vf => [fx, fv, fz],     [vb, vc, vd, ve], boundary;
            );

            // TODO: check edges
        }

        test_helper!(@if_item [TriMesh, EdgeMesh, FullAdj] in [$($extra),*] => {
            #[test]
            fn split_edge_with_two_faces() {
                //
                //             (A)                    (A)
                //            / | \                  / | \
                //           /  |  \                / (N) \
                //          /   |   \      =>      / / | \ \
                //         /   (M)   \            / / (M) \ \
                //        /  ⟋    ⟍  \          / / ⟋   ⟍\ \
                //       (B) ------- (C)        (B) ------- (C)
                //
                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vm = m.add_vertex();

                m.add_triangle([va, vb, vm]);
                let f_bc = m.add_triangle([vb, vc, vm]);
                m.add_triangle([vc, va, vm]);

                let edge = m.edge_between_vertices(va, vm)
                    .expect("`edge_between_vertices` returned `None` incorrectly");

                let res = m.split_edge_with_faces(edge);
                let vn = res.vertex;

                // -- check stuff
                assert_eq!(m.num_faces(), 5);
                assert_eq!(m.num_edges(), 9);

                let split_faces = m.faces_around_vertex(vn).collect::<Vec<_>>();
                assert_eq!(split_faces.len(), 4);
                crate::ds::tests::assert_eq_set_fn(
                    split_faces.into_iter().chain(vec![f_bc]),
                    &m.face_handles().collect::<Vec<_>>(),
                    "split_faces.into_iter().chain(vec![f_bc])",
                    "m.face_handles()",
                );

                // TODO: we should check more stuff here

                assert_vertices!(m; [$($extra),*];
                    va    => no_check, [vc, vn, vb],     boundary;
                    vb    => no_check, [va, vn, vm, vc], boundary;
                    vc    => no_check, [vb, vm, vn, va], boundary;
                    vm    => no_check, [vb, vn, vc],     interior;
                    vn    => no_check, [va, vc, vm, vb], interior;
                );
            }

            #[test]
            fn split_edge_with_one_face() {
                //
                //  (a) ------- (b) ------- (c)      (a) ------- (b) ------- (c)
                //     \         |         /            \       / |         /
                //      \        |        /              \     /  |        /
                //       \       |       /                \   /   |       /
                //        \      |      /                  \ /    |      /
                //         \     |     /        =>         (m)    |     /
                //          \    |    /                      \    |    /
                //           \   |   /                        \   |   /
                //            \  |  /                          \  |  /
                //             \ | /                            \ | /
                //              (d)                              (d)
                //
                //
                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();

                m.add_triangle([va, vd, vb]);
                let f_bdc = m.add_triangle([vb, vd, vc]);

                let edge = m.edge_between_vertices(va, vd)
                    .expect("`edge_between_vertices` returned `None` incorrectly");

                let res = m.split_edge_with_faces(edge);
                let vm = res.vertex;

                // -- check stuff
                assert_eq!(m.num_faces(), 3);
                assert_eq!(m.num_edges(), 7);

                let split_faces = m.faces_around_vertex(vm).collect::<Vec<_>>();
                assert_eq!(split_faces.len(), 2);
                crate::ds::tests::assert_eq_set_fn(
                    split_faces.into_iter().chain(vec![f_bdc]),
                    &m.face_handles().collect::<Vec<_>>(),
                    "split_faces.into_iter().chain(vec![f_bdc])",
                    "m.face_handles()",
                );

                // TODO: we should check more stuff here

                assert_vertices!(m; [$($extra),*];
                    va => no_check, [vb, vm],         boundary;
                    vb => no_check, [vc, vd, vm, va], boundary;
                    vc => [f_bdc],  [vd, vb],         boundary;
                    vd => no_check, [vm, vb, vc],     boundary;
                    vm => no_check, [va, vb, vd],     boundary;
                );
            }
        });

        test_helper!(@if_item [SupportsMultiBlade] in [$($extra),*] => {
            #[test]
            fn vertex_with_two_blades() {
                //
                //      (b)-------(c)
                //        \       /
                //         \  X  /
                //          \   /
                //           \ /
                //           (a)
                //           / \
                //          /   \
                //         /  Y  \
                //        /       \
                //      (d)-------(e)
                //

                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();
                let ve = m.add_vertex();

                let fx = m.add_triangle([va, vc, vb]);
                let fy = m.add_triangle([va, vd, ve]);


                // ----- Check stuff
                assert_faces!(m; [$($extra),*];
                    fx => [], [va, vc, vb], boundary;
                    fy => [], [va, vd, ve], boundary;
                );

                assert_vertices!(m; [$($extra),*];
                    va => [fx, fy], [vb, vc, ve, vd], boundary;
                    vb => [fx],     [vc, va],         boundary;
                    vc => [fx],     [va, vb],         boundary;
                    vd => [fy],     [va, ve],         boundary;
                    ve => [fy],     [vd, va],         boundary;
                );

                test_helper!(@if EdgeMesh in [$($extra),*] => {
                    let eh = EdgeHandle::new;
                    assert_eq!(m.num_edges(), 6);
                    assert_eq_set!(
                        m.edge_handles(),
                        [eh(0), eh(1), eh(2), eh(3), eh(4), eh(5)],
                    );

                    test_helper!(@if EdgeAdj in [$($extra),*] => {
                        assert_face_edges!(m, [eh(0), eh(1), eh(2)], fx, []);
                        assert_face_edges!(m, [eh(3), eh(4), eh(5)], fy, []);
                    });
                });
            }

            #[test]
            fn vertex_with_three_blades() {
                //
                //       (b)-------(c)
                //         \       /
                //          \  X  /
                //           \   /
                //            \ /
                //  (g)-------(a)-------(d)
                //    \       / \       /
                //     \  Z  /   \  Y  /
                //      \   /     \   /
                //       \ /       \ /
                //       (f)       (e)
                //

                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();
                let ve = m.add_vertex();
                let vf = m.add_vertex();
                let vg = m.add_vertex();

                let fx = m.add_triangle([va, vc, vb]);
                let fy = m.add_triangle([va, ve, vd]);
                let fz = m.add_triangle([va, vg, vf]);


                // ----- Check stuff
                assert_faces!(m; [$($extra),*];
                    fx => [], [va, vc, vb], boundary;
                    fy => [], [va, ve, vd], boundary;
                    fz => [], [va, vg, vf], boundary;
                );

                assert_vertices!(m; [$($extra),*];
                    va => {fx, fy, fz}, {vb, vc, vd, ve, vf, vg}, boundary;
                    vb => [fx],         [vc, va],                 boundary;
                    vc => [fx],         [va, vb],                 boundary;
                    vd => [fy],         [va, ve],                 boundary;
                    ve => [fy],         [vd, va],                 boundary;
                    vf => [fz],         [va, vg],                 boundary;
                    vg => [fz],         [vf, va],                 boundary;
                );

                test_helper!(@if EdgeMesh in [$($extra),*] => {
                    let eh = EdgeHandle::new;
                    assert_eq!(m.num_edges(), 9);
                    assert_eq_set!(
                        m.edge_handles(),
                        [eh(0), eh(1), eh(2), eh(3), eh(4), eh(5), eh(6), eh(7), eh(8)],
                    );

                    test_helper!(@if EdgeAdj in [$($extra),*] => {
                        assert_face_edges!(m, [eh(0), eh(1), eh(2)], fx, []);
                        assert_face_edges!(m, [eh(3), eh(4), eh(5)], fy, []);
                        assert_face_edges!(m, [eh(6), eh(7), eh(8)], fz, []);
                    });
                });
            }

            #[test]
            fn connect_two_blades_around_vertex() {
                // We start with the same mesh as in `vertex_with_three_blades` and
                // will then add a face in two different ways.
                //
                // Since the order of the three blades is not defined, we should be
                // able to connect any two blades. All these faces should be valid
                // insertions:
                // - [d, c, a]
                // - [f, c, a]
                // - [b, e, a]
                // - [b, g, a]
                // - [a, d, g]
                // - [e, a, f]
                //
                // We only check everything for the first two, but still try to
                // insert the last four just to check that nothing panics.
                //
                //       (b)-------(c)
                //         \       /
                //          \  X  /
                //           \   /
                //            \ /
                //  (g)-------(a)-------(d)
                //    \       / \       /
                //     \  Z  /   \  Y  /
                //      \   /     \   /
                //       \ /       \ /
                //       (f)       (e)
                //

                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();
                let ve = m.add_vertex();
                let vf = m.add_vertex();
                let vg = m.add_vertex();

                let fx = m.add_triangle([va, vc, vb]);
                let fy = m.add_triangle([va, ve, vd]);
                let fz = m.add_triangle([va, vg, vf]);


                // Insert [d, c, a]
                {
                    let mut m = m.clone();
                    let f = m.add_triangle([vd, vc, va]);

                    // ----- Check stuff
                    assert_faces!(m; [$($extra),*];
                        fx => [f],      [va, vc, vb], boundary;
                        fy => [f],      [va, ve, vd], boundary;
                        fz => [],       [va, vg, vf], boundary;
                        f  => [fx, fy], [vd, vc, va], boundary;
                    );

                    assert_vertices!(m; [$($extra),*];
                        va => [fx, f, fy, fz], [vb, vc, vd, ve, vf, vg], boundary;
                        vb => [fx],            [vc, va],                 boundary;
                        vc => [fx, f],         [vd, va, vb],             boundary;
                        vd => [fy, f],         [ve, va, vc],             boundary;
                        ve => [fy],            [vd, va],                 boundary;
                        vf => [fz],            [va, vg],                 boundary;
                        vg => [fz],            [vf, va],                 boundary;
                    );

                    test_helper!(@if EdgeMesh in [$($extra),*] => {
                        let eh = EdgeHandle::new;
                        assert_eq!(m.num_edges(), 10);
                        assert_eq_set!(
                            m.edge_handles(),
                            [eh(0), eh(1), eh(2), eh(3), eh(4), eh(5), eh(6), eh(7), eh(8), eh(9)],
                        );

                        test_helper!(@if EdgeAdj in [$($extra),*] => {
                            let e = assert_face_edges!(m, [eh(0), eh(1), eh(2)], fx, [f]);
                            let e_ac = e[0];

                            let e = assert_face_edges!(m, [eh(3), eh(4), eh(5)], fy, [f]);
                            let e_ad = e[0];

                            assert_face_edges!(m, [eh(6), eh(7), eh(8)], fz, []);
                            assert_face_edges!(m, [e_ac, e_ad, eh(9)], f, [fx, fy]);
                        });
                    });
                }

                // Insert [f, c, a]
                {
                    let mut m = m.clone();
                    let f = m.add_triangle([vf, vc, va]);

                    // ----- Check stuff
                    assert_faces!(m; [$($extra),*];
                        fx => [f],      [va, vc, vb], boundary;
                        fy => [],       [va, ve, vd], boundary;
                        fz => [f],      [va, vg, vf], boundary;
                        f  => [fx, fz], [vf, vc, va], boundary;
                    );

                    assert_vertices!(m; [$($extra),*];
                        va => [fx, f, fz, fy], [vb, vc, vf, vg, vd, ve], boundary;
                        vb => [fx],            [vc, va],                 boundary;
                        vc => [fx, f],         [vf, va, vb],             boundary;
                        vd => [fy],            [ve, va],                 boundary;
                        ve => [fy],            [vd, va],                 boundary;
                        vf => [fz, f],         [vg, va, vc],             boundary;
                        vg => [fz],            [vf, va],                 boundary;
                    );

                    test_helper!(@if EdgeMesh in [$($extra),*] => {
                        let eh = EdgeHandle::new;
                        assert_eq!(m.num_edges(), 10);
                        assert_eq_set!(
                            m.edge_handles(),
                            [eh(0), eh(1), eh(2), eh(3), eh(4), eh(5), eh(6), eh(7), eh(8), eh(9)],
                        );

                        test_helper!(@if EdgeAdj in [$($extra),*] => {
                            let e = assert_face_edges!(m, [eh(0), eh(1), eh(2)], fx, [f]);
                            let e_ac = e[0];

                            assert_face_edges!(m, [eh(3), eh(4), eh(5)], fy, []);

                            let e = assert_face_edges!(m, [eh(6), eh(7), eh(8)], fz, [f]);
                            let e_af = e[0];

                            assert_face_edges!(m, [e_ac, e_af, eh(9)], f, [fx, fz]);
                        });
                    });
                }

                // Try to insert the remaning faces (just make sure it doesn't
                // panic).
                m.clone().add_triangle([vb, ve, va]);
                m.clone().add_triangle([vb, vg, va]);
                m.clone().add_triangle([va, vd, vg]);
                m.clone().add_triangle([ve, va, vf]);
            }


            #[test]
            fn triforce() {
                //
                //             (a)
                //             / \
                //            /   \
                //           /  X  \
                //          /       \
                //        (b) ----- (c)
                //        / \       / \
                //       /   \  W  /   \
                //      /  Y  \   /  Z  \
                //     /       \ /       \
                //   (d) ----- (e) ----- (f)
                //
                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();
                let ve = m.add_vertex();
                let vf = m.add_vertex();

                let fx = m.add_triangle([va, vb, vc]);
                let fy = m.add_triangle([vb, vd, ve]);
                let fz = m.add_triangle([vc, ve, vf]);

                // -- check stuff with hole in the middle (real triforce)
                assert_faces!(m; [$($extra),*];
                    fx => [], [va, vb, vc], boundary;
                    fy => [], [vb, vd, ve], boundary;
                    fz => [], [vc, ve, vf], boundary;
                );

                assert_vertices!(m; [$($extra),*];
                    va => [fx],     [vb, vc],         boundary;
                    vb => [fx, fy], [va, vc, ve, vd], boundary;
                    vc => [fx, fz], [vf, ve, vb, va], boundary;
                    vd => [fy],     [vb, ve],         boundary;
                    ve => [fy, fz], [vd, vb, vc, vf], boundary;
                    vf => [fz],     [ve, vc],         boundary;
                );

                // TODO: check edges

                // -- fill hole in middle
                let fw = m.add_triangle([vb, ve, vc]);

                assert_faces!(m; [$($extra),*];
                    fx => [fw],         [va, vb, vc], boundary;
                    fy => [fw],         [vb, vd, ve], boundary;
                    fz => [fw],         [vc, ve, vf], boundary;
                    fw => [fx, fy, fz], [vb, ve, vc], interior;
                );

                assert_vertices!(m; [$($extra),*];
                    va => [fx],         [vb, vc],         boundary;
                    vb => [fx, fw, fy], [va, vc, ve, vd], boundary;
                    vc => [fz, fw, fx], [vf, ve, vb, va], boundary;
                    vd => [fy],         [vb, ve],         boundary;
                    ve => [fy, fw, fz], [vd, vb, vc, vf], boundary;
                    vf => [fz],         [ve, vc],         boundary;
                );

                // TODO: check edges
            }
        });

        test_helper!(@if_item [Manifold] in [$($extra),*] => {
            #[test]
            #[should_panic]
            fn non_manifold_triple_edge() {
                // This creates a non-manifold mesh by connecting three faces to a
                // single edge. This is never allowed by mesh data structures. So
                // we expect them to panic.
                //
                //
                //            (a)⟍
                //           / | \ ⟍
                //          /  |  \  ⟍
                //         /   |   \   ⟍
                //       (c)   |   (d)  (e)
                //         \   |   /   ⟋
                //          \  |  /  ⟋
                //           \ | / ⟋
                //            (b)⟋
                //
                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();
                let ve = m.add_vertex();

                m.add_triangle([va, vc, vb]);
                m.add_triangle([va, vb, vd]);

                // This should panic
                m.add_triangle([va, vb, ve]);
            }

            #[test]
            #[should_panic]
            fn non_manifold_add_to_closed_fan() {
                // This creates a non-manifold mesh by first creating a vertex
                // (A) that has a closed fan around itself. Then we try to add
                // another face to that vertex (the one with E and F).
                //
                //             (B)          (E)
                //            / | \       ⟋  |
                //           /  |  \    ⟋    |
                //          /  (A)  \  A -- (F)
                //         / ⟋    ⟍ \
                //        (C) ----- (D)
                //
                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();
                let ve = m.add_vertex();
                let vf = m.add_vertex();

                m.add_triangle([va, vb, vc]);
                m.add_triangle([va, vc, vd]);
                m.add_triangle([va, vd, vb]);

                // This should panic
                m.add_triangle([va, vf, ve]);
            }
        });

        // TODO: Double Sided triangle
        // TODO: Möbius strip

        // TODO: something that maybe challenges the assumption repeated
        // next() in a HEM equals a prev(). Maybe something like this?
        //
        //          (b)---(d)
        //         / |   / | \
        //      (a)  |  /  |  (a)
        //         \ | /   | /
        //          (c)---(e)
        //
        // The (a) vertex exists only once.

        // TODO: something that "only" has multi fan blades but cannot be
        // repaired into anything useful anymore. Like:
        //
        //          (b)---(d)            (f)---(h)
        //         / |   / | \          / |   / | \
        //      (a)  |  /  |  (a)    (a)  |  /  |  (a)
        //         \ | /   | /          \ | /   | /
        //          (c)---(e)            (g)---(i)
        //
        // The (a) vertex exists only once. Although... that doesn't necessarly
        // break things right? It cannot be closed without breaking stuff, but
        // it's fine as a manifold open mesh?

        // TODO: test with 4 or more fan blades

        // TODO: flip edge
        // TODO: split edge
        // TODO: split face
    };
}
