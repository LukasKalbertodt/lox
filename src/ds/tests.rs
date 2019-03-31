//! This module contains macros to generate unit tests for mesh data
//! structures.

use std::{
    collections::{BTreeSet, HashSet},
    cmp::PartialEq,
    fmt::{Debug, Write},
};

use crate::{
    prelude::*,
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
    ($mesh:ident, [$e0:expr, $e1:expr, $e2:expr] of $f:expr => [$($neighbor:expr),* $(,)*]) => {{
        let edge_faces = [
            $mesh.faces_of_edge($e0),
            $mesh.faces_of_edge($e1),
            $mesh.faces_of_edge($e2),
        ];
        crate::ds::tests::assert_face_edges_fn($f, [$e0, $e1, $e2], edge_faces, &[$($neighbor),*]);
    }}
}

/// Internal helper function for `assert_face_edges`.
pub fn assert_face_edges_fn(
    face: FaceHandle,
    edges: [EdgeHandle; 3],
    edge_faces: [DiList<FaceHandle>; 3],
    neighbors: &[FaceHandle],
) {
    let mut actual = HashSet::new();
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
}


/// Takes something slice-like and a list of elements. Asserts that the
/// elements occur in the slice exactly in the specified order (but potentially
/// shifted).
///
/// So `assert_eq_order(v, [a, b, c])` would be fine if `v` is any of this:
/// - `[a, b, c]`
/// - `[b, c, a]`
/// - `[c, a, b]`
///
/// This check is a stricter version of `assert_eq_set`. For <= 2 elements,
/// it's more or less equivalent to `assert_eq_set`.
macro_rules! assert_eq_order {
    ($list:expr, [] $(,)?) => {{
        assert_eq!($list, []);
    }};
    ($list:expr, [$a:expr $(, $tail:expr)*] $(,)?) => {{
        crate::ds::tests::assert_eq_order_fn(
            &$list[..],
            &[$a $(, $tail)*],
            stringify!($list),
        );
    }};
}

/// Helper function for macro `assert_eq_order`. Function instead of macro to
/// improve test compile times (no inlining!).
#[inline(never)]
pub fn assert_eq_order_fn<T: Debug + PartialEq + Copy>(
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

/// Generates unit tests for the mesh data structure `$name`.
///
/// In the brackets, you should specify additional traits that are implemented
/// for the mesh type. These will generate additional asserts in the tests. The
/// following traits are assumed to be implemented by every mesh type this
/// macro is invoked with:
/// - `TriMesh`
/// - `TriMeshMut`
///
/// These traits need to be specified in the brackets and will generate
/// additional asserts:
/// - `VerticesAroundFace`
/// - `FacesAroundVertex`
/// - `VerticesAroundVertex`
macro_rules! gen_tri_mesh_tests {
    ($name:ident : [$($extra:ident),*]) => {
        $(
            gen_tri_mesh_tests!(@is_valid_extra_trait $extra);
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
            let m = $name::empty();

            assert_eq!(m.num_faces(), 0);
            assert_eq!(m.num_vertices(), 0);

            assert!(m.faces().next().is_none());
            assert!(m.vertices().next().is_none());

            assert!(!m.contains_vertex(VertexHandle::new(0)));
            assert!(!m.contains_vertex(VertexHandle::new(27)));
            assert!(!m.contains_face(FaceHandle::new(0)));
            assert!(!m.contains_face(FaceHandle::new(27)));

            gen_tri_mesh_tests!(@if EdgeMesh in [$($extra),*] => {
                assert_eq!(m.num_edges(), 0);
                assert!(m.edges().next().is_none());
                assert!(!m.contains_edge(EdgeHandle::new(0)));
                assert!(!m.contains_edge(EdgeHandle::new(27)));
            });
        }

        #[test]
        fn single_vertex() {
            let mut m = $name::empty();
            let v = m.add_vertex();

            assert_eq!(m.num_faces(), 0);
            assert_eq!(m.num_vertices(), 1);

            assert!(m.faces().next().is_none());
            assert_eq_set!(m.vertex_handles(), [v]);

            assert!(m.contains_vertex(v));
            assert!(!m.contains_vertex(VertexHandle::new(v.idx().next())));

            gen_tri_mesh_tests!(@if FacesAroundVertex in [$($extra),*] => {
                assert_eq_order!(m.faces_around_vertex(v).into_vec(), []);
            });

            gen_tri_mesh_tests!(@if VerticesAroundVertex in [$($extra),*] => {
                assert_eq_order!(m.vertices_around_vertex(v).into_vec(), []);
            });

            gen_tri_mesh_tests!(@if EdgeMesh in [$($extra),*] => {
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
            let mut m = $name::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let f = m.add_triangle([va, vb, vc]);

            assert_eq!(m.num_faces(), 1);
            assert_eq!(m.num_vertices(), 3);

            assert_eq_set!(m.face_handles(), [f]);
            assert_eq_set!(m.vertex_handles(), [va, vb, vc]);

            assert!(m.contains_vertex(va));
            assert!(m.contains_vertex(vb));
            assert!(m.contains_vertex(vc));
            assert!(m.contains_face(f));
            assert!(!m.contains_face(FaceHandle::new(f.idx().next())));

            gen_tri_mesh_tests!(@if VerticesAroundFace in [$($extra),*] => {
                assert_eq_order!(m.vertices_around_face(f).into_vec(), [va, vb, vc]);

                gen_tri_mesh_tests!(@if TriMesh in [$($extra),*] => {
                    assert_eq_order!(m.vertices_around_triangle(f), [va, vb, vc]);
                });
            });

            gen_tri_mesh_tests!(@if FacesAroundVertex in [$($extra),*] => {
                assert_eq_order!(m.faces_around_vertex(va).into_vec(), [f]);
                assert_eq_order!(m.faces_around_vertex(vb).into_vec(), [f]);
                assert_eq_order!(m.faces_around_vertex(vc).into_vec(), [f]);
            });

            gen_tri_mesh_tests!(@if VerticesAroundVertex in [$($extra),*] => {
                assert_eq_order!(m.vertices_around_vertex(va).into_vec(), [vc, vb]);
                assert_eq_order!(m.vertices_around_vertex(vb).into_vec(), [va, vc]);
                assert_eq_order!(m.vertices_around_vertex(vc).into_vec(), [vb, va]);
            });

            gen_tri_mesh_tests!(@if FacesAroundFace in [$($extra),*] => {
                assert_eq_order!(m.faces_around_face(f).into_vec(), []);

                gen_tri_mesh_tests!(@if TriMesh in [$($extra),*] => {
                    assert_eq_order!(m.faces_around_triangle(f).into_vec(), []);
                });
            });

            gen_tri_mesh_tests!(@if EdgeMesh in [$($extra),*] => {
                let e0 = EdgeHandle::new(0);
                let e1 = EdgeHandle::new(1);
                let e2 = EdgeHandle::new(2);

                assert_eq!(m.num_edges(), 3);
                assert_eq_set!(m.edge_handles(), [e0, e1, e2]);
                assert!(m.contains_edge(e0));
                assert!(m.contains_edge(e1));
                assert!(m.contains_edge(e2));

                gen_tri_mesh_tests!(@if EToF in [$($extra),*] => {
                    assert_face_edges!(m, [e0, e1, e2] of f => []);
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
            let mut m = $name::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let v_top = m.add_vertex();

            let f_bottom = m.add_triangle([va, vc, vb]);
            let f_ab = m.add_triangle([va, vb, v_top]);
            let f_bc = m.add_triangle([vb, vc, v_top]);
            let f_ca = m.add_triangle([vc, va, v_top]);

            assert_eq!(m.num_faces(), 4);
            assert_eq!(m.num_vertices(), 4);

            assert_eq_set!(m.face_handles(), [f_bottom, f_ab, f_bc, f_ca]);
            assert_eq_set!(m.vertex_handles(), [va, vb, vc, v_top]);

            gen_tri_mesh_tests!(@if VerticesAroundFace in [$($extra),*] => {
                assert_eq_order!(m.vertices_around_face(f_bottom).into_vec(), [va, vc, vb]);
                assert_eq_order!(m.vertices_around_face(f_ab).into_vec(), [va, vb, v_top]);
                assert_eq_order!(m.vertices_around_face(f_bc).into_vec(), [vb, vc, v_top]);
                assert_eq_order!(m.vertices_around_face(f_ca).into_vec(), [vc, va, v_top]);

                gen_tri_mesh_tests!(@if TriMesh in [$($extra),*] => {
                    assert_eq_order!(m.vertices_around_triangle(f_bottom), [va, vc, vb]);
                    assert_eq_order!(m.vertices_around_triangle(f_ab), [va, vb, v_top]);
                    assert_eq_order!(m.vertices_around_triangle(f_bc), [vb, vc, v_top]);
                    assert_eq_order!(m.vertices_around_triangle(f_ca), [vc, va, v_top]);
                });
            });

            gen_tri_mesh_tests!(@if FacesAroundVertex in [$($extra),*] => {
                assert_eq_order!(m.faces_around_vertex(va).into_vec(), [f_bottom, f_ca, f_ab]);
                assert_eq_order!(m.faces_around_vertex(vb).into_vec(), [f_bottom, f_ab, f_bc]);
                assert_eq_order!(m.faces_around_vertex(vc).into_vec(), [f_bottom, f_bc, f_ca]);
                assert_eq_order!(m.faces_around_vertex(v_top).into_vec(), [f_ca, f_bc, f_ab]);
            });

            gen_tri_mesh_tests!(@if VerticesAroundVertex in [$($extra),*] => {
                assert_eq_order!(m.vertices_around_vertex(va).into_vec(), [v_top, vb, vc]);
                assert_eq_order!(m.vertices_around_vertex(vb).into_vec(), [v_top, vc, va]);
                assert_eq_order!(m.vertices_around_vertex(vc).into_vec(), [v_top, va, vb]);
                assert_eq_order!(m.vertices_around_vertex(v_top).into_vec(), [va, vc, vb]);
            });

            gen_tri_mesh_tests!(@if FacesAroundFace in [$($extra),*] => {
                assert_eq_order!(m.faces_around_face(f_bottom).into_vec(), [f_ca, f_bc, f_ab]);
                assert_eq_order!(m.faces_around_face(f_ab).into_vec(), [f_bc, f_ca, f_bottom]);
                assert_eq_order!(m.faces_around_face(f_bc).into_vec(), [f_ca, f_ab, f_bottom]);
                assert_eq_order!(m.faces_around_face(f_ca).into_vec(), [f_ab, f_bc, f_bottom]);

                gen_tri_mesh_tests!(@if TriMesh in [$($extra),*] => {
                    assert_eq_order!(
                        m.faces_around_triangle(f_bottom).into_vec(),
                        [f_ca, f_bc, f_ab],
                    );
                    assert_eq_order!(
                        m.faces_around_triangle(f_ab).into_vec(),
                        [f_bc, f_ca, f_bottom],
                    );
                    assert_eq_order!(
                        m.faces_around_triangle(f_bc).into_vec(),
                        [f_ca, f_ab, f_bottom],
                    );
                    assert_eq_order!(
                        m.faces_around_triangle(f_ca).into_vec(),
                        [f_ab, f_bc, f_bottom],
                    );
                });
            });

            gen_tri_mesh_tests!(@if EdgeMesh in [$($extra),*] => {
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

                gen_tri_mesh_tests!(@if EToF in [$($extra),*] => {
                    // assert_face_edges!(m, [e0, e1, e2] of f_bottom => []);
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
            let mut m = $name::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let fx = m.add_triangle([va, vb, vc]);
            // Everything is correct now, this is checked by `single_triangle`

            // ----- Add second face
            let vd = m.add_vertex();
            let fy = m.add_triangle([va, vc, vd]);

            assert_eq!(m.num_faces(), 2);
            assert_eq!(m.num_vertices(), 4);

            assert_eq_set!(m.face_handles(), [fx, fy]);
            assert_eq_set!(m.vertex_handles(), [va, vb, vc, vd]);

            gen_tri_mesh_tests!(@if VerticesAroundFace in [$($extra),*] => {
                assert_eq_order!(m.vertices_around_face(fx).into_vec(), [va, vb, vc]);
                assert_eq_order!(m.vertices_around_face(fy).into_vec(), [va, vc, vd]);

                gen_tri_mesh_tests!(@if TriMesh in [$($extra),*] => {
                    assert_eq_order!(m.vertices_around_triangle(fx), [va, vb, vc]);
                    assert_eq_order!(m.vertices_around_triangle(fy), [va, vc, vd]);
                });
            });

            gen_tri_mesh_tests!(@if FacesAroundVertex in [$($extra),*] => {
                assert_eq_order!(m.faces_around_vertex(va).into_vec(), [fy, fx]);
                assert_eq_order!(m.faces_around_vertex(vb).into_vec(), [fx]);
                assert_eq_order!(m.faces_around_vertex(vc).into_vec(), [fx, fy]);
                assert_eq_order!(m.faces_around_vertex(vd).into_vec(), [fy]);
            });

            gen_tri_mesh_tests!(@if VerticesAroundVertex in [$($extra),*] => {
                assert_eq_order!(m.vertices_around_vertex(va).into_vec(), [vd, vc, vb]);
                assert_eq_order!(m.vertices_around_vertex(vb).into_vec(), [va, vc]);
                assert_eq_order!(m.vertices_around_vertex(vc).into_vec(), [vb, va, vd]);
                assert_eq_order!(m.vertices_around_vertex(vd).into_vec(), [vc, va]);
            });

            gen_tri_mesh_tests!(@if FacesAroundFace in [$($extra),*] => {
                assert_eq_order!(m.faces_around_face(fx).into_vec(), [fy]);
                assert_eq_order!(m.faces_around_face(fy).into_vec(), [fx]);

                gen_tri_mesh_tests!(@if TriMesh in [$($extra),*] => {
                    assert_eq_order!(m.faces_around_triangle(fx).into_vec(), [fy]);
                    assert_eq_order!(m.faces_around_triangle(fy).into_vec(), [fx]);
                });
            });

            // ----- Add third face
            let ve = m.add_vertex();
            let fz = m.add_triangle([vd, vc, ve]);

            assert_eq!(m.num_faces(), 3);
            assert_eq!(m.num_vertices(), 5);

            assert_eq_set!(m.face_handles(), [fx, fy, fz]);
            assert_eq_set!(m.vertex_handles(), [va, vb, vc, vd, ve]);

            gen_tri_mesh_tests!(@if VerticesAroundFace in [$($extra),*] => {
                assert_eq_order!(m.vertices_around_face(fx).into_vec(), [va, vb, vc]);
                assert_eq_order!(m.vertices_around_face(fy).into_vec(), [va, vc, vd]);
                assert_eq_order!(m.vertices_around_face(fz).into_vec(), [vd, vc, ve]);

                gen_tri_mesh_tests!(@if TriMesh in [$($extra),*] => {
                    assert_eq_order!(m.vertices_around_triangle(fx), [va, vb, vc]);
                    assert_eq_order!(m.vertices_around_triangle(fy), [va, vc, vd]);
                    assert_eq_order!(m.vertices_around_triangle(fz), [vd, vc, ve]);
                });
            });

            gen_tri_mesh_tests!(@if FacesAroundVertex in [$($extra),*] => {
                assert_eq_order!(m.faces_around_vertex(va).into_vec(), [fy, fx]);
                assert_eq_order!(m.faces_around_vertex(vb).into_vec(), [fx]);
                assert_eq_order!(m.faces_around_vertex(vc).into_vec(), [fx, fy, fz]);
                assert_eq_order!(m.faces_around_vertex(vd).into_vec(), [fz, fy]);
                assert_eq_order!(m.faces_around_vertex(ve).into_vec(), [fz]);
            });

            gen_tri_mesh_tests!(@if VerticesAroundVertex in [$($extra),*] => {
                assert_eq_order!(m.vertices_around_vertex(va).into_vec(), [vd, vc, vb]);
                assert_eq_order!(m.vertices_around_vertex(vb).into_vec(), [va, vc]);
                assert_eq_order!(m.vertices_around_vertex(vc).into_vec(), [vb, va, vd, ve]);
                assert_eq_order!(m.vertices_around_vertex(vd).into_vec(), [ve, vc, va]);
                assert_eq_order!(m.vertices_around_vertex(ve).into_vec(), [vc, vd]);
            });

            gen_tri_mesh_tests!(@if FacesAroundFace in [$($extra),*] => {
                assert_eq_order!(m.faces_around_face(fx).into_vec(), [fy]);
                assert_eq_order!(m.faces_around_face(fy).into_vec(), [fx, fz]);
                assert_eq_order!(m.faces_around_face(fz).into_vec(), [fy]);

                gen_tri_mesh_tests!(@if TriMesh in [$($extra),*] => {
                    assert_eq_order!(m.faces_around_triangle(fx).into_vec(), [fy]);
                    assert_eq_order!(
                        *m.faces_around_triangle(fy).to_array(),
                        [Some(fx), Some(fz), None],
                    );
                    assert_eq_order!(m.faces_around_triangle(fz).into_vec(), [fy]);
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

            let mut m = $name::empty();
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
            assert_eq!(m.num_faces(), 6);
            assert_eq!(m.num_vertices(), 6);

            assert_eq_set!(m.face_handles(), [fu, fv, fw, fx, fy, fz]);
            assert_eq_set!(m.vertex_handles(), [va, vb, vc, vd, ve, vf]);

            gen_tri_mesh_tests!(@if VerticesAroundFace in [$($extra),*] => {
                assert_eq_order!(m.vertices_around_face(fu).into_vec(), [va, vc, vb]);
                assert_eq_order!(m.vertices_around_face(fv).into_vec(), [vb, vc, vd]);
                assert_eq_order!(m.vertices_around_face(fw).into_vec(), [va, vb, ve]);
                assert_eq_order!(m.vertices_around_face(fx).into_vec(), [vb, vf, ve]);
                assert_eq_order!(m.vertices_around_face(fy).into_vec(), [vc, vf, vd]);
                assert_eq_order!(m.vertices_around_face(fz).into_vec(), [vc, ve, vf]);

                gen_tri_mesh_tests!(@if FacesAroundFace in [$($extra),*] => {
                    assert_eq_order!(m.vertices_around_triangle(fu), [va, vc, vb]);
                    assert_eq_order!(m.vertices_around_triangle(fv), [vb, vc, vd]);
                    assert_eq_order!(m.vertices_around_triangle(fw), [va, vb, ve]);
                    assert_eq_order!(m.vertices_around_triangle(fx), [vb, vf, ve]);
                    assert_eq_order!(m.vertices_around_triangle(fy), [vc, vf, vd]);
                    assert_eq_order!(m.vertices_around_triangle(fz), [vc, ve, vf]);
                });
            });

            gen_tri_mesh_tests!(@if FacesAroundVertex in [$($extra),*] => {
                assert_eq_order!(m.faces_around_vertex(va).into_vec(), [fu, fw]);
                assert_eq_order!(m.faces_around_vertex(vb).into_vec(), [fw, fx, fv, fu]);
                assert_eq_order!(m.faces_around_vertex(vc).into_vec(), [fu, fv, fy, fz]);
                assert_eq_order!(m.faces_around_vertex(vd).into_vec(), [fv, fy]);
                assert_eq_order!(m.faces_around_vertex(ve).into_vec(), [fz, fx, fw]);
                assert_eq_order!(m.faces_around_vertex(vf).into_vec(), [fx, fz, fy]);
            });

            gen_tri_mesh_tests!(@if VerticesAroundVertex in [$($extra),*] => {
                assert_eq_order!(m.vertices_around_vertex(va).into_vec(), [ve, vb, vc]);
                assert_eq_order!(m.vertices_around_vertex(vb).into_vec(), [va, ve, vf, vd, vc]);
                assert_eq_order!(m.vertices_around_vertex(vc).into_vec(), [va, vb, vd, vf, ve]);
                assert_eq_order!(m.vertices_around_vertex(vd).into_vec(), [vb, vf, vc]);
                assert_eq_order!(m.vertices_around_vertex(ve).into_vec(), [vc, vf, vb, va]);
                assert_eq_order!(m.vertices_around_vertex(vf).into_vec(), [vb, ve, vc, vd]);
            });

            gen_tri_mesh_tests!(@if FacesAroundFace in [$($extra),*] => {
                assert_eq_order!(m.faces_around_face(fu).into_vec(), [fv, fw]);
                assert_eq_order!(m.faces_around_face(fv).into_vec(), [fu, fy]);
                assert_eq_order!(m.faces_around_face(fw).into_vec(), [fu, fx]);
                assert_eq_order!(m.faces_around_face(fx).into_vec(), [fz, fw]);
                assert_eq_order!(m.faces_around_face(fy).into_vec(), [fv, fz]);
                assert_eq_order!(m.faces_around_face(fz).into_vec(), [fx, fy]);

                gen_tri_mesh_tests!(@if FacesAroundFace in [$($extra),*] => {
                    assert_eq_order!(
                        *m.faces_around_triangle(fu).to_array(),
                        [Some(fv), Some(fw), None],
                    );
                    assert_eq_order!(
                        *m.faces_around_triangle(fv).to_array(),
                        [Some(fu), Some(fy), None],
                    );
                    assert_eq_order!(
                        *m.faces_around_triangle(fw).to_array(),
                        [Some(fu), Some(fx), None],
                    );
                    assert_eq_order!(
                        *m.faces_around_triangle(fx).to_array(),
                        [Some(fz), Some(fw), None],
                    );
                    assert_eq_order!(
                        *m.faces_around_triangle(fy).to_array(),
                        [Some(fv), Some(fz), None],
                    );
                    assert_eq_order!(
                        *m.faces_around_triangle(fz).to_array(),
                        [Some(fx), Some(fy), None],
                    );
                });
            });
        }

        gen_tri_mesh_tests!(@if_item SupportsMultiBlade in [$($extra),*] => {
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

                let mut m = $name::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();
                let ve = m.add_vertex();

                let fx = m.add_triangle([va, vc, vb]);
                let fy = m.add_triangle([va, vd, ve]);


                // ----- Check stuff
                assert_eq!(m.num_faces(), 2);
                assert_eq!(m.num_vertices(), 5);

                assert_eq_set!(m.face_handles(), [fx, fy]);
                assert_eq_set!(m.vertex_handles(), [va, vb, vc, vd, ve]);

                gen_tri_mesh_tests!(@if VerticesAroundFace in [$($extra),*] => {
                    assert_eq_order!(m.vertices_around_face(fx).into_vec(), [va, vc, vb]);
                    assert_eq_order!(m.vertices_around_face(fy).into_vec(), [va, vd, ve]);

                    gen_tri_mesh_tests!(@if TriMesh in [$($extra),*] => {
                        assert_eq_order!(m.vertices_around_triangle(fx), [va, vc, vb]);
                        assert_eq_order!(m.vertices_around_triangle(fy), [va, vd, ve]);
                    });
                });

                gen_tri_mesh_tests!(@if FacesAroundVertex in [$($extra),*] => {
                    assert_eq_order!(m.faces_around_vertex(va).into_vec(), [fx, fy]);
                    assert_eq_order!(m.faces_around_vertex(vb).into_vec(), [fx]);
                    assert_eq_order!(m.faces_around_vertex(vc).into_vec(), [fx]);
                    assert_eq_order!(m.faces_around_vertex(vd).into_vec(), [fy]);
                    assert_eq_order!(m.faces_around_vertex(ve).into_vec(), [fy]);
                });

                gen_tri_mesh_tests!(@if VerticesAroundVertex in [$($extra),*] => {
                    assert_eq_order!(m.vertices_around_vertex(va).into_vec(), [vb, vc, ve, vd]);
                    assert_eq_order!(m.vertices_around_vertex(vb).into_vec(), [vc, va]);
                    assert_eq_order!(m.vertices_around_vertex(vc).into_vec(), [va, vb]);
                    assert_eq_order!(m.vertices_around_vertex(vd).into_vec(), [va, ve]);
                    assert_eq_order!(m.vertices_around_vertex(ve).into_vec(), [vd, va]);
                });

                gen_tri_mesh_tests!(@if FacesAroundFace in [$($extra),*] => {
                    assert_eq_order!(m.faces_around_face(fx).into_vec(), []);
                    assert_eq_order!(m.faces_around_face(fy).into_vec(), []);

                    gen_tri_mesh_tests!(@if TriMesh in [$($extra),*] => {
                        assert_eq_order!(m.faces_around_triangle(fx).into_vec(), []);
                        assert_eq_order!(m.faces_around_triangle(fy).into_vec(), []);
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

                let mut m = $name::empty();
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
                assert_eq!(m.num_faces(), 3);
                assert_eq!(m.num_vertices(), 7);

                assert_eq_set!(m.face_handles(), [fx, fy, fz]);
                assert_eq_set!(m.vertex_handles(), [va, vb, vc, vd, ve, vf, vg]);

                gen_tri_mesh_tests!(@if VerticesAroundFace in [$($extra),*] => {
                    assert_eq_order!(m.vertices_around_face(fx).into_vec(), [va, vc, vb]);
                    assert_eq_order!(m.vertices_around_face(fy).into_vec(), [va, ve, vd]);
                    assert_eq_order!(m.vertices_around_face(fz).into_vec(), [va, vg, vf]);

                    gen_tri_mesh_tests!(@if TriMesh in [$($extra),*] => {
                        assert_eq_order!(m.vertices_around_triangle(fx), [va, vc, vb]);
                        assert_eq_order!(m.vertices_around_triangle(fy), [va, ve, vd]);
                        assert_eq_order!(m.vertices_around_triangle(fz), [va, vg, vf]);
                    });
                });

                gen_tri_mesh_tests!(@if FacesAroundVertex in [$($extra),*] => {
                    // We can't assume any order for the faces around (a).
                    assert_eq_set!(m.faces_around_vertex(va), [fx, fy, fz]);

                    assert_eq_order!(m.faces_around_vertex(vb).into_vec(), [fx]);
                    assert_eq_order!(m.faces_around_vertex(vc).into_vec(), [fx]);
                    assert_eq_order!(m.faces_around_vertex(vd).into_vec(), [fy]);
                    assert_eq_order!(m.faces_around_vertex(ve).into_vec(), [fy]);
                    assert_eq_order!(m.faces_around_vertex(vf).into_vec(), [fz]);
                    assert_eq_order!(m.faces_around_vertex(vg).into_vec(), [fz]);
                });

                gen_tri_mesh_tests!(@if VerticesAroundVertex in [$($extra),*] => {
                    assert_eq_set!(m.vertices_around_vertex(va), [vb, vc, vd, ve, vf, vg]);

                    assert_eq_order!(m.vertices_around_vertex(vb).into_vec(), [vc, va]);
                    assert_eq_order!(m.vertices_around_vertex(vc).into_vec(), [va, vb]);
                    assert_eq_order!(m.vertices_around_vertex(vd).into_vec(), [va, ve]);
                    assert_eq_order!(m.vertices_around_vertex(ve).into_vec(), [vd, va]);
                    assert_eq_order!(m.vertices_around_vertex(vf).into_vec(), [va, vg]);
                    assert_eq_order!(m.vertices_around_vertex(vg).into_vec(), [vf, va]);
                });

                gen_tri_mesh_tests!(@if FacesAroundFace in [$($extra),*] => {
                    assert_eq_order!(m.faces_around_face(fx).into_vec(), []);
                    assert_eq_order!(m.faces_around_face(fy).into_vec(), []);
                    assert_eq_order!(m.faces_around_face(fz).into_vec(), []);

                    gen_tri_mesh_tests!(@if TriMesh in [$($extra),*] => {
                        assert_eq_order!(m.faces_around_triangle(fx).into_vec(), []);
                        assert_eq_order!(m.faces_around_triangle(fy).into_vec(), []);
                        assert_eq_order!(m.faces_around_triangle(fz).into_vec(), []);
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

                let mut m = $name::empty();
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
                    assert_eq!(m.num_faces(), 4);
                    assert_eq!(m.num_vertices(), 7);

                    assert_eq_set!(m.face_handles(), [fx, fy, fz, f]);
                    assert_eq_set!(m.vertex_handles(), [va, vb, vc, vd, ve, vf, vg]);

                    gen_tri_mesh_tests!(@if VerticesAroundFace in [$($extra),*] => {
                        assert_eq_order!(m.vertices_around_face(fx).into_vec(), [va, vc, vb]);
                        assert_eq_order!(m.vertices_around_face(fy).into_vec(), [va, ve, vd]);
                        assert_eq_order!(m.vertices_around_face(fz).into_vec(), [va, vg, vf]);
                        assert_eq_order!(m.vertices_around_face(f).into_vec(), [vd, vc, va]);

                        gen_tri_mesh_tests!(@if TriMesh in [$($extra),*] => {
                            assert_eq_order!(m.vertices_around_triangle(fx), [va, vc, vb]);
                            assert_eq_order!(m.vertices_around_triangle(fy), [va, ve, vd]);
                            assert_eq_order!(m.vertices_around_triangle(fz), [va, vg, vf]);
                            assert_eq_order!(m.vertices_around_triangle(f), [vd, vc, va]);
                        });
                    });

                    gen_tri_mesh_tests!(@if FacesAroundVertex in [$($extra),*] => {
                        // Since we have only two blades again, we can assume order
                        assert_eq_order!(m.faces_around_vertex(va).into_vec(), [fx, f, fy, fz]);

                        assert_eq_order!(m.faces_around_vertex(vb).into_vec(), [fx]);
                        assert_eq_order!(m.faces_around_vertex(vc).into_vec(), [fx, f]);
                        assert_eq_order!(m.faces_around_vertex(vd).into_vec(), [fy, f]);
                        assert_eq_order!(m.faces_around_vertex(ve).into_vec(), [fy]);
                        assert_eq_order!(m.faces_around_vertex(vf).into_vec(), [fz]);
                        assert_eq_order!(m.faces_around_vertex(vg).into_vec(), [fz]);
                    });

                    gen_tri_mesh_tests!(@if VerticesAroundVertex in [$($extra),*] => {
                        assert_eq_order!(
                            m.vertices_around_vertex(va).into_vec(),
                            [vb, vc, vd, ve, vf, vg]
                        );
                        assert_eq_order!(m.vertices_around_vertex(vb).into_vec(), [vc, va]);
                        assert_eq_order!(m.vertices_around_vertex(vc).into_vec(), [vd, va, vb]);
                        assert_eq_order!(m.vertices_around_vertex(vd).into_vec(), [ve, va, vc]);
                        assert_eq_order!(m.vertices_around_vertex(ve).into_vec(), [vd, va]);
                        assert_eq_order!(m.vertices_around_vertex(vf).into_vec(), [va, vg]);
                        assert_eq_order!(m.vertices_around_vertex(vg).into_vec(), [vf, va]);
                    });

                    gen_tri_mesh_tests!(@if FacesAroundFace in [$($extra),*] => {
                        assert_eq_order!(m.faces_around_face(fx).into_vec(), [f]);
                        assert_eq_order!(m.faces_around_face(fy).into_vec(), [f]);
                        assert_eq_order!(m.faces_around_face(fz).into_vec(), []);
                        assert_eq_order!(m.faces_around_face(f).into_vec(), [fx, fy]);

                        gen_tri_mesh_tests!(@if TriMesh in [$($extra),*] => {
                            assert_eq_order!(m.faces_around_triangle(fx).into_vec(), [f]);
                            assert_eq_order!(m.faces_around_triangle(fy).into_vec(), [f]);
                            assert_eq_order!(m.faces_around_triangle(fz).into_vec(), []);
                            assert_eq_order!(
                                *m.faces_around_triangle(f).to_array(),
                                [Some(fx), Some(fy), None]
                            );
                        });
                    });
                }

                // Insert [f, c, a]
                {
                    let mut m = m.clone();
                    let f = m.add_triangle([vf, vc, va]);

                    // ----- Check stuff
                    assert_eq!(m.num_faces(), 4);
                    assert_eq!(m.num_vertices(), 7);

                    assert_eq_set!(m.face_handles(), [fx, fy, fz, f]);
                    assert_eq_set!(m.vertex_handles(), [va, vb, vc, vd, ve, vf, vg]);

                    gen_tri_mesh_tests!(@if VerticesAroundFace in [$($extra),*] => {
                        assert_eq_order!(m.vertices_around_face(fx).into_vec(), [va, vc, vb]);
                        assert_eq_order!(m.vertices_around_face(fy).into_vec(), [va, ve, vd]);
                        assert_eq_order!(m.vertices_around_face(fz).into_vec(), [va, vg, vf]);
                        assert_eq_order!(m.vertices_around_face(f).into_vec(), [vf, vc, va]);

                        gen_tri_mesh_tests!(@if TriMesh in [$($extra),*] => {
                            assert_eq_order!(m.vertices_around_triangle(fx), [va, vc, vb]);
                            assert_eq_order!(m.vertices_around_triangle(fy), [va, ve, vd]);
                            assert_eq_order!(m.vertices_around_triangle(fz), [va, vg, vf]);
                            assert_eq_order!(m.vertices_around_triangle(f), [vf, vc, va]);
                        });
                    });

                    gen_tri_mesh_tests!(@if FacesAroundVertex in [$($extra),*] => {
                        // Since we have only two blades again, we can assume order
                        assert_eq_order!(m.faces_around_vertex(va).into_vec(), [fx, f, fz, fy]);

                        assert_eq_order!(m.faces_around_vertex(vb).into_vec(), [fx]);
                        assert_eq_order!(m.faces_around_vertex(vc).into_vec(), [fx, f]);
                        assert_eq_order!(m.faces_around_vertex(vd).into_vec(), [fy]);
                        assert_eq_order!(m.faces_around_vertex(ve).into_vec(), [fy]);
                        assert_eq_order!(m.faces_around_vertex(vf).into_vec(), [fz, f]);
                        assert_eq_order!(m.faces_around_vertex(vg).into_vec(), [fz]);
                    });

                    gen_tri_mesh_tests!(@if VerticesAroundVertex in [$($extra),*] => {
                        assert_eq_order!(
                            m.vertices_around_vertex(va).into_vec(),
                            [vb, vc, vf, vg, vd, ve]
                        );
                        assert_eq_order!(m.vertices_around_vertex(vb).into_vec(), [vc, va]);
                        assert_eq_order!(m.vertices_around_vertex(vc).into_vec(), [vf, va, vb]);
                        assert_eq_order!(m.vertices_around_vertex(vd).into_vec(), [ve, va]);
                        assert_eq_order!(m.vertices_around_vertex(ve).into_vec(), [vd, va]);
                        assert_eq_order!(m.vertices_around_vertex(vf).into_vec(), [vg, va, vc]);
                        assert_eq_order!(m.vertices_around_vertex(vg).into_vec(), [vf, va]);
                    });

                    gen_tri_mesh_tests!(@if FacesAroundFace in [$($extra),*] => {
                        assert_eq_order!(m.faces_around_face(fx).into_vec(), [f]);
                        assert_eq_order!(m.faces_around_face(fy).into_vec(), []);
                        assert_eq_order!(m.faces_around_face(fz).into_vec(), [f]);
                        assert_eq_order!(m.faces_around_face(f).into_vec(), [fx, fz]);

                        gen_tri_mesh_tests!(@if TriMesh in [$($extra),*] => {
                            assert_eq_order!(m.faces_around_triangle(fx).into_vec(), [f]);
                            assert_eq_order!(m.faces_around_triangle(fy).into_vec(), []);
                            assert_eq_order!(m.faces_around_triangle(fz).into_vec(), [f]);
                            assert_eq_order!(
                                *m.faces_around_triangle(f).to_array(),
                                [Some(fx), Some(fz), None]
                            );
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
        });

        gen_tri_mesh_tests!(@if_item Manifold in [$($extra),*] => {
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
                let mut m = $name::empty();
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
                let mut m = $name::empty();
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
    };

    // These two arms are used to conditionally expand to a given body.
    //
    // If the first ident ($needle) is in list following it, these arms expand
    // to `$body`, otherwise they expand to an empty expression.
    (@if $needle:ident in [] => $body:tt) => {{
        // The needle was not found in the extra traits. To make sure there
        // wasn't a typo bug in this macro definition, we check that `$needle`
        // is a valid extra trait to begin with. We know that all idents in the
        // list are valid, because we checked it above.
        gen_tri_mesh_tests!(@is_valid_extra_trait $needle);
    }};
    (@if $needle:ident in [$head:ident $(, $tail:ident)*] => $body:tt) => {{
        macro_rules! __inner_helper {
            ($needle $needle) => { $body };
            ($needle $head) => { gen_tri_mesh_tests!(@if $needle in [$($tail),*] => $body) }
        };

        __inner_helper!($needle $head)
    }};

    // This is the same as above but for bodies which expand to items (instead
    // of expressions).
    (@if_item $needle:ident in [] => { $($body:tt)* }) => {
        // The needle was not found in the extra traits. To make sure there
        // wasn't a typo bug in this macro definition, we check that `$needle`
        // is a valid extra trait to begin with. We know that all idents in the
        // list are valid, because we checked it above.
        gen_tri_mesh_tests!(@is_valid_extra_trait $needle);
    };
    (@if_item $needle:ident in [$head:ident $(, $tail:ident)*] => { $($body:tt)* }) => {
        macro_rules! __inner_helper {
            ($needle $needle) => { $($body)* };
            ($needle $head) => {
                gen_tri_mesh_tests!(@if_item $needle in [$($tail),*] => { $($body)* });
            }
        }

        __inner_helper!($needle $head);
    };

    // These arms are used to make sure all traits passed into the macro
    // (include the ones used in the definition of the macro) are valid.
    // Otherwise it's too easy to make a typo.
    (@is_valid_extra_trait VerticesAroundFace) => {};
    (@is_valid_extra_trait FacesAroundVertex) => {};
    (@is_valid_extra_trait VerticesAroundVertex) => {};
    (@is_valid_extra_trait FacesAroundFace) => {};
    (@is_valid_extra_trait EToF) => {};
    (@is_valid_extra_trait EToV) => {};
    (@is_valid_extra_trait TriMesh) => {};
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
