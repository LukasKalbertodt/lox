#[macro_use]
pub(crate) mod util;


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
        // TODO: make sure exactly one of `TriMesh` and `PolyMesh` is
        // specified as extra trait.
        $(
            test_helper!(@is_valid_extra_trait $extra);
        )*

        gen_tri_mesh_tests!(@inner $name, [ $($extra),* ]);
    };
    (@inner $name:ty, $extras:tt) => {
        #[allow(unused_imports)]
        use crate::{
            prelude::*,
            handle::{Handle, HSizeExt},
        };

        #[test]
        fn empty() {
            let m = <$name>::empty();

            check_mesh!(m; $extras; {
                vertices: {},
                faces: {},
                edges: {},
            });
        }

        #[test]
        fn single_vertex() {
            let mut m = <$name>::empty();
            let v = m.add_vertex();

            check_mesh!(m; $extras; {
                vertices: {
                    v => [], [], boundary;
                },
                faces: {},
                edges: {},
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

            check_mesh!(m; $extras; {
                vertices: {
                    va => [f], [vc, vb], boundary;
                    vb => [f], [va, vc], boundary;
                    vc => [f], [vb, va], boundary;
                },
                faces: {
                    f => [], [va, vb, vc], boundary;
                },
                edges: {
                    va -- vb => {f}, boundary;
                    vb -- vc => {f}, boundary;
                    vc -- va => {f}, boundary;
                },
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

            check_mesh!(m; $extras; {
                vertices: {
                    va    => [f_bottom, f_ca, f_ab], [v_top, vb, vc], interior;
                    vb    => [f_bottom, f_ab, f_bc], [v_top, vc, va], interior;
                    vc    => [f_bottom, f_bc, f_ca], [v_top, va, vb], interior;
                    v_top => [f_ca, f_bc, f_ab],     [va, vc, vb],    interior;
                },
                faces: {
                    f_bottom => [f_ca, f_bc, f_ab],     [va, vc, vb],    interior;
                    f_ab     => [f_bc, f_ca, f_bottom], [va, vb, v_top], interior;
                    f_bc     => [f_ca, f_ab, f_bottom], [vb, vc, v_top], interior;
                    f_ca     => [f_ab, f_bc, f_bottom], [vc, va, v_top], interior;
                },
                edges: {
                    va -- vb    => {f_bottom, f_ab}, interior;
                    vb -- vc    => {f_bottom, f_bc}, interior;
                    vc -- va    => {f_bottom, f_ca}, interior;
                    va -- v_top => {f_ca, f_ab}, interior;
                    vb -- v_top => {f_ab, f_bc}, interior;
                    vc -- v_top => {f_bc, f_ca}, interior;
                },
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

            check_mesh!(m; $extras; {
                vertices: {
                    va => [fy, fx], [vd, vc, vb], boundary;
                    vb => [fx],     [va, vc],     boundary;
                    vc => [fx, fy], [vb, va, vd], boundary;
                    vd => [fy],     [vc, va],     boundary;
                },
                faces: {
                    fx => [fy], [va, vb, vc], boundary;
                    fy => [fx], [va, vc, vd], boundary;
                },
                edges: {
                    va -- vb => {fx},     boundary;
                    va -- vc => {fx, fy}, interior;
                    va -- vd => {fy},     boundary;
                    vb -- vc => {fx},     boundary;
                    vc -- vd => {fy},     boundary;
                },
            });

            // ----- Add third face
            let ve = m.add_vertex();
            let fz = m.add_triangle([vd, vc, ve]);


            check_mesh!(m; $extras; {
                vertices: {
                    va => [fy, fx],     [vd, vc, vb],     boundary;
                    vb => [fx],         [va, vc],         boundary;
                    vc => [fx, fy, fz], [vb, va, vd, ve], boundary;
                    vd => [fz, fy],     [ve, vc, va],     boundary;
                    ve => [fz],         [vc, vd],         boundary;
                },
                faces: {
                    fx => [fy],     [va, vb, vc], boundary;
                    fy => [fx, fz], [va, vc, vd], boundary;
                    fz => [fy],     [vd, vc, ve], boundary;
                },
                edges: {
                    va -- vb => {fx},     boundary;
                    va -- vc => {fx, fy}, interior;
                    va -- vd => {fy},     boundary;
                    vb -- vc => {fx},     boundary;
                    vc -- vd => {fy, fz}, interior;
                    vc -- ve => {fz},      boundary;
                    vd -- ve => {fz},      boundary;
                },
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
            check_mesh!(m; $extras; {
                vertices: {
                    va => [fu, fw],         [ve, vb, vc],         boundary;
                    vb => [fw, fx, fv, fu], [va, ve, vf, vd, vc], boundary;
                    vc => [fu, fv, fy, fz], [va, vb, vd, vf, ve], boundary;
                    vd => [fv, fy],         [vb, vf, vc],         boundary;
                    ve => [fz, fx, fw],     [vc, vf, vb, va],     boundary;
                    vf => [fx, fz, fy],     [vb, ve, vc, vd],     boundary;
                },
                faces: {
                    fu => [fv, fw], [va, vc, vb], boundary;
                    fv => [fu, fy], [vb, vc, vd], boundary;
                    fw => [fu, fx], [va, vb, ve], boundary;
                    fx => [fz, fw], [vb, vf, ve], boundary;
                    fy => [fv, fz], [vc, vf, vd], boundary;
                    fz => [fx, fy], [vc, ve, vf], boundary;
                },
                edges: {
                    va -- vb => {fu, fw}, interior;
                    va -- vc => {fu},     boundary;
                    va -- ve => {fw},     boundary;
                    vb -- vc => {fu, fv}, interior;
                    vb -- vd => {fv},     boundary;
                    vb -- ve => {fw, fx}, interior;
                    vb -- vf => {fx},     boundary;
                    vc -- vd => {fv, fy}, interior;
                    vc -- ve => {fz},     boundary;
                    vc -- vf => {fy, fz}, interior;
                    vd -- vf => {fy},     boundary;
                    ve -- vf => {fx, fz}, interior;
                },
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

            check_mesh!(m; $extras; {
                vertices: {
                    va => [fu, fy, fw, ft], [vb, ve, vd, vc], interior;
                    vb => [fy, fu, fx],     [ve, va, vc, vf], boundary;
                    vc => [fu, ft, fv, fx], [va, vd, vf, vb], interior;
                    vd => [ft, fw, fz, fv], [va, ve, vf, vc], interior;
                    ve => [fz, fw, fy],     [vf, vd, va, vb], boundary;
                    vf => [fx, fv, fz],     [vb, vc, vd, ve], boundary;
                },
                faces: {
                    ft => [fu, fv, fw], [va, vc, vd], interior;
                    fu => [fx, ft, fy], [vb, vc, va], interior;
                    fv => [fz, ft, fx], [vc, vf, vd], interior;
                    fw => [fy, ft, fz], [va, vd, ve], interior;
                    fx => [fv, fu],     [vb, vf, vc], boundary;
                    fy => [fu, fw],     [va, ve, vb], boundary;
                    fz => [fw, fv],     [vf, ve, vd], boundary;
                },
                edges: {
                    va -- vb => {fu, fy}, interior;
                    va -- vc => {ft, fu}, interior;
                    va -- vd => {ft, fw}, interior;
                    va -- ve => {fw, fy}, interior;
                    vb -- vc => {fu, fx}, interior;
                    vb -- ve => {fy},     boundary;
                    vb -- vf => {fx},     boundary;
                    vc -- vd => {ft, fv}, interior;
                    vc -- vf => {fv, fx}, interior;
                    vd -- ve => {fw, fz}, interior;
                    vd -- vf => {fv, fz}, interior;
                    ve -- vf => {fz},     boundary;
                },
            });
        }

        test_helper!(@if_item [TriMesh, EdgeMesh, FullAdj] in $extras => {
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

                // Obtain and check faces around `vn`
                let upper_faces = m.faces_around_vertex(vn)
                    .filter(|&fh| m.vertices_around_face(fh).any(|v| v == va))
                    .collect::<Vec<_>>();
                assert_eq!(upper_faces.len(), 2);
                let [fu0, fu1] = [upper_faces[0], upper_faces[1]];

                let lower_faces = m.faces_around_vertex(vn)
                    .filter(|&fh| m.vertices_around_face(fh).any(|v| v == vm))
                    .collect::<Vec<_>>();
                assert_eq!(lower_faces.len(), 2);
                let [fl0, fl1] = [lower_faces[0], lower_faces[1]];

                // Check returned edges
                let [re0, re1] = res.replacement_edges;
                assert!(
                    m.endpoints_of_edge(re0).contains(&va) ^ m.endpoints_of_edge(re1).contains(&va)
                );
                assert!(
                    m.endpoints_of_edge(re0).contains(&vm) ^ m.endpoints_of_edge(re1).contains(&vm)
                );


                check_mesh!(m; $extras; {
                    vertices: {
                        va => {fu0, fu1},           [vc, vn, vb],     boundary;
                        vb => {f_bc ...},           [va, vn, vm, vc], boundary;
                        vc => {f_bc ...},           [vb, vm, vn, va], boundary;
                        vm => {fl0, fl1, f_bc},     [vb, vn, vc],     interior;
                        vn => {fu0, fu1, fl0, fl1}, [va, vc, vm, vb], interior;
                    },
                    faces: {
                        f_bc => {fl0, fl1},         [vb, vc, vm], boundary;
                        fu0  => {fu1 ...; 2},       {va, vn ...}, boundary;
                        fu1  => {fu0 ...; 2},       {va, vn ...}, boundary;
                        fl0  => {fl1, f_bc ...; 3}, {vm, vn ...}, interior;
                        fl1  => {fl0, f_bc ...; 3}, {vm, vn ...}, interior;
                    },
                    edges: {
                        va -- vb => {...; 1}, boundary;
                        va -- vc => {...; 1}, boundary;
                        va -- vn => {...; 2}, interior;
                        vb -- vc => {f_bc},   boundary;
                        vb -- vm => {...; 2}, interior;
                        vb -- vn => {...; 2}, interior;
                        vc -- vm => {...; 2}, interior;
                        vc -- vn => {...; 2}, interior;
                        vm -- vn => {...; 2}, interior;
                    },
                });
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
                assert_eq!(m.num_edges(), 7);

                // Obtain and check faces around `vn`
                let split_faces = m.faces_around_vertex(vm).collect::<Vec<_>>();
                assert_eq!(split_faces.len(), 2);
                let [fx, fy] = [split_faces[0], split_faces[1]];

                // Check returned edges
                let [re0, re1] = res.replacement_edges;
                assert!(
                    m.endpoints_of_edge(re0).contains(&va) ^ m.endpoints_of_edge(re1).contains(&va)
                );
                assert!(
                    m.endpoints_of_edge(re0).contains(&vd) ^ m.endpoints_of_edge(re1).contains(&vd)
                );


                check_mesh!(m; $extras; {
                    vertices: {
                        va => no_check, [vb, vm],         boundary;
                        vb => no_check, [vc, vd, vm, va], boundary;
                        vc => [f_bdc],  [vd, vb],         boundary;
                        vd => no_check, [vm, vb, vc],     boundary;
                        vm => no_check, [va, vb, vd],     boundary;
                    },
                    faces: {
                        f_bdc => {...; 1}, [vb, vd, vc], boundary;
                        fx    => {fy ...}, {vm, vb ...}, boundary;
                        fy    => {fx ...}, {vm, vb ...}, boundary;
                    },
                    edges: {
                        va -- vb => {...; 1},       boundary;
                        va -- vm => {...; 1},       boundary;
                        vb -- vc => {f_bdc},        boundary;
                        vb -- vd => {f_bdc ...; 2}, interior;
                        vb -- vm => {fx, fy},       interior;
                        vc -- vd => {f_bdc},        boundary;
                        vd -- vm => {...; 1},       boundary;
                    },
                });
            }
        });

        #[test]
        fn split_face_isolated() {
            //          (C)                       (C)
            //         /   \                     / | \
            //        /     \        =>         /  |  \
            //       /       \                 /  (X)  \
            //      /         \               / ⟋    ⟍ \
            //    (A) ------- (B)           (A) ------- (B)

            let mut m = <$name>::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let f = m.add_triangle([va, vb, vc]);
            let vx = m.split_face(f);

            assert_eq!(m.faces().count(), 3);
            let faces = m.face_handles().collect::<Vec<_>>();
            let [f0, f1, f2] = [faces[0], faces[1], faces[2]];

            check_mesh!(m; $extras; {
                vertices: {
                    va => {...; 2},     [vc, vx, vb], boundary;
                    vb => {...; 2},     [va, vx, vc], boundary;
                    vc => {...; 2},     [vb, vx, va], boundary;
                    vx => {f0, f1, f2}, [va, vc, vb], interior;
                },
                faces: {
                    f0 => {f1, f2}, {vx ...; 3}, boundary;
                    f1 => {f0, f2}, {vx ...; 3}, boundary;
                    f2 => {f0, f1}, {vx ...; 3}, boundary;
                },
                edges: {
                    va -- vb => {...; 1}, boundary;
                    vb -- vc => {...; 1}, boundary;
                    vc -- va => {...; 1}, boundary;
                    va -- vx => {...; 2}, interior;
                    vb -- vx => {...; 2}, interior;
                    vc -- vx => {...; 2}, interior;
                },
            });
        }

        // TODO: more `split_face` tests:
        // - one adjacent face
        // - interior face
        // - non-tri face

        test_helper!(@if_item [SupportsMultiBlade] in $extras => {
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
                check_mesh!(m; $extras; {
                    vertices: {
                        va => [fx, fy], [vb, vc, ve, vd], boundary;
                        vb => [fx],     [vc, va],         boundary;
                        vc => [fx],     [va, vb],         boundary;
                        vd => [fy],     [va, ve],         boundary;
                        ve => [fy],     [vd, va],         boundary;
                    },
                    faces: {
                        fx => [], [va, vc, vb], boundary;
                        fy => [], [va, vd, ve], boundary;
                    },
                    edges: {
                        va -- vb => {fx}, boundary;
                        va -- vc => {fx}, boundary;
                        va -- vd => {fy}, boundary;
                        va -- ve => {fy}, boundary;
                        vb -- vc => {fx}, boundary;
                        vd -- ve => {fy}, boundary;
                    },
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
                check_mesh!(m; $extras; {
                    vertices: {
                        va => {fx, fy, fz}, {vb, vc, vd, ve, vf, vg}, boundary;
                        vb => [fx],         [vc, va],                 boundary;
                        vc => [fx],         [va, vb],                 boundary;
                        vd => [fy],         [va, ve],                 boundary;
                        ve => [fy],         [vd, va],                 boundary;
                        vf => [fz],         [va, vg],                 boundary;
                        vg => [fz],         [vf, va],                 boundary;
                    },
                    faces: {
                        fx => [], [va, vc, vb], boundary;
                        fy => [], [va, ve, vd], boundary;
                        fz => [], [va, vg, vf], boundary;
                    },
                    edges: {
                        va -- vb => {fx}, boundary;
                        va -- vc => {fx}, boundary;
                        va -- vd => {fy}, boundary;
                        va -- ve => {fy}, boundary;
                        va -- vf => {fz}, boundary;
                        va -- vg => {fz}, boundary;
                        vb -- vc => {fx}, boundary;
                        vd -- ve => {fy}, boundary;
                        vf -- vg => {fz}, boundary;
                    }
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
                    check_mesh!(m; $extras; {
                        vertices: {
                            va => [fx, f, fy, fz], [vb, vc, vd, ve, vf, vg], boundary;
                            vb => [fx],            [vc, va],                 boundary;
                            vc => [fx, f],         [vd, va, vb],             boundary;
                            vd => [fy, f],         [ve, va, vc],             boundary;
                            ve => [fy],            [vd, va],                 boundary;
                            vf => [fz],            [va, vg],                 boundary;
                            vg => [fz],            [vf, va],                 boundary;
                        },
                        faces: {
                            fx => [f],      [va, vc, vb], boundary;
                            fy => [f],      [va, ve, vd], boundary;
                            fz => [],       [va, vg, vf], boundary;
                            f  => [fx, fy], [vd, vc, va], boundary;
                        },
                        edges: {
                            va -- vb => {fx},    boundary;
                            va -- vc => {fx, f}, interior;
                            va -- vd => {fy, f}, interior;
                            va -- ve => {fy},    boundary;
                            va -- vf => {fz},    boundary;
                            va -- vg => {fz},    boundary;
                            vb -- vc => {fx},    boundary;
                            vc -- vd => {f},     boundary;
                            vd -- ve => {fy},    boundary;
                            vf -- vg => {fz},    boundary;
                        }
                    });
                }

                // Insert [f, c, a]
                {
                    let mut m = m.clone();
                    let f = m.add_triangle([vf, vc, va]);

                    // ----- Check stuff
                    check_mesh!(m; $extras; {
                        vertices: {
                            va => [fx, f, fz, fy], [vb, vc, vf, vg, vd, ve], boundary;
                            vb => [fx],            [vc, va],                 boundary;
                            vc => [fx, f],         [vf, va, vb],             boundary;
                            vd => [fy],            [ve, va],                 boundary;
                            ve => [fy],            [vd, va],                 boundary;
                            vf => [fz, f],         [vg, va, vc],             boundary;
                            vg => [fz],            [vf, va],                 boundary;
                        },
                        faces: {
                            fx => [f],      [va, vc, vb], boundary;
                            fy => [],       [va, ve, vd], boundary;
                            fz => [f],      [va, vg, vf], boundary;
                            f  => [fx, fz], [vf, vc, va], boundary;
                        },
                        edges: {
                            va -- vb => {fx},    boundary;
                            va -- vc => {fx, f}, interior;
                            va -- vd => {fy},    boundary;
                            va -- ve => {fy},    boundary;
                            va -- vf => {fz, f}, interior;
                            va -- vg => {fz},    boundary;
                            vb -- vc => {fx},    boundary;
                            vc -- vf => {f},     boundary;
                            vd -- ve => {fy},    boundary;
                            vf -- vg => {fz},    boundary;
                        }
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
                check_mesh!(m; $extras; {
                    vertices: {
                        va => [fx],     [vb, vc],         boundary;
                        vb => [fx, fy], [va, vc, ve, vd], boundary;
                        vc => [fx, fz], [vf, ve, vb, va], boundary;
                        vd => [fy],     [vb, ve],         boundary;
                        ve => [fy, fz], [vd, vb, vc, vf], boundary;
                        vf => [fz],     [ve, vc],         boundary;
                    },
                    faces: {
                        fx => [], [va, vb, vc], boundary;
                        fy => [], [vb, vd, ve], boundary;
                        fz => [], [vc, ve, vf], boundary;
                    },
                    edges: {
                        va -- vb => {fx}, boundary;
                        va -- vc => {fx}, boundary;
                        vb -- vc => {fx}, boundary;
                        vb -- vd => {fy}, boundary;
                        vb -- ve => {fy}, boundary;
                        vd -- ve => {fy}, boundary;
                        vc -- ve => {fz}, boundary;
                        vc -- vf => {fz}, boundary;
                        ve -- vf => {fz}, boundary;
                    },
                });

                // -- fill hole in middle
                let fw = m.add_triangle([vb, ve, vc]);

                check_mesh!(m; $extras; {
                    vertices: {
                        va => [fx],         [vb, vc],         boundary;
                        vb => [fx, fw, fy], [va, vc, ve, vd], boundary;
                        vc => [fz, fw, fx], [vf, ve, vb, va], boundary;
                        vd => [fy],         [vb, ve],         boundary;
                        ve => [fy, fw, fz], [vd, vb, vc, vf], boundary;
                        vf => [fz],         [ve, vc],         boundary;
                    },
                    faces: {
                        fx => [fw],         [va, vb, vc], boundary;
                        fy => [fw],         [vb, vd, ve], boundary;
                        fz => [fw],         [vc, ve, vf], boundary;
                        fw => [fx, fy, fz], [vb, ve, vc], interior;
                    },
                    edges: {
                        va -- vb => {fx},     boundary;
                        va -- vc => {fx},     boundary;
                        vb -- vc => {fx, fw}, interior;
                        vb -- vd => {fy},     boundary;
                        vb -- ve => {fy, fw}, interior;
                        vd -- ve => {fy},     boundary;
                        vc -- ve => {fz, fw}, interior;
                        vc -- vf => {fz},     boundary;
                        ve -- vf => {fz},     boundary;
                    },
                });
            }
        });

        test_helper!(@if_item [Manifold] in $extras => {
            #[test]
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

                assert_panic!(m.add_triangle([va, vb, ve]));
            }

            #[test]
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

                assert_panic!(m.add_triangle([va, vf, ve]));
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
