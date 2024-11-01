#[macro_use]
pub(crate) mod util;


// ===============================================================================================
// ===== The main macro containing the test suite
// ===============================================================================================

/// Generates unit tests for the mesh data structure `$name`.
///
/// In the brackets, you should specify additional traits that are implemented
/// for the mesh type. These will generate additional unit tests and additional
/// checks in all tests. The following traits are assumed to be implemented by
/// every mesh type this macro is invoked with:
/// - `TriMesh`
/// - `TriMeshMut`
///
/// For a list of traits you can specify here, check the `test_helper` macro
/// definition.
macro_rules! gen_mesh_tests {
    // Entry point: here we just do some sanity checks on the specified extra
    // traits.
    ($name:ty : [$($extra:ident),*]) => {
        $(
            test_helper!(@is_valid_extra_trait $extra);
        )*
        test_helper!(@if_item [TriMesh, PolyMesh] in [$($extra),*] => {
            compile_error!(
                "`TriMesh` and `PolyMesh` given to `gen_mesh_tests`! \
                    Those are mutually exclusive."
            );
        });

        gen_mesh_tests!(@inner $name, [ $($extra),* ]);
    };

    // The main part. It's an extra macro arm in order to treat `$extras` as
    // single tt which is a bit more convenient.
    (@inner $name:ty, $extras:tt) => {
        #[allow(unused_imports)]
        use crate::{
            prelude::*,
            util::HSizeExt,
            test_utils::cmp_rotated,
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
        fn two_triangles() {
            //
            //         (C) ----- (D)
            //        /   \  Y  /
            //       /  X  \   /
            //      /       \ /
            //    (A) ----- (B)
            //
            let mut m = <$name>::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let fx = m.add_triangle([va, vb, vc]);

            let vd = m.add_vertex();
            let fy = m.add_triangle([vb, vd, vc]);

            check_mesh!(m; $extras; {
                vertices: {
                    va => [fx],     [vc, vb],     boundary;
                    vb => [fx, fy], [va, vc, vd], boundary;
                    vc => [fx, fy], [vb, va, vd], boundary;
                    vd => [fy],     [vb, vc],     boundary;
                },
                faces: {
                    fx => [fy], [va, vb, vc], boundary;
                    fy => [fx], [vb, vd, vc], boundary;
                },
                edges: {
                    va -- vb => {fx},     boundary;
                    va -- vc => {fx},     boundary;
                    vb -- vc => {fx, fy}, interior;
                    vb -- vd => {fy},     boundary;
                    vc -- vd => {fy},     boundary;
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

        #[test]
        fn split_face_one_adjacent_face() {
            //          (C) ------ (D)             (C) ------ (D)
            //         /   \       /              / | \       /
            //        /     \  K  /   =>         /  |  \  K  /
            //       /   J   \   /              /  (X)  \   /
            //      /         \ /              / ⟋    ⟍ \ /
            //    (A) ------- (B)           (A) -------- (B)

            let mut m = <$name>::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let vd = m.add_vertex();
            let fj = m.add_triangle([va, vb, vc]);
            let fk = m.add_triangle([vd, vc, vb]);

            let vx = m.split_face(fj);

            let faces = m.face_handles().filter(|fh| *fh != fk).collect::<Vec<_>>();
            let [f0, f1, f2] = [faces[0], faces[1], faces[2]];

            check_mesh!(m; $extras; {
                vertices: {
                    va => {...; 2},     [vc, vx, vb],     boundary;
                    vb => {fk ...; 3},  [va, vx, vc, vd], boundary;
                    vc => {fk ...; 3},  [vb, vx, va, vd], boundary;
                    vd => {fk},         [vb, vc],         boundary;
                    vx => {f0, f1, f2}, [va, vc, vb],     interior;
                },
                faces: {
                    f0 => {f1, f2 ...}, {vx ...; 3},  no_check;
                    f1 => {f0, f2 ...}, {vx ...; 3},  no_check;
                    f2 => {f0, f1 ...}, {vx ...; 3},  no_check;
                    fk => {...; 1},     [vb, vd, vc], boundary;
                },
                edges: {
                    va -- vb => {...; 1},    boundary;
                    va -- vx => {...; 2},    interior;
                    vb -- vc => {fk ...; 2}, interior;
                    vb -- vd => {fk},        boundary;
                    vb -- vx => {...; 2},    interior;
                    vc -- va => {...; 1},    boundary;
                    vc -- vd => {fk},        boundary;
                    vc -- vx => {...; 2},    interior;
                },
            });
        }

        #[test]
        fn split_face_one_different_adjacent_face() {
            // This is like the other test, but the additional face K is in a
            // different position. This is important as most `split_face`
            // implementations will reuse the original face for one of the side
            // faces.
            //
            //   (D) ------ (C)              (D) ------ (C)
            //     \       /   \               \       / | \
            //      \  K  /     \      =>       \  K  /  |  \
            //       \   /   J   \               \   /  (X)  \
            //        \ /         \               \ / ⟋    ⟍ \
            //        (A) ------- (B)             (A) ------- (B)

            let mut m = <$name>::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let vd = m.add_vertex();
            let fj = m.add_triangle([va, vb, vc]);
            let fk = m.add_triangle([vd, va, vc]);

            let vx = m.split_face(fj);

            let faces = m.face_handles().filter(|fh| *fh != fk).collect::<Vec<_>>();
            let [f0, f1, f2] = [faces[0], faces[1], faces[2]];

            check_mesh!(m; $extras; {
                vertices: {
                    va => {fk ...; 3},  [vc, vx, vb, vd], boundary;
                    vb => {...; 2},     [va, vx, vc],     boundary;
                    vc => {fk ...; 3},  [vb, vx, va, vd], boundary;
                    vd => {fk},         [vc, va],         boundary;
                    vx => {f0, f1, f2}, [va, vc, vb],     interior;
                },
                faces: {
                    f0 => {f1, f2 ...}, {vx ...; 3},  no_check;
                    f1 => {f0, f2 ...}, {vx ...; 3},  no_check;
                    f2 => {f0, f1 ...}, {vx ...; 3},  no_check;
                    fk => {...; 1},     [va, vc, vd], boundary;
                },
                edges: {
                    va -- vb => {...; 1},    boundary;
                    va -- vd => {fk},        boundary;
                    va -- vx => {...; 2},    interior;
                    vb -- vc => {...; 1},    boundary;
                    vb -- vx => {...; 2},    interior;
                    vc -- va => {fk ...; 2}, interior;
                    vc -- vd => {fk},        boundary;
                    vc -- vx => {...; 2},    interior;
                },
            });
        }

        #[test]
        fn split_face_interior() {
            //
            //             (a)                         (a)
            //             / \                         / \
            //            /   \                       /   \
            //           /  X  \                     /  X  \
            //          /       \                   /       \
            //        (b) ----- (c)      ==>      (b) ----- (c)
            //        / \       / \               / \ ⟍  ⟋ / \
            //       /   \  W  /   \             /   \ (m) /   \
            //      /  Y  \   /  Z  \           /  Y  \ | /  Z  \
            //     /       \ /       \         /       \|/       \
            //   (d) ----- (e) ----- (f)     (d) ----- (e) ----- (f)
            //
            let mut m = <$name>::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let vd = m.add_vertex();
            let ve = m.add_vertex();
            let vf = m.add_vertex();

            let fw = m.add_triangle([vb, ve, vc]);
            let fx = m.add_triangle([va, vb, vc]);
            let fy = m.add_triangle([vb, vd, ve]);
            let fz = m.add_triangle([vc, ve, vf]);

            let vm = m.split_face(fw);

            let faces = m.face_handles()
                .filter(|&fh| fh != fx && fh != fy && fh != fz)
                .collect::<Vec<_>>();
            let [f0, f1, f2] = [faces[0], faces[1], faces[2]];

            check_mesh!(m; $extras; {
                vertices: {
                   va => [fx],            [vb, vc],             boundary;
                   vb => {fx, fy ...; 4}, [va, vc, vm, ve, vd], boundary;
                   vc => {fz, fx ...; 4}, [vf, ve, vm, vb, va], boundary;
                   vd => [fy],            [vb, ve],             boundary;
                   ve => {fy, fz ...; 4}, [vd, vb, vm, vc, vf], boundary;
                   vf => [fz],            [ve, vc],             boundary;
                   vm => {f0, f1, f2},    [vb, vc, ve],         interior;
               },
               faces: {
                   fx => {...; 1},        [va, vb, vc], boundary;
                   fy => {...; 1},        [vb, vd, ve], boundary;
                   fz => {...; 1},        [vc, ve, vf], boundary;
                   f0 => {f1, f2 ...; 3}, {vm ...; 3},  interior;
                   f1 => {f2, f0 ...; 3}, {vm ...; 3},  interior;
                   f2 => {f0, f1 ...; 3}, {vm ...; 3},  interior;
               },
               edges: {
                   va -- vb => {fx},        boundary;
                   va -- vc => {fx},        boundary;
                   vb -- vc => {fx ...; 2}, interior;
                   vb -- vd => {fy},        boundary;
                   vb -- ve => {fy ...; 2}, interior;
                   vd -- ve => {fy},        boundary;
                   vc -- ve => {fz ...; 2}, interior;
                   vc -- vf => {fz},        boundary;
                   ve -- vf => {fz},        boundary;
                   vm -- vb => {...; 2},    interior;
                   vm -- vc => {...; 2},    interior;
                   vm -- ve => {...; 2},    interior;
               },
            });
        }

        #[test]
        fn panic_on_invalid_vertex() {
            let mut m = <$name>::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let invalid = VertexHandle::new(
                [va, vb, vc].iter().map(|h| h.idx()).max().unwrap() + 1
            );

            let mut clone = m.clone();
            assert_panic!(clone.add_triangle([va, vb, invalid]));

            let mut clone = m.clone();
            assert_panic!(clone.add_triangle([va, invalid, vb]));

            let mut clone = m.clone();
            assert_panic!(clone.add_triangle([invalid, va, vb]));
        }

        #[test]
        fn panic_on_invalid_split_face() {
            let mut m = <$name>::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let f = m.add_triangle([va, vb, vc]);

            let invalid = FaceHandle::new(f.idx() + 1);
            assert_panic!(m.split_face(invalid));
        }

        #[test]
        fn remove_single_triangle() {
            let mut m = <$name>::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let f = m.add_triangle([va, vb, vc]);
            m.remove_face(f);

            check_mesh!(m; $extras; {
                vertices: {
                    va => [], [], boundary;
                    vb => [], [], boundary;
                    vc => [], [], boundary;
                },
                faces: {},
                edges: {},
            });
        }

        #[test]
        fn remove_one_of_two_triangles() {
            //
            //         (a) ----- (b)               (a) ----- (b)
            //        /   \  Y  /                     \  Y  /
            //       /  X  \   /        =>             \   /
            //      /       \ /                         \ /
            //    (c) ----- (d)               (c)       (d)
            //
            // We repeat the test three times with only one difference: the
            // order of vertices for `fx`. This might be important for
            // `remove_face`.
            for shift in 0..3 {
                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();

                let mut vertices = [va, vc, vd];
                vertices.rotate_left(shift);
                let fx = m.add_triangle(vertices);
                let fy = m.add_triangle([va, vd, vb]);

                m.remove_face(fx);

                check_mesh!(m; $extras; {
                    vertices: {
                        va => [fy], [vb, vd], boundary;
                        vb => [fy], [va, vd], boundary;
                        vc => [],   [],       boundary;
                        vd => [fy], [va, vb], boundary;
                    },
                    faces: {
                        fy => [], [va, vd, vb], boundary;
                    },
                    edges: {
                        va -- vb => {fy}, boundary;
                        va -- vd => {fy}, boundary;
                        vb -- vd => {fy}, boundary;
                    },
                });
            }
        }

        #[test]
        fn remove_interior_face() {
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

            m.remove_face(f_bottom);

            check_mesh!(m; $extras; {
                vertices: {
                    va    => [f_ca, f_ab],       [v_top, vb, vc], boundary;
                    vb    => [f_ab, f_bc],       [v_top, vc, va], boundary;
                    vc    => [f_bc, f_ca],       [v_top, va, vb], boundary;
                    v_top => [f_ca, f_bc, f_ab], [va, vc, vb],    interior;
                },
                faces: {
                    f_ab => [f_bc, f_ca], [va, vb, v_top], boundary;
                    f_bc => [f_ca, f_ab], [vb, vc, v_top], boundary;
                    f_ca => [f_ab, f_bc], [vc, va, v_top], boundary;
                },
                edges: {
                    va -- vb    => {f_ab},       boundary;
                    vb -- vc    => {f_bc},       boundary;
                    vc -- va    => {f_ca},       boundary;
                    va -- v_top => {f_ca, f_ab}, interior;
                    vb -- v_top => {f_ab, f_bc}, interior;
                    vc -- v_top => {f_bc, f_ca}, interior;
                },
            });
        }

        #[test]
        fn remove_tetrahedron_face_by_face() {
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

            m.remove_face(f_bottom);
            // This state is checked by `remove_interior_face`

            m.remove_face(f_bc);
            check_mesh!(m; $extras; {
                vertices: {
                    va    => [f_ca, f_ab], [v_top, vb, vc], boundary;
                    vb    => [f_ab],       [v_top, va],     boundary;
                    vc    => [f_ca],       [v_top, va],     boundary;
                    v_top => [f_ca, f_ab], [va, vc, vb],    boundary;
                },
                faces: {
                    f_ab => [f_ca], [va, vb, v_top], boundary;
                    f_ca => [f_ab], [vc, va, v_top], boundary;
                },
                edges: {
                    va -- vb    => {f_ab},       boundary;
                    vc -- va    => {f_ca},       boundary;
                    va -- v_top => {f_ca, f_ab}, interior;
                    vb -- v_top => {f_ab},       boundary;
                    vc -- v_top => {f_ca},       boundary;
                },
            });

            m.remove_face(f_ca);
            check_mesh!(m; $extras; {
                vertices: {
                    va    => [f_ab], [v_top, vb], boundary;
                    vb    => [f_ab], [v_top, va], boundary;
                    vc    => [],     [],          boundary;
                    v_top => [f_ab], [va, vb],    boundary;
                },
                faces: {
                    f_ab => [], [va, vb, v_top], boundary;
                },
                edges: {
                    va -- vb    => {f_ab}, boundary;
                    va -- v_top => {f_ab}, boundary;
                    vb -- v_top => {f_ab}, boundary;
                },
            });

            m.remove_face(f_ab);
            check_mesh!(m; $extras; {
                vertices: {
                    va    => [], [], boundary;
                    vb    => [], [], boundary;
                    vc    => [], [], boundary;
                    v_top => [], [], boundary;
                },
                faces: {},
                edges: {},
            });
        }

        #[test]
        fn remove_isolated_vertices() {
            //
            //  (e)      (a) ----- (b)        (e)      (a) ----- (b)
            //          /   \  Y  /                       \  Y  /
            //         /  X  \   /        =>               \   /
            //        /       \ /                           \ /
            //      (c) ----- (d)                 (c)       (d)
            //
            let mut m = <$name>::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let vd = m.add_vertex();
            let ve = m.add_vertex();

            let fx = m.add_triangle([va, vc, vd]);
            let fy = m.add_triangle([va, vd, vb]);

            // First remove only `ve`
            m.remove_isolated_vertex(ve);

            check_mesh!(m; $extras; {
                vertices: {
                    va => [fy, fx], [vb, vd, vc], boundary;
                    vb => [fy],     [va, vd],     boundary;
                    vc => [fx],     [va, vd],     boundary;
                    vd => [fy, fx], [vc, va, vb], boundary;
                },
                faces: {
                    fx => [fy], [va, vc, vd], boundary;
                    fy => [fx], [va, vd, vb], boundary;
                },
                edges: {
                    va -- vb => {fy},     boundary;
                    va -- vc => {fx},     boundary;
                    va -- vd => {fy, fx}, interior;
                    vb -- vd => {fy},     boundary;
                    vc -- vd => {fx},     boundary;
                },
            });

            // Now remove face `fx` to make `vc` isolated.
            m.remove_face(fx);
            m.remove_isolated_vertex(vc);

            check_mesh!(m; $extras; {
                vertices: {
                    va => [fy], [vb, vd], boundary;
                    vb => [fy], [va, vd], boundary;
                    vd => [fy], [va, vb], boundary;
                },
                faces: {
                    fy => [], [va, vd, vb], boundary;
                },
                edges: {
                    va -- vb => {fy}, boundary;
                    va -- vd => {fy}, boundary;
                    vb -- vd => {fy}, boundary;
                },
            });
        }

        test_helper!(@if_item [PolyMesh] in $extras => {
            #[test]
            fn square() {
                //
                //     (a) ----- (b)
                //      |         |
                //      |    F    |
                //      |         |
                //     (c) ----- (d)
                //
                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();
                let f = m.add_face(&[va, vc, vd, vb]);

                check_mesh!(m; $extras; {
                    vertices: {
                        va => [f], [vb, vc], boundary;
                        vb => [f], [va, vd], boundary;
                        vc => [f], [va, vd], boundary;
                        vd => [f], [vb, vc], boundary;
                    },
                    faces: {
                        f => [], [va, vc, vd, vb], boundary;
                    },
                    edges: {
                        va -- vb => [f], boundary;
                        va -- vc => [f], boundary;
                        vb -- vd => [f], boundary;
                        vc -- vd => [f], boundary;
                    },
                });
            }

            #[test]
            fn honeycomb() {
                //
                //                (a)
                //              ⟋    ⟍
                //           (b)       (c)
                //            |    X    |
                //           (d)       (e)
                //         ⟋    ⟍   ⟋    ⟍
                //      (f)       (g)       (h)
                //       |    Y    |    Z    |
                //      (i)       (j)       (k)
                //         ⟍    ⟋   ⟍    ⟋
                //           (l) - - - (m)
                //
                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();
                let ve = m.add_vertex();
                let vf = m.add_vertex();
                let vg = m.add_vertex();
                let vh = m.add_vertex();
                let vi = m.add_vertex();
                let vj = m.add_vertex();
                let vk = m.add_vertex();
                let vl = m.add_vertex();
                let vm = m.add_vertex();

                let fx = m.add_face(&[va, vb, vd, vg, ve, vc]);
                let fy = m.add_face(&[vd, vf, vi, vl, vj, vg]);
                let fz = m.add_face(&[ve, vg, vj, vm, vk, vh]);

                check_mesh!(m; $extras; {
                    vertices: {
                        va => [fx],         [vb, vc],     boundary;
                        vb => [fx],         [va, vd],     boundary;
                        vc => [fx],         [va, ve],     boundary;
                        vd => [fx, fy],     [vb, vg, vf], boundary;
                        ve => [fx, fz],     [vc, vh, vg], boundary;
                        vf => [fy],         [vd, vi],     boundary;
                        vg => [fx, fz, fy], [vd, ve, vj], interior;
                        vh => [fz],         [ve, vk],     boundary;
                        vi => [fy],         [vf, vl],     boundary;
                        vj => [fy, fz],     [vg, vm, vl], boundary;
                        vk => [fz],         [vh, vm],     boundary;
                        vl => [fy],         [vi, vj],     boundary;
                        vm => [fz],         [vj, vk],     boundary;
                    },
                    faces: {
                        fx => [fy, fz], [va, vb, vd, vg, ve, vc], boundary;
                        fy => [fx, fz], [vd, vf, vi, vl, vj, vg], boundary;
                        fz => [fx, fy], [ve, vg, vj, vm, vk, vh], boundary;
                    },
                    edges: {
                        va -- vb => [fx],     boundary;
                        va -- vc => [fx],     boundary;
                        vb -- vd => [fx],     boundary;
                        vc -- ve => [fx],     boundary;
                        vd -- vf => [fy],     boundary;
                        vd -- vg => [fx, fy], interior;
                        ve -- vg => [fx, fz], interior;
                        ve -- vh => [fz],     boundary;
                        vf -- vi => [fy],     boundary;
                        vg -- vj => [fy, fz], interior;
                        vh -- vk => [fz],     boundary;
                        vi -- vl => [fy],     boundary;
                        vj -- vl => [fy],     boundary;
                        vj -- vm => [fz],     boundary;
                        vk -- vm => [fz],     boundary;
                    },
                });

                let fw = m.add_face(&[vj, vl, vm]);

                check_mesh!(m; $extras; {
                    vertices: {
                        va => [fx],         [vb, vc],     boundary;
                        vb => [fx],         [va, vd],     boundary;
                        vc => [fx],         [va, ve],     boundary;
                        vd => [fx, fy],     [vb, vg, vf], boundary;
                        ve => [fx, fz],     [vc, vh, vg], boundary;
                        vf => [fy],         [vd, vi],     boundary;
                        vg => [fx, fz, fy], [vd, ve, vj], interior;
                        vh => [fz],         [ve, vk],     boundary;
                        vi => [fy],         [vf, vl],     boundary;
                        vj => [fy, fz, fw], [vg, vm, vl], interior;
                        vk => [fz],         [vh, vm],     boundary;
                        vl => [fy, fw],     [vi, vj, vm], boundary;
                        vm => [fz, fw],     [vj, vk, vl], boundary;
                    },
                    faces: {
                        fx => [fy, fz],     [va, vb, vd, vg, ve, vc], boundary;
                        fy => [fw, fz, fx], [vd, vf, vi, vl, vj, vg], boundary;
                        fz => [fx, fy, fw], [ve, vg, vj, vm, vk, vh], boundary;
                        fw => [fy, fz],     [vj, vl, vm], boundary;
                    },
                    edges: {
                        va -- vb => [fx],     boundary;
                        va -- vc => [fx],     boundary;
                        vb -- vd => [fx],     boundary;
                        vc -- ve => [fx],     boundary;
                        vd -- vf => [fy],     boundary;
                        vd -- vg => [fx, fy], interior;
                        ve -- vg => [fx, fz], interior;
                        ve -- vh => [fz],     boundary;
                        vf -- vi => [fy],     boundary;
                        vg -- vj => [fy, fz], interior;
                        vh -- vk => [fz],     boundary;
                        vi -- vl => [fy],     boundary;
                        vj -- vl => [fy, fw], interior;
                        vj -- vm => [fz, fw], interior;
                        vk -- vm => [fz],     boundary;
                        vl -- vm => [fw],     boundary;
                    },
                });
            }

            #[test]
            fn cube() {
                //
                //               (a) ----- (b)
                //                |         |
                //                |    W    |
                //                |         |
                //     (a) ----- (c) ----- (d) ----- (b) ----- (a)
                //      |         |         |         |         |
                //      |    X    |    B    |    Y    |    T    |
                //      |         |         |         |         |
                //     (e) ----- (f) ----- (g) ----- (h) ----- (e)
                //                |         |
                //                |    Z    |
                //                |         |
                //               (e) ----- (h)
                //
                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();
                let ve = m.add_vertex();
                let vf = m.add_vertex();
                let vg = m.add_vertex();
                let vh = m.add_vertex();

                let fb = m.add_face(&[vc, vf, vg, vd]);
                let fw = m.add_face(&[va, vc, vd, vb]);
                let fx = m.add_face(&[va, ve, vf, vc]);
                let fy = m.add_face(&[vd, vg, vh, vb]);
                let fz = m.add_face(&[vf, ve, vh, vg]);

                // First without top face
                check_mesh!(m; $extras; {
                    vertices: {
                        va => [fw, fx],     [vb, vc, ve], boundary;
                        vb => [fy, fw],     [va, vh, vd], boundary;
                        vc => [fb, fx, fw], [va, vd, vf], interior;
                        vd => [fb, fw, fy], [vb, vg, vc], interior;
                        ve => [fx, fz],     [va, vf, vh], boundary;
                        vf => [fb, fz, fx], [vc, vg, ve], interior;
                        vg => [fb, fy, fz], [vh, vf, vd], interior;
                        vh => [fz, fy],     [vb, ve, vg], boundary;
                    },
                    faces: {
                        fb => [fw, fx, fz, fy], [vc, vf, vg, vd], interior;
                        fw => [fx, fb, fy],     [va, vc, vd, vb], boundary;
                        fx => [fz, fb, fw],     [va, ve, vf, vc], boundary;
                        fy => [fb, fz, fw],     [vd, vg, vh, vb], boundary;
                        fz => [fx, fy, fb],     [vf, ve, vh, vg], boundary;
                    },
                    edges: {
                        va -- vb => [fw],     boundary;
                        va -- vc => [fw, fx], interior;
                        va -- ve => [fx],     boundary;
                        vb -- vd => [fw, fy], interior;
                        vb -- vh => [fy],     boundary;
                        vc -- vd => [fw, fb], interior;
                        vc -- vf => [fx, fb], interior;
                        vd -- vg => [fb, fy], interior;
                        ve -- vf => [fx, fz], interior;
                        ve -- vh => [fz],     boundary;
                        vf -- vg => [fb, fz], interior;
                        vg -- vh => [fz, fy], interior;
                    },
                });

                // Add top face
                let ft = m.add_face(&[vb, vh, ve, va]);

                check_mesh!(m; $extras; {
                    vertices: {
                        va => [ft, fw, fx], [vb, vc, ve], interior;
                        vb => [ft, fy, fw], [va, vh, vd], interior;
                        vc => [fb, fx, fw], [va, vd, vf], interior;
                        vd => [fb, fw, fy], [vb, vg, vc], interior;
                        ve => [fx, fz, ft], [va, vf, vh], interior;
                        vf => [fb, fz, fx], [vc, vg, ve], interior;
                        vg => [fb, fy, fz], [vh, vf, vd], interior;
                        vh => [ft, fz, fy], [vb, ve, vg], interior;
                    },
                    faces: {
                        fb => [fw, fx, fz, fy], [vc, vf, vg, vd], interior;
                        fw => [fx, fb, fy, ft], [va, vc, vd, vb], interior;
                        fx => [ft, fz, fb, fw], [va, ve, vf, vc], interior;
                        fy => [fb, fz, ft, fw], [vd, vg, vh, vb], interior;
                        fz => [fx, ft, fy, fb], [vf, ve, vh, vg], interior;
                        ft => [fy, fz, fx, fw], [vb, vh, ve, va], interior;
                    },
                    edges: {
                        va -- vb => [fw, ft], interior;
                        va -- vc => [fw, fx], interior;
                        va -- ve => [fx, ft], interior;
                        vb -- vd => [fw, fy], interior;
                        vb -- vh => [fy, ft], interior;
                        vc -- vd => [fw, fb], interior;
                        vc -- vf => [fx, fb], interior;
                        vd -- vg => [fb, fy], interior;
                        ve -- vf => [fx, fz], interior;
                        ve -- vh => [fz, ft], interior;
                        vf -- vg => [fb, fz], interior;
                        vg -- vh => [fz, fy], interior;
                    },
                });
            }

            #[test]
            fn huge_face() {
                //
                //         (a)
                //         / \
                //        /   \
                //       /  X  \
                //     (b) --- (c)
                //   ⟋            ⟍
                //  ...     Y     ...
                //
                const VALENCE: usize = 200;

                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();

                let mut face_vertices = vec![vc, vb];
                for _ in 0..VALENCE - 2 {
                    face_vertices.push(m.add_vertex());
                }

                let fx = m.add_face(&[va, vb, vc]);
                let fy = m.add_face(&face_vertices);


                test_helper!(@if BasicAdj in $extras => {
                    let actual = m.vertices_around_face(fy).collect::<Vec<_>>();
                    if let Err(rotated) = cmp_rotated(&actual, &face_vertices) {
                        panic!(
                            "wrong neighbors returned by `m.vertices_around_face(fy)` \
                                (order respecting comparison)\n\
                                | expected: {:?} (original: {:?})\n\
                                |   actual: {:?}\n",
                            rotated,
                            face_vertices,
                            actual,
                        );
                    }
                });

                check_mesh!(m; $extras; {
                    vertices: {
                        va => [fx],     [vb, vc],        boundary;
                        vb => [fx, fy], {va, vc ...; 3}, boundary;
                        vc => [fx, fy], {va, vb ...; 3}, boundary;
                        ...; VALENCE + 1
                    },
                    faces: {
                        fx => [fy], [va, vb, vc],          boundary;
                        fy => [fx], {vb, vc ...; VALENCE}, boundary;
                    },
                    edges: {
                        va -- vb => [fx],     boundary;
                        va -- vc => [fx],     boundary;
                        vb -- vc => [fx, fy], interior;
                        ...; VALENCE + 2
                    },
                });
            }

            #[test]
            fn many_different_face_valences() {
                //
                //              (a) ---- (b) ---- (c) ---- (d)
                //            ⟋ |        |         |        |
                //          ⟋   |   V    |         |        |
                //        ⟋  U  |        |         |        |
                //     (e) ---- (f) ---- (g)      (h)  X   (i)
                //      |               ⟋    Y     |        |
                //      |      W      ⟋            |        |
                //      |           ⟋              |        |
                //     (j) ---- (k) ---- (l) ---- (m) ---- (n)
                //
                // Plus a face Z connecting the whole boundary.
                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();
                let ve = m.add_vertex();
                let vf = m.add_vertex();
                let vg = m.add_vertex();
                let vh = m.add_vertex();
                let vi = m.add_vertex();
                let vj = m.add_vertex();
                let vk = m.add_vertex();
                let vl = m.add_vertex();
                let vm = m.add_vertex();
                let vn = m.add_vertex();

                let fu = m.add_triangle([va, ve, vf]);
                let fv = m.add_face(&[va, vf, vg, vb]);
                let fw = m.add_face(&[ve, vj, vk, vg, vf]);
                let fx = m.add_face(&[vc, vh, vm, vn, vi, vd]);
                let fy = m.add_face(&[vb, vg, vk, vl, vm, vh, vc]);
                let fz = m.add_face(&[va, vb, vc, vd, vi, vn, vm, vl, vk, vj, ve]);

                check_mesh!(m; $extras; {
                    vertices: {
                        va => [fv, fu, fz], [vb, vf, ve], interior;
                        vb => [fy, fv, fz], [vc, vg, va], interior;
                        vc => [fx, fy, fz], [vd, vh, vb], interior;
                        vd => [fx, fz],     [vi, vc],     interior;
                        ve => [fu, fw, fz], [va, vf, vj], interior;
                        vf => [fu, fv, fw], [va, vg, ve], interior;
                        vg => [fv, fy, fw], [vk, vf, vb], interior;
                        vh => [fx, fy],     [vc, vm],     interior;
                        vi => [fx, fz],     [vd, vn],     interior;
                        vj => [fw, fz],     [ve, vk],     interior;
                        vk => [fw, fy, fz], [vj, vg, vl], interior;
                        vl => [fy, fz],     [vk, vm],     interior;
                        vm => [fy, fx, fz], [vl, vh, vn], interior;
                        vn => [fx, fz],     [vm, vi],     interior;
                    },
                    faces: {
                        fu => [fw, fv, fz],                 [va, ve, vf],                 interior;
                        fv => [fu, fw, fy, fz],             [va, vf, vg, vb],             interior;
                        fw => [fy, fv, fu, fz, fz],         [ve, vj, vk, vg, vf],         interior;
                        fx => [fy, fy, fz, fz, fz, fz],     [vc, vh, vm, vn, vi, vd],     interior;
                        fy => [fv, fw, fz, fz, fx, fx, fz], [vb, vg, vk, vl, vm, vh, vc], interior;
                        fz => [fu, fv, fy, fx, fx, fx, fx, fy, fy, fw, fw],
                            [va, vb, vc, vd, vi, vn, vm, vl, vk, vj, ve], interior;
                    },
                    edges: {
                        va -- vb => [fv, fz], interior;
                        va -- ve => [fu, fz], interior;
                        va -- vf => [fu, fv], interior;
                        vb -- vc => [fy, fz], interior;
                        vb -- vg => [fv, fy], interior;
                        vc -- vd => [fx, fz], interior;
                        vc -- vh => [fx, fy], interior;
                        vd -- vi => [fx, fz], interior;
                        ve -- vf => [fu, fw], interior;
                        ve -- vj => [fw, fz], interior;
                        vf -- vg => [fv, fw], interior;
                        vg -- vk => [fw, fy], interior;
                        vh -- vm => [fx, fy], interior;
                        vi -- vn => [fx, fz], interior;
                        vj -- vk => [fw, fz], interior;
                        vk -- vl => [fy, fz], interior;
                        vl -- vm => [fy, fz], interior;
                        vm -- vn => [fx, fz], interior;
                    },
                });
            }

            #[test]
            fn split_quad_face() {
                //
                //            (a) ---- (b)                   (a) ---- (b)
                //          ⟋ |        |                  ⟋ | ⟍   ⟋ |
                //        ⟋   |   Y    |                ⟋   |  (m)  |
                //      ⟋  X  |        |              ⟋  X  | ⟋   ⟍ |
                //   (c) ---- (d) ---- (e)    =>    (c) ---- (d) ---- (e)
                //      ⟍            ⟋                ⟍            ⟋
                //        ⟍   Z    ⟋                    ⟍   Z    ⟋
                //          ⟍    ⟋                        ⟍    ⟋
                //            (f)                            (f)
                //
                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();
                let ve = m.add_vertex();
                let vf = m.add_vertex();

                let fx = m.add_triangle([va, vc, vd]);
                let fy = m.add_face(&[va, vd, ve, vb]);
                let fz = m.add_face(&[vc, vf, ve, vd]);

                let vm = m.split_face(fy);

                let faces = m.face_handles()
                    .filter(|&fh| fh != fx && fh != fz)
                    .collect::<Vec<_>>();
                let [f0, f1, f2, f3] = [faces[0], faces[1], faces[2], faces[3]];

                check_mesh!(m; $extras; {
                    vertices: {
                        va => {fx ...; 3},      [vb, vm, vd, vc], boundary;
                        vb => {...; 2},         [ve, vm, va],     boundary;
                        vc => [fx, fz],         [va, vd, vf],     boundary;
                        vd => {fx, fz ...; 4},  [va, vm, ve, vc], interior;
                        ve => {fz ...; 3},      [vf, vd, vm, vb], boundary;
                        vf => [fz],             [vc, ve],         boundary;
                        vm => {f0, f1, f2, f3}, [va, vb, ve, vd], interior;
                    },
                    faces: {
                        fx => {fz ...; 2}, [va, vc, vd],     boundary;
                        fz => {fx ...; 2}, [vc, vf, ve, vd], boundary;
                        f0 => no_check,    {vm ...; 3},      no_check;
                        f1 => no_check,    {vm ...; 3},      no_check;
                        f2 => no_check,    {vm ...; 3},      no_check;
                        f3 => no_check,    {vm ...; 3},      no_check;
                    },
                    edges: {
                        va -- vb => {...; 1},    boundary;
                        va -- vc => {fx},        boundary;
                        va -- vd => {fx ...; 2}, interior;
                        va -- vm => {...; 2},    interior;
                        vb -- ve => {...; 1},    boundary;
                        vb -- vm => {...; 2},    interior;
                        vc -- vd => {fx, fz},    interior;
                        vc -- vf => {fz},        boundary;
                        vd -- ve => {fz ...; 2}, interior;
                        vd -- vm => {...; 2},    interior;
                        ve -- vf => {fz},        boundary;
                        ve -- vm => {...; 2},    interior;
                    },
                });
            }

            #[test]
            fn remove_quad_face() {
                //
                //            (a) ---- (b)                   (a)      (b)
                //          ⟋ |        |                  ⟋  |
                //        ⟋   |   Y    |                ⟋    |
                //      ⟋  X  |        |              ⟋  X   |
                //   (c) ---- (d) ---- (e)    =>    (c) ---- (d) ---- (e)
                //      ⟍            ⟋                ⟍            ⟋
                //        ⟍   Z    ⟋                    ⟍   Z    ⟋
                //          ⟍    ⟋                        ⟍    ⟋
                //            (f)                            (f)
                //
                // We repeat the test three times with only one difference: the
                // order of vertices for `fy`. This might be important for
                // `remove_face`.
                for shift in 0..4 {
                    let mut m = <$name>::empty();
                    let va = m.add_vertex();
                    let vb = m.add_vertex();
                    let vc = m.add_vertex();
                    let vd = m.add_vertex();
                    let ve = m.add_vertex();
                    let vf = m.add_vertex();

                    let mut vertices = [va, vd, ve, vb];
                    vertices.rotate_left(shift);
                    let fx = m.add_triangle([va, vc, vd]);
                    let fy = m.add_face(&vertices);
                    let fz = m.add_face(&[vc, vf, ve, vd]);

                    m.remove_face(fy);

                    check_mesh!(m; $extras; {
                        vertices: {
                            va => [fx],     [vd, vc],     boundary;
                            vb => [],       [],           boundary;
                            vc => [fx, fz], [va, vd, vf], boundary;
                            vd => [fx, fz], [va, ve, vc], boundary;
                            ve => [fz],     [vf, vd],     boundary;
                            vf => [fz],     [vc, ve],     boundary;
                        },
                        faces: {
                            fx => [fz], [va, vc, vd],     boundary;
                            fz => [fx], [vc, vf, ve, vd], boundary;
                        },
                        edges: {
                            va -- vc => {fx},     boundary;
                            va -- vd => {fx},     boundary;
                            vc -- vd => {fx, fz}, interior;
                            vc -- vf => {fz},     boundary;
                            vd -- ve => {fz},     boundary;
                            ve -- vf => {fz},     boundary;
                        },
                    });
                }
            }

            #[test]
            fn panic_on_too_small_face() {
                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                m.add_vertex(); // A third one, such that a proper face *could* be possible

                let mut clone = m.clone();
                assert_panic!(clone.add_face(&[]));

                let mut clone = m.clone();
                assert_panic!(clone.add_face(&[va]));

                let mut clone = m.clone();
                assert_panic!(clone.add_face(&[va, vb]));
            }
        });

        test_helper!(@if_item [FullAdj] in $extras => {
            #[test]
            fn panic_on_remove_non_isolated_vertex() {
                //
                //  (e)      (a) ----- (b)        (e)      (a) ----- (b)
                //          /   \  Y  /                       \  Y  /
                //         /  X  \   /        =>               \   /
                //        /       \ /                           \ /
                //      (c) ----- (d)                 (c)       (d)
                //
                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();
                m.add_vertex();

                m.add_triangle([va, vc, vd]);
                m.add_triangle([va, vd, vb]);

                let mut clone = m.clone();
                assert_panic!(clone.remove_isolated_vertex(va));

                let mut clone = m.clone();
                assert_panic!(clone.remove_isolated_vertex(vb));

                let mut clone = m.clone();
                assert_panic!(clone.remove_isolated_vertex(vc));

                let mut clone = m.clone();
                assert_panic!(clone.remove_isolated_vertex(vd));
            }
        });

        test_helper!(@if_item [TriMesh, EdgeMesh, FullAdj] in $extras => {
            #[test]
            fn flip_edge() {
                //
                //        (a) ----- (b)               (a) ----- (b)
                //        / \       / \               /       ⟋/  \
                //       /   \  Y  /   \     =>      /  ?  ⟋  /    \
                //      /  X  \   /  Z  \           /   ⟋ ?  /  Z   \
                //     /       \ /       \         / ⟋      /        \
                //   (c) ----- (d) ----- (e)     (c) ----- (d) ----- (e)
                //
                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();
                let ve = m.add_vertex();

                let fx = m.add_triangle([va, vc, vd]);
                let fy = m.add_triangle([va, vd, vb]);
                let fz = m.add_triangle([vb, vd, ve]);

                let e = m.edge_between_vertices(va, vd)
                    .expect("`edge_between_vertices` returned `None` unexpectedly");
                m.flip_edge(e);

                check_mesh!(m; $extras; {
                    vertices: {
                        va => {...; 1},     [vb, vc],         boundary;
                        vb => {fx, fy, fz}, [ve, vd, vc, va], boundary;
                        vc => {fx, fy},     [va, vb, vd],     boundary;
                        vd => {fz ...; 2},  [vc, vb, ve],     boundary;
                        ve => {fz},         [vd, vb],         boundary;
                    },
                    faces: {
                        fx => {fy ...}, {vb, vc ...; 3}, boundary;
                        fy => {fx ...}, {vb, vc ...; 3}, boundary;
                        fz => {...; 1}, [ve, vb, vd],    boundary;
                    },
                    edges: {
                        va -- vb     => {...; 1},    boundary;
                        va -- vc     => {...; 1},    boundary;
                        vb -- vc @ e => {fx, fy},    interior;
                        vb -- vd     => {fz ...; 2}, interior;
                        vb -- ve     => {fz},        boundary;
                        vc -- vd     => {...; 1},    boundary;
                        vd -- ve     => {fz},        boundary;
                    },
                });
            }

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

        test_helper!(@if_item [TriMesh, EdgeMesh, EdgeAdj] in $extras => {
            #[test]
            fn panic_on_invalid_flip_edge() {
                //
                //         (C) ----- (D)
                //        /   \  Y  /
                //       /  X  \   /
                //      /       \ /
                //    (A) ----- (B)
                //
                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();
                m.add_triangle([va, vb, vc]);
                m.add_triangle([vd, vc, vb]);

                // Edge must be interior!
                let e0 = m.edge_between_vertices(va, vb).unwrap();
                let mut clone = m.clone();
                assert_panic!(clone.flip_edge(e0));

                // And edge must be valid
                let e1 = m.edge_between_vertices(va, vc).unwrap();
                let e2 = m.edge_between_vertices(vb, vc).unwrap();
                let e3 = m.edge_between_vertices(vb, vd).unwrap();
                let e4 = m.edge_between_vertices(vc, vd).unwrap();
                let invalid = EdgeHandle::new(
                    [e0, e1, e2, e3, e4].iter().map(|h| h.idx()).max().unwrap() + 1
                );
                assert_panic!(m.flip_edge(invalid));
            }
        });

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

                // Try to insert the remaining faces (just make sure it doesn't
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

            #[test]
            fn two_blades_in_loop() {
                //
                //          (b)---(d)
                //         / |   / | \
                //      (a)  |  /  |  (a)
                //         \ | /   | /
                //          (c)---(e)
                //
                let mut m = <$name>::empty();
                let va = m.add_vertex();
                let vb = m.add_vertex();
                let vc = m.add_vertex();
                let vd = m.add_vertex();
                let ve = m.add_vertex();

                let f_acb = m.add_triangle([va, vc, vb]);
                let f_bcd = m.add_triangle([vb, vc, vd]);
                let f_ced = m.add_triangle([vc, ve, vd]);
                let f_ead = m.add_triangle([ve, va, vd]);

                check_mesh!(m; $extras; {
                    vertices: {
                        va => [f_acb, f_ead],        {vb, vc, vd, ve}, boundary;
                        vb => [f_acb, f_bcd],        [vd, vc, va],     boundary;
                        vc => [f_acb, f_bcd, f_ced], [va, vb, vd, ve], boundary;
                        vd => [f_ead, f_ced, f_bcd], [va, ve, vc, vb], boundary;
                        ve => [f_ced, f_ead],        [vc, vd, va],     boundary;
                    },
                    faces: {
                        f_acb => [f_bcd],        [va, vc, vb], boundary;
                        f_bcd => [f_acb, f_ced], [vb, vc, vd], boundary;
                        f_ced => [f_bcd, f_ead], [vc, ve, vd], boundary;
                        f_ead => [f_ced],        [ve, va, vd], boundary;
                    },
                    edges: {
                        va -- vb => {f_acb},        boundary;
                        va -- vc => {f_acb},        boundary;
                        va -- vd => {f_ead},        boundary;
                        va -- ve => {f_ead},        boundary;
                        vb -- vc => {f_bcd, f_acb}, interior;
                        vb -- vd => {f_bcd},        boundary;
                        vc -- vd => {f_bcd, f_ced}, interior;
                        vc -- ve => {f_ced},        boundary;
                        vd -- ve => {f_ced, f_ead}, interior;
                    },
                });


                // Add the two missing faces of the four sided pyramid.
                //
                //             (a)
                //            /   \
                //          (b)---(d)
                //         / |   / | \
                //      (a)  |  /  |  (a)
                //         \ | /   | /
                //          (c)---(e)
                //            \   /
                //             (a)
                //
                let f_bda = m.add_triangle([vb, vd, va]);
                let f_aec = m.add_triangle([va, ve, vc]);

                check_mesh!(m; $extras; {
                    vertices: {
                        va => [f_acb, f_aec, f_ead, f_bda], {vb, vc, vd, ve}, interior;
                        vb => [f_bda, f_bcd, f_acb],        [vd, vc, va],     interior;
                        vc => [f_acb, f_bcd, f_ced, f_aec], [va, vb, vd, ve], interior;
                        vd => [f_ead, f_ced, f_bcd, f_bda], [va, ve, vc, vb], interior;
                        ve => [f_aec, f_ced, f_ead],        [vc, vd, va],     interior;
                    },
                    faces: {
                        f_acb => [f_aec, f_bcd, f_bda], [va, vc, vb], interior;
                        f_bcd => [f_acb, f_ced, f_bda], [vb, vc, vd], interior;
                        f_ced => [f_bcd, f_aec, f_ead], [vc, ve, vd], interior;
                        f_ead => [f_bda, f_ced, f_aec], [ve, va, vd], interior;
                        f_bda => [f_acb, f_bcd, f_ead], [vb, vd, va], interior;
                        f_aec => [f_ead, f_ced, f_acb], [va, ve, vc], interior;
                    },
                    edges: {
                        va -- vb => {f_acb, f_bda}, interior;
                        va -- vc => {f_acb, f_aec}, interior;
                        va -- vd => {f_ead, f_bda}, interior;
                        va -- ve => {f_ead, f_aec}, interior;
                        vb -- vc => {f_bcd, f_acb}, interior;
                        vb -- vd => {f_bcd, f_bda}, interior;
                        vc -- vd => {f_bcd, f_ced}, interior;
                        vc -- ve => {f_ced, f_aec}, interior;
                        vd -- ve => {f_ced, f_ead}, interior;
                    },
                });
            }

            #[test]
            fn remove_face_of_two_blade_vertex() {
                //
                //      (b)-------(c)       (b)       (c)
                //        \       /
                //         \  X  /
                //          \   /
                //           \ /
                //           (a)       =>        (a)
                //           / \                 / \
                //          /   \               /   \
                //         /  Y  \             /  Y  \
                //        /       \           /       \
                //      (d)-------(e)       (d)-------(e)
                //
                // We repeat the test three times with only one difference: the
                // order of vertices for `fx`. This might be important for
                // `remove_face`.
                for shift in 0..3 {
                    let mut m = <$name>::empty();
                    let va = m.add_vertex();
                    let vb = m.add_vertex();
                    let vc = m.add_vertex();
                    let vd = m.add_vertex();
                    let ve = m.add_vertex();

                    let mut vertices = [va, vc, vb];
                    vertices.rotate_left(shift);
                    let fx = m.add_triangle(vertices);
                    let fy = m.add_triangle([va, vd, ve]);

                    m.remove_face(fx);

                    check_mesh!(m; $extras; {
                        vertices: {
                            va => [fy], [vd, ve], boundary;
                            vb => [],   [],       boundary;
                            vc => [],   [],       boundary;
                            vd => [fy], [va, ve], boundary;
                            ve => [fy], [va, vd], boundary;
                        },
                        faces: {
                            fy => [], [va, vd, ve], boundary;
                        },
                        edges: {
                            va -- vd => {fy}, boundary;
                            va -- ve => {fy}, boundary;
                            vd -- ve => {fy}, boundary;
                        },
                    });
                }
            }

            #[test]
            fn remove_face_of_three_blade_vertex() {
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
                // We repeat the test three times with only one difference: the
                // order of vertices for `fx`. This might be important for
                // `remove_face`.
                for shift in 0..3 {
                    let mut m = <$name>::empty();
                    let va = m.add_vertex();
                    let vb = m.add_vertex();
                    let vc = m.add_vertex();
                    let vd = m.add_vertex();
                    let ve = m.add_vertex();
                    let vf = m.add_vertex();
                    let vg = m.add_vertex();

                    let mut vertices = [va, vc, vb];
                    vertices.rotate_left(shift);
                    let fx = m.add_triangle(vertices);
                    let fy = m.add_triangle([va, ve, vd]);
                    let fz = m.add_triangle([va, vg, vf]);

                    m.remove_face(fx);

                    // ----- Check stuff
                    check_mesh!(m; $extras; {
                        vertices: {
                            va => [fy, fz], {vd, ve, vf, vg}, boundary;
                            vb => [],       [],               boundary;
                            vc => [],       [],               boundary;
                            vd => [fy],     [va, ve],         boundary;
                            ve => [fy],     [vd, va],         boundary;
                            vf => [fz],     [va, vg],         boundary;
                            vg => [fz],     [vf, va],         boundary;
                        },
                        faces: {
                            fy => [], [va, ve, vd], boundary;
                            fz => [], [va, vg, vf], boundary;
                        },
                        edges: {
                            va -- vd => {fy}, boundary;
                            va -- ve => {fy}, boundary;
                            va -- vf => {fz}, boundary;
                            va -- vg => {fz}, boundary;
                            vd -- ve => {fy}, boundary;
                            vf -- vg => {fz}, boundary;
                        }
                    });
                }
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


        // TODO: something that "only" has multi fan blades but cannot be
        // repaired into anything useful anymore. Like:
        //
        //          (b)---(d)            (f)---(h)
        //         / |   / | \          / |   / | \
        //      (a)  |  /  |  (a)    (a)  |  /  |  (a)
        //         \ | /   | /          \ | /   | /
        //          (c)---(e)            (g)---(i)
        //
        // The (a) vertex exists only once. Although... that doesn't necessarily
        // break things right? It cannot be closed without breaking stuff, but
        // it's fine as a manifold open mesh?
        //
        // TODO: things that are not clear yet if/how they are supported:
        //  - Double Sided triangle
        //  - Möbius strip
        //
        // TODO: test with 4 or more fan blades
        //
        // TODO: poly mesh tests
        //  - face with huge valance
        //  - many different valences in one mesh
        //  - split edge with non-triangular face
        //  - split non-triangular face
        //
        // TODO: removal
        //  - remove face/vertex simple
        //  - remove and reinsert
        //  - remove all faces/vertices
    };
}
