
macro_rules! set {
    ($($item:expr),* $(,)*) => {
        vec![$($item),*].into_iter().collect::<::std::collections::HashSet<_>>()
    }
}

macro_rules! assert_eq_set {
    ($iter:expr, [$($item:expr),* $(,)*]) => {
        assert_eq!(
            $iter.collect::<::std::collections::HashSet<_>>(),
            set!($($item),*)
        );
    }
}

macro_rules! assert_eq_order {
    ($arr:expr, [$a:ident, $b:ident, $c:ident]) => {{
        let a = $arr;
        let pos = a.iter().position(|&e| e == $a)
            .expect(concat!(stringify!($a), " not found in ", stringify!($arr)));

        assert_eq!(a[(pos + 1) % 3], $b);
        assert_eq!(a[(pos + 2) % 3], $c);
    }}
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
/// - `TriVerticesOfFace`
macro_rules! gen_tri_mesh_tests {
    ($name:ident : [$($extra:ident),*]) => {
        $(
            gen_tri_mesh_tests!(@is_valid_extra_trait $extra);
        )*

        #[test]
        fn empty() {
            let m = $name::empty();

            assert_eq!(m.num_faces(), 0);
            assert_eq!(m.num_vertices(), 0);

            assert!(m.faces().next().is_none());
            assert!(m.vertices().next().is_none());
        }

        #[test]
        fn single_triangle() {
            let mut m = $name::empty();
            let va = m.add_vertex();
            let vb = m.add_vertex();
            let vc = m.add_vertex();
            let f = m.add_face([va, vb, vc]);

            assert_eq!(m.num_faces(), 1);
            assert_eq!(m.num_vertices(), 3);

            assert_eq_set!(m.faces().map(|x| x.handle()), [f]);
            assert_eq_set!(m.vertices().map(|x| x.handle()), [va, vb, vc]);

            gen_tri_mesh_tests!(@if TriVerticesOfFace in [$($extra),*] => {
                assert_eq_order!(m.vertices_of_face(f), [va, vb, vc]);
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

            let f_bottom = m.add_face([va, vc, vb]);
            let f_ab = m.add_face([va, vb, v_top]);
            let f_bc = m.add_face([vb, vc, v_top]);
            let f_ca = m.add_face([vc, va, v_top]);

            assert_eq!(m.num_faces(), 4);
            assert_eq!(m.num_vertices(), 4);

            assert_eq_set!(m.faces().map(|x| x.handle()), [f_bottom, f_ab, f_bc, f_ca]);
            assert_eq_set!(m.vertices().map(|x| x.handle()), [va, vb, vc, v_top]);

            gen_tri_mesh_tests!(@if TriVerticesOfFace in [$($extra),*] => {
                assert_eq_order!(m.vertices_of_face(f_bottom), [va, vc, vb]);
                assert_eq_order!(m.vertices_of_face(f_ab), [va, vb, v_top]);
                assert_eq_order!(m.vertices_of_face(f_bc), [vb, vc, v_top]);
                assert_eq_order!(m.vertices_of_face(f_ca), [vc, va, v_top]);
            });
        }
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

    // These arms are used to make sure all traits passed into the macro
    // (include the ones used in the definition of the macro) are valid.
    // Otherwise it's too easy to make a typo.
    (@is_valid_extra_trait TriVerticesOfFace) => {};
    (@is_valid_extra_trait $other:ident) => {
        compile_error!(concat!(
            "`",
            stringify!($other),
            "` is not a valid trait to pass to `gen_tri_mesh_tests`",
        ));
    };
}
