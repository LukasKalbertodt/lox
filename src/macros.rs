/// Convenience macro to quickly create a small triangular mesh.
///
/// # TODO
/// - Example
#[macro_export]
macro_rules! mesh {
    // Special rule for an empty mesh
    (
        type: $mesh_type:path,
        vertices: [],
        faces: [] $(,)*
    ) => { <$mesh_type as $crate::Mesh>::empty() };

    // The main rule
    (
        type: $mesh_type:path,
        vertices: [
            $first_vert_name:ident: ( $($first_vert_val:expr),* ),
            $( $vert_name:ident: ( $($vert_val:expr),* ) ,)*
        ],
        faces: [
            $([$va:ident, $vb:ident, $vc:ident] ,)*
        ] $(,)*
    ) => {{
        use $crate::{
            Mesh, ExplicitFace, ExplicitVertex,
            map::{PropStoreMut, VecMap},
        };

        let mut mesh = <$mesh_type as Mesh>::empty();

        // Add vertices
        let $first_vert_name = mesh.add_vertex();
        $(
            let $vert_name = mesh.add_vertex();
        )*

        // Add faces
        $(
            <$mesh_type as ExplicitFace>::add_face(&mut mesh, [$va, $vb, $vc]);
        )*

        // Create all property maps and return everything
        mesh!(@make
            (mesh);
            ( $( {$first_vert_val} )* );
            [
                $first_vert_name: ( $($first_vert_val),* ),
                $( $vert_name: ( $($vert_val),* ) ,)*
            ]
        )
    }};

    (@make
        $ret:tt;
        ();
        $($_tail:tt)*
    ) => {{
        $ret
    }};

    (@make
        ($($ret:tt)*);
        ($_head:tt $($tail:tt)* );
        [$($vert_name:ident: ( $first_vert_val:expr $(, $val:expr)* ) ,)*]
    ) => {{
        let mut map = VecMap::new();
        $(
            <VecMap<_, _> as PropStoreMut<_>>::insert(&mut map, $vert_name, $first_vert_val);
        )*

        mesh!(@make
            ($($ret)*, map);
            ($($tail)*);
            [$($vert_name: ( $($val),* ) ,)*]
        )
    }};
}
