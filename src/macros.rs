/// Convenience macro to quickly create a small triangular mesh.
///
/// # TODO
/// - Example
#[macro_export]
macro_rules! tri_mesh {
    (
        vertices: [
            $($name:ident ,)*
        ],
        faces: [
            $([$va:ident, $vb:ident, $vc:ident] ,)*
        ],
    ) => {{
        use $crate::{Mesh, ExplicitFace, ExplicitVertex};

        fn build<M: Mesh + ExplicitFace + ExplicitVertex>() -> M {
            let mut m = M::empty();

            $(
                let $name = m.add_vertex();
            )*

            $(
                m.add_face([$va, $vb, $vc]);
            )*

            m
        }

        build()
    }}
}
