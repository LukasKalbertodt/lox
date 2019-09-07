use crate::traits::marker::{False, True};
use super::*;

mod minimal {
    use super::*;

    enum Conf {}
    impl Config for Conf {
        type StoreNext = False;
        type StorePrev = False;
    }

    gen_mesh_tests!(DirectedEdgeMesh::<Conf>: [
        TriMesh,
        BasicAdj,
        FullAdj,
        Manifold,
        SupportsMultiBlade
    ]);
}

mod with_next_prev {
    use super::*;

    enum Conf {}
    impl Config for Conf {
        type StoreNext = True;
        type StorePrev = True;
    }

    gen_mesh_tests!(DirectedEdgeMesh::<Conf>: [
        TriMesh,
        BasicAdj,
        FullAdj,
        Manifold,
        SupportsMultiBlade
    ]);
}
