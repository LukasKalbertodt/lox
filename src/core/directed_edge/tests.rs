use crate::core::{StoreField, OmitField};
use super::*;

mod minimal {
    use super::*;

    enum Conf {}
    impl Config for Conf {
        type NextEdge = OmitField;
        type PrevEdge = OmitField;
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
        type NextEdge = StoreField;
        type PrevEdge = StoreField;
    }

    gen_mesh_tests!(DirectedEdgeMesh::<Conf>: [
        TriMesh,
        BasicAdj,
        FullAdj,
        Manifold,
        SupportsMultiBlade
    ]);
}
