use crate::ds::{StoreField, OmitField, half_edge::*};

mod tri {
    use super::*;

    pub enum Conf {}
    impl Config for Conf {
        type FaceKind = TriFaces;
        type PrevEdge = OmitField;
    }

    gen_mesh_tests!(HalfEdgeMesh::<Conf>: [
        TriMesh,
        EdgeMesh,
        BasicAdj,
        FullAdj,
        EdgeAdj,
        Manifold,
        SupportsMultiBlade
    ]);
}

mod poly {
    use super::*;

    pub enum Conf {}
    impl Config for Conf {
        type FaceKind = PolyFaces;
        type PrevEdge = OmitField;
    }

    gen_mesh_tests!(HalfEdgeMesh::<Conf>: [
        PolyMesh,
        EdgeMesh,
        BasicAdj,
        FullAdj,
        EdgeAdj,
        Manifold,
        SupportsMultiBlade
    ]);
}

mod tri_with_prev {
    use super::*;

    pub enum Conf {}
    impl Config for Conf {
        type FaceKind = TriFaces;
        type PrevEdge = StoreField;
    }

    gen_mesh_tests!(HalfEdgeMesh::<Conf>: [
        TriMesh,
        EdgeMesh,
        BasicAdj,
        FullAdj,
        EdgeAdj,
        Manifold,
        SupportsMultiBlade
    ]);
}

mod poly_with_prev {
    use super::*;

    pub enum Conf {}
    impl Config for Conf {
        type FaceKind = PolyFaces;
        type PrevEdge = StoreField;
    }

    gen_mesh_tests!(HalfEdgeMesh::<Conf>: [
        PolyMesh,
        EdgeMesh,
        BasicAdj,
        FullAdj,
        EdgeAdj,
        Manifold,
        SupportsMultiBlade
    ]);
}
