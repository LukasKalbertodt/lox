use crate::traits::marker::{False, True};

mod tri {
    use super::{*, super::*};

    pub enum Conf {}
    impl Config for Conf {
        type FaceKind = TriFaces;
        type StorePrev = False;
    }

    gen_tri_mesh_tests!(HalfEdgeMesh::<Conf>: [
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
    use super::{*, super::*};

    pub enum Conf {}
    impl Config for Conf {
        type FaceKind = PolyFaces;
        type StorePrev = False;
    }

    gen_tri_mesh_tests!(HalfEdgeMesh::<Conf>: [
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
    use super::{*, super::*};

    pub enum Conf {}
    impl Config for Conf {
        type FaceKind = TriFaces;
        type StorePrev = True;
    }

    gen_tri_mesh_tests!(HalfEdgeMesh::<Conf>: [
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
    use super::{*, super::*};

    pub enum Conf {}
    impl Config for Conf {
        type FaceKind = PolyFaces;
        type StorePrev = True;
    }

    gen_tri_mesh_tests!(HalfEdgeMesh::<Conf>: [
        PolyMesh,
        EdgeMesh,
        BasicAdj,
        FullAdj,
        EdgeAdj,
        Manifold,
        SupportsMultiBlade
    ]);
}
