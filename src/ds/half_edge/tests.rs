mod tri {
    use super::super::*;

    gen_tri_mesh_tests!(HalfEdgeMesh::<TriConfig>: [
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
    use super::super::*;

    gen_tri_mesh_tests!(HalfEdgeMesh::<PolyConfig>: [
        PolyMesh,
        EdgeMesh,
        BasicAdj,
        FullAdj,
        EdgeAdj,
        Manifold,
        SupportsMultiBlade
    ]);
}
