use super::*;

gen_tri_mesh_tests!(DirectedEdgeMesh::<DefaultConfig>: [
    TriMesh,
    // EdgeMesh,
    BasicAdj,
    FullAdj,
    // EdgeAdj,
    Manifold,
    SupportsMultiBlade
]);
