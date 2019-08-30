use super::*;

gen_mesh_tests!(DirectedEdgeMesh::<DefaultConfig>: [
    TriMesh,
    // EdgeMesh,
    BasicAdj,
    FullAdj,
    // EdgeAdj,
    Manifold,
    SupportsMultiBlade
]);
