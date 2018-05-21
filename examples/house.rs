extern crate fev;

use std::fs::File;

use fev::{
    impls::FvTriMesh,
    io::{MeshSerializer, Ply},
    maps::VertexVecMap,
};


fn main() {
    let mut mesh: FvTriMesh = FvTriMesh::new();
    let mut positions = VertexVecMap::new();

    let va = mesh.add_vertex();
    positions.insert(va, (0.0, 0.0, 0.0));
    let vb = mesh.add_vertex();
    positions.insert(vb, (1.0, 0.0, 0.0));
    let vc = mesh.add_vertex();
    positions.insert(vc, (0.0, 1.0, 0.0));

    let _f = mesh.add_face([va, vb, vc]);


    let mut file = File::create("foo.ply").unwrap();
    let serializer = Ply::ascii();
    serializer.serialize(&mut file, &mesh).unwrap();
}
