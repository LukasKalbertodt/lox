extern crate fev;

use std::fs::File;

use fev::{
    impls::FvTriMesh,
    io::{Ply},
    map::VertexVecMap,
};


fn main() {
    let mut mesh: FvTriMesh = FvTriMesh::new();
    let mut xs = VertexVecMap::new();
    let mut ys = VertexVecMap::new();

    let va = mesh.add_vertex();
    xs.insert(va, 0);
    ys.insert(va, 3);
    let vb = mesh.add_vertex();
    xs.insert(vb, 1);
    ys.insert(vb, 0);
    let vc = mesh.add_vertex();
    xs.insert(vc, 5);
    ys.insert(vc, 1);

    let _f = mesh.add_face([va, vb, vc]);


    let mut file = File::create("foo.ply").unwrap();
    Ply::ascii()
        .add_vertex_attr("x", &xs)
        .add_vertex_attr("y", &ys)
        .write(&mut file, &mesh)
        .unwrap();
}
