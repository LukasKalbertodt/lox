extern crate fev;


use fev::{
    impls::sv::SharedVertexMesh,
};

fn main() {
    let mut mesh = SharedVertexMesh::new();
    let a = mesh.add_vertex((1.0, 2.0, 3.0));
    let b = mesh.add_vertex((3.0, 2.0, 1.0));
    let c = mesh.add_vertex((4.0, 4.0, 4.0));
    mesh.add_face([a, b, c], ());

    // println!("{:#?}", mesh);
}
