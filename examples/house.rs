extern crate resh;

use std::fs::File;

use resh::io::{MeshSerialize, Ply};
use resh::impls::FvTriMesh;


fn main() {
    let mesh: FvTriMesh = FvTriMesh::new();


    let file = File::create("foo.ply").unwrap();
    let writer = Ply::ascii(file);
    writer.serialize(&mesh).unwrap();
}
