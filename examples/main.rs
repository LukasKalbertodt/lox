#![feature(proc_macro_non_items)]

use lox::{
    mesh,
    ds::SharedVertexMesh,
};


fn main() {
    let (mesh, maps) = mesh! {
        type: SharedVertexMesh,
        vertices: [
            v0: (3.0),
            v1: (5.0),
            v2: (1.0),
            v3: (8.2),
        ],
        faces: [
            [v0, v1, v2],
            [v1, v2, v3],
        ],
    };

    println!("{:?}", mesh);
    println!("{:?}", maps);
}
