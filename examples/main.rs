#![feature(proc_macro_non_items)]

use lox::{
    mesh,
    ds::SharedVertexMesh,
};


fn main() {
    let (mesh, positions, labels, names) = mesh! {
        type: SharedVertexMesh,
        vertices: [
            v0: (3.0, 'x'),
            v1: (5.0, 'y'),
            v2: (1.0, 'z'),
            v3: (8.2, 'w'),
        ],
        faces: [
            [v0, v1, v2]: ("top"),
            [v1, v2, v3]: ("bottom"),
        ],
    };

    println!("{:?}", mesh);
    println!("{:?}", positions);
    println!("{:?}", labels);
    println!("{:?}", names);
}
