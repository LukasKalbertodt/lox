#![feature(proc_macro_hygiene)]

use lox::{
    core::SharedVertexMesh,
    mesh,
};


fn main() {
    // Creates a mesh with two triangles.
    let (mesh, vertex_positions, face_colors) = mesh! {
        type: SharedVertexMesh,
        vertices: [
            v0: ([0.0, 0.0, 0.0]),
            v1: ([0.0, 1.0, 0.0]),
            v2: ([1.0, 0.0, 0.0]),
            v3: ([1.0, 1.0, 0.0]),
        ],
        faces: [
            [v0, v2, v1]: ("red"),
            [v3, v1, v2]: ("green"),
        ],
    };

    dbg!(&mesh);
    dbg!(&vertex_positions);
    dbg!(&face_colors);
}
