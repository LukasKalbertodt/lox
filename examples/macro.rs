#![feature(proc_macro_hygiene)]

use failure::Error;
use lox::{
    ds::SharedVertexMesh,
    io::stl,
    mesh,
    prelude::*,
};


fn main() -> Result<(), Error> {
    // Example: create a small mesh via `mesh!` macro
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


    // Example: how to iterate through vertices
    for v in mesh.vertices() {
        let h = v.handle();
        println!("Vertex {:?} has position {:?}", h, vertex_positions[h]);
    }


    // Example: write mesh to file
    stl::Config::binary()
        .into_writer(&mesh, &vertex_positions)
        .write_to_file("test.stl")?;


    Ok(())
}
