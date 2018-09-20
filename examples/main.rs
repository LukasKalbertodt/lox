#![feature(proc_macro_non_items)]

use cgmath::Point3;
use failure::Error;

use lox::{
    mesh,
    ds::SharedVertexMesh,
    io::{
        MeshWriter,
        stl,
    },
};


fn main() -> Result<(), Error> {
    let (mesh, positions) = mesh! {
        type: SharedVertexMesh,
        vertices: [
            v0: (Point3::new(0.0, 0.0, 0.0)),
            v1: (Point3::new(0.0, 1.0, 0.0)),
            v2: (Point3::new(1.0, 0.0, 0.0)),
            v3: (Point3::new(1.0, 1.0, 0.0)),
        ],
        faces: [
            [v0, v2, v1],
            [v3, v1, v2],
        ],
    };

    stl::Serializer::ascii()
    // stl::Serializer::binary()
        .into_writer(&mesh, &positions)
        .write_to_stdout()?;
        // .write_to_file("mesh.stl")?;


    Ok(())
}
