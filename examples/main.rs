#![feature(proc_macro_hygiene)]
#![allow(unused_imports)]

use cgmath::{Point3, Vector3};
use failure::Error;

use lox::{
    mesh,
    ds::SharedVertexMesh,
    io::{stl, ply},
    prelude::*,
};


fn main() -> Result<(), Error> {
    let (mesh, positions, _face_normals) = mesh! {
        type: SharedVertexMesh,
        vertices: [
            v0: (Point3::new(0.0f32, 0.0, 0.0)),
            v1: (Point3::new(0.0, 1.0, 0.0)),
            v2: (Point3::new(1.0, 0.0, 0.0)),
            v3: (Point3::new(1.0, 1.0, 1.0)),
        ],
        faces: [
            [v0, v2, v1]: (Vector3::new(0.0, 0.0, 1.0)),
            [v3, v1, v2]: (Vector3::new(0.0, 0.0, -1.0)),
        ],
    };

    ply::Serializer::ascii()
    // stl::Serializer::binary()
        .into_writer(&mesh, &positions)
        // .with_face_normals(&face_normals)
        // .write_to_stdout()?;
        .write_to_file("mesh.ply")?;


    Ok(())
}
