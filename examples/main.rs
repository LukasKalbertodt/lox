#![feature(proc_macro_non_items)]

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
            v0: ([0.0, 0.0, 0.0]),
            v1: ([0.0, 1.0, 0.0]),
            v2: ([1.0, 0.0, 0.0]),
            v3: ([1.0, 1.0, 0.0]),
        ],
        faces: [
            [v0, v2, v1],
            [v3, v1, v2],
        ],
    };

    // println!("{:?}", mesh);
    // println!("{:?}", positions);
    // println!("{:?}", labels);
    // println!("{:?}", names);

    stl::Serializer::ascii()
        .into_writer(&mesh, &positions)
        // .write_to_stdout()?;
        .write_to_file("mesh.stl")?;


    Ok(())
}
