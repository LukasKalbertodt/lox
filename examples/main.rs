#![feature(proc_macro_hygiene, never_type)]
#![allow(unused_imports)]

use cgmath::{Point3, Vector3};
use failure::{Error, ResultExt};

use lox::{
    ds::{LinkedFaceMesh, SharedVertexMesh},
    io::{stl, ply, MemSink, StreamingSource},
    map::{ConstMap, FnMap},
    math::PrimitiveNum,
    mesh,
    prelude::*,
    shape,
};


fn main() {
    // We just catch potential errors here and pretty print them.
    if let Err(e) = run() {
        println!("ERROR: {}", e);

        for cause in e.iter_causes() {
            println!("  ... caused by: {}", cause);
        }

        if std::env::var("RUST_BACKTRACE") == Ok("1".to_string()) {
            println!();
            println!("{}", e.backtrace());
        }

        std::process::exit(1);
    }
}

fn run() -> Result<(), Error> {
    let mesh = mesh! {
        type: LinkedFaceMesh,
        vertices: [
            v0,
            v1,
            v2,
            v3,
            v4,
            v5,
            v6,
        ],
        faces: [
            [v0, v2, v1],
            [v3, v1, v2],
            [v2, v4, v5],
            [v2, v5, v6],
            [v3, v2, v6],
        ],
    };

    // println!("{:#?}", mesh);

    for vh in mesh.vertices().map(|v| v.handle()) {
        println!("around {:?}: {:?}", vh, mesh.faces_around_vertex(vh).into_vec());
    }

    Ok(())
}
