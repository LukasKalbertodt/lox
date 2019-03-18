
#![allow(unused_imports)]
use cgmath::{Point3, Vector3};
use failure::{Error, ResultExt};

use lox::{
    MemSink, MemSource,
    prelude::*,
    algo,
    ds::{FaceDelegateMesh, SharedVertexMesh},
    io::{self, stl, ply},
    map::VecMap,
    shape::Disc,
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
    let m: SimpleMesh = if let Some(filename) = std::env::args().nth(1) {
        io::read_file(filename)?
    } else {
        let disc = Disc { faces: 3, .. Disc::default() };
        SimpleMesh::create_from(disc)?
    };

    println!("#faces:    {}", m.mesh.num_faces());
    println!("#vertices: {}", m.mesh.num_vertices());
    // println!("{:#?}", m);

    io::write_file("out.ply", &m)?;

    Ok(())
}


#[derive(Empty, MemSink, MemSource, Debug)]
struct SimpleMesh {
    #[lox(core_mesh)]
    mesh: FaceDelegateMesh,

    #[lox(vertex_position)]
    vertex_positions: VecMap<VertexHandle, Point3<f32>>,

    // #[lox(vertex_normal)]
    // vertex_normals: VecMap<VertexHandle, Vector3<f32>>,

    // #[lox(vertex_color)]
    // vertex_colors: VecMap<VertexHandle, [u8; 3]>,

    // #[lox(face_normal)]
    // face_normals: VecMap<FaceHandle, Vector3<f32>>,

    // #[lox(face_color)]
    // face_colors: VecMap<FaceHandle, (f32, f32, f32, f32)>,
}
