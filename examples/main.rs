#![feature(proc_macro_hygiene)]
#![allow(unused_imports)]

use cgmath::{Point3, Vector3};
use failure::Error;

use lox::{
    mesh,
    ds::SharedVertexMesh,
    io::{stl, ply},
    map::{ConstMap, FnMap},
    prelude::*,
};


fn main() -> Result<(), Error> {
    // let (mesh, positions, _face_normals) = mesh! {
    //     type: SharedVertexMesh,
    //     vertices: [
    //         v0: (Point3::new(0.0f32, 0.0, 0.0)),
    //         v1: (Point3::new(0.0, 1.0, 0.0)),
    //         v2: (Point3::new(1.0, 0.0, 0.0)),
    //         v3: (Point3::new(1.0, 1.0, 1.0)),
    //     ],
    //     faces: [
    //         [v0, v2, v1]: (Vector3::new(0.0f32, 0.0, 1.0)),
    //         [v3, v1, v2]: (Vector3::new(0.0, 0.0, -1.0)),
    //     ],
    // };

    // ply::Serializer::ascii()
    // stl::Serializer::binary()
    //     .into_writer(&mesh, &positions)
    //     // .add_vertex_prop("peter", &ConstMap(27u8))
    //     // .with_vertex_normals(&ConstMap(Vector3::new(0.0, 0.0, 1.0)))
    //     // .with_face_normals(&face_normals)
    //     // .write_to_stdout()?;
    //     .write_to_file("mesh.stl")?;

    let filename = std::env::args().nth(1).unwrap();
    println!("loading {}", filename);

    let before = std::time::Instant::now();
    let res = stl::Reader::open(filename)?
        .read::<SharedVertexMesh>(stl::ReadOptions {
            unify_vertices: true,
            read_positions: false,
            .. stl::ReadOptions::default()
        })?;
    println!("Mesh: {} vertices, {} faces", res.mesh.num_vertices(), res.mesh.num_faces());
    if let Some(positions) = &res.vertex_positions {
        println!("First position: {:?}", positions[VertexHandle::from_usize(0)]);
    }
    if let Some(normals) = &res.face_normals {
        println!("First normal: {:?}", normals[FaceHandle::from_usize(0)]);
    }

    println!("elapsed: {:.2?}", before.elapsed());

    Ok(())
}
