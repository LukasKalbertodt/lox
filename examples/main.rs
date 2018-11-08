#![feature(proc_macro_hygiene)]
#![allow(unused_imports)]

use cgmath::{Point3, Vector3};
use failure::Error;

use lox::{
    MeshWithProps,
    ds::SharedVertexMesh,
    io::{stl, ply},
    map::{ConstMap, FnMap},
    mesh,
    prelude::*,
    shape,
};


fn main() -> Result<(), Error> {
    // let disc = shape::Disc {};
    // disc.build();

    // let res = gl::Buffer::build_from(shape::Disc {
    //     faces: 6,
    // });
    // println!("{:?}", res);

    let res = MeshWithProps::<SharedVertexMesh, _, _>::build_from(shape::Disc {
        // faces: 6,
        .. Default::default()
    }).unwrap();
    // println!("{:#?}", res);
    ply::Serializer::ascii()
        .into_writer(
            &res.mesh,
            &res.vertex_props.map_value(|prop| prop.position.map(|s| s as f32)),
        )
        .with_vertex_normals(&res.vertex_props.map_value(|prop| prop.normal.map(|s| s as f32)))
        .write_to_file("shape.ply")?;


    Ok(())
}
