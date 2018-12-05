#![feature(proc_macro_hygiene, never_type)]
#![allow(unused_imports)]

use cgmath::{Point3, Vector3};
use failure::{Error, ResultExt};

use lox::{
    MeshWithProps,
    ds::SharedVertexMesh,
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
    let mut reader = ply::Reader::open(std::env::args().nth(1).unwrap())?;
    let mut dummy = Dummy { mesh: SharedVertexMesh::empty() };
    reader.transfer_to(&mut dummy);
    // let mut res = ply::RawResult::new();
    // reader.read_raw_into(&mut res)?;
    // println!("{:#?}", res);


    Ok(())
}

struct Dummy {
    mesh: SharedVertexMesh,
}

impl MemSink for Dummy {
    fn add_vertex(&mut self) -> VertexHandle {
        let h = self.mesh.add_vertex();
        println!("add_vertex() -> {:?}", h);
        h
    }
    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle {
        let h = self.mesh.add_face(vertices);
        println!("add_face({:?}) -> {:?}", vertices, h);
        h
    }

    fn set_vertex_position<N: PrimitiveNum>(
        &mut self,
        v: VertexHandle,
        position: Point3<N>,
    ) {
        println!("set_vertex_position({:?}, {:?})", v, position);
    }
}
