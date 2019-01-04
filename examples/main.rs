#![feature(proc_macro_hygiene, never_type)]
#![allow(unused_imports)]

use cgmath::{Point3, Vector3};
use failure::{Error, ResultExt};

use lox::{
    algo,
    ds::{LinkedFaceMesh, SharedVertexMesh},
    io::{stl, ply, MemSink, StreamingSource},
    map::{ConstMap, FnMap, VecMap},
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
    let file = std::env::args().nth(1).expect("no filename given");
    let reader = ply::Reader::open(&file)?;
    let mut m = SimpleMesh {
        mesh: LinkedFaceMesh::empty(),
        vertex_positions: VecMap::empty(),
    };
    reader.transfer_to(&mut m)?;

    let new_pos = algo::cog_smoothing(&m.mesh, &m.vertex_positions);

    ply::Serializer::binary()
        // .into_writer(&m.mesh, &m.vertex_positions)
        .into_writer(&m.mesh, &new_pos)
        .write_to_file("smoothed.ply")?;

    Ok(())
}


struct SimpleMesh {
    mesh: LinkedFaceMesh,
    vertex_positions: VecMap<VertexHandle, Point3<f32>>,
}

impl MemSink for SimpleMesh {
    fn add_vertex(&mut self) -> VertexHandle {
        self.mesh.add_vertex()
    }

    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle {
        self.mesh.add_face(vertices)
    }

    fn set_vertex_position<N: PrimitiveNum>(
        &mut self,
        v: VertexHandle,
        position: Point3<N>,
    ) {
        self.vertex_positions.insert(v, position.map(|s| s.to_f32().unwrap()));
    }
}
