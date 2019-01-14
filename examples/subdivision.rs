use cgmath::Point3;
use failure::Error;

#[allow(unused_imports)]
use lox::{
    algo,
    ds::{FaceDelegateMesh, SharedVertexMesh, HalfEdgeMesh},
    io::{ply, MemSink},
    map::VecMap,
    math::PrimitiveNum,
    prelude::*,
};


fn main() -> Result<(), Error> {
    // Read CLI arguments
    let input_file = std::env::args().nth(1).expect("no input filename given");
    let output_file = std::env::args().nth(2).expect("no output filename given");

    // Read, smooth, write
    let mut m: MyMesh = ply::read(&input_file)?;
    algo::sqrt3_subdivision(&mut m.mesh, &mut m.vertex_positions);

    ply::Serializer::binary()
        .into_writer(&m.mesh, &m.vertex_positions)
        .write_to_file(&output_file)?;


    Ok(())
}


#[derive(Empty)]
struct MyMesh {
    mesh: HalfEdgeMesh,
    vertex_positions: VecMap<VertexHandle, Point3<f32>>,
}

impl MemSink for MyMesh {
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
