use cgmath::Point3;
use failure::Error;

#[allow(unused_imports)]
use lox::{
    algo,
    ds::{FaceDelegateMesh, SharedVertexMesh, HalfEdgeMesh},
    io::ply,
    map::VecMap,
    prelude::*,
};


fn main() -> Result<(), Error> {
    // Read CLI arguments
    let input_file = std::env::args().nth(1).expect("no input filename given");
    let output_file = std::env::args().nth(2).expect("no output filename given");

    // Read, smooth, write
    let mut m: MyMesh = ply::read(&input_file)?;
    algo::sqrt3_subdivision(&mut m.mesh, &mut m.vertex_positions);
    ply::write(&output_file, &m)?;

    Ok(())
}


#[derive(Empty)]
struct MyMesh {
    mesh: HalfEdgeMesh,
    vertex_positions: VecMap<VertexHandle, Point3<f32>>,
}










// --- This stuff is boilerplate code that won't be necessary in the future ---

#[allow(unused_imports)]
use lox::{
    handle::hsize,
    io::{MemSink, MemSource, PrimitiveType, Primitive},
    math::PrimitiveNum,
};

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

impl MemSource for MyMesh {
    type CoreMesh = HalfEdgeMesh;
    fn core_mesh(&self) -> &Self::CoreMesh {
        &self.mesh
    }
    fn vertex_position_type(&self) -> Option<PrimitiveType> {
        Some(PrimitiveType::Float32)
    }
    fn vertex_position<T: Primitive>(&self, v: VertexHandle) -> Point3<T> {
        self.vertex_positions[v].map(|s| T::from(s).unwrap())
    }
}
