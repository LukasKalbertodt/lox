use failure::Error;

#[allow(unused_imports)]
use lox::{
    MemSink,
    algo,
    ds::{FaceDelegateMesh, SharedVertexMesh, HalfEdgeMesh},
    fat::MiniMesh,
    io::ply,
    map::VecMap,
    prelude::*,
};

type MyMesh = MiniMesh<FaceDelegateMesh>;

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
