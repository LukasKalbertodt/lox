use failure::Error;

#[allow(unused_imports)]
use lox::{
    MemSink,
    algo,
    ds::{FaceDelegateMesh, SharedVertexMesh, HalfEdgeMesh},
    fat::MiniMesh,
    io,
    map::VecMap,
    prelude::*,
};

type MyMesh = MiniMesh<FaceDelegateMesh>;

fn main() -> Result<(), Error> {
    // Read CLI arguments
    let input_file = std::env::args().nth(1).expect("no input filename given");
    let output_file = std::env::args().nth(2).expect("no output filename given");

    // Read, smooth, write
    let mut m: MyMesh = io::read_file(&input_file)?;
    algo::sqrt3_subdivision(&mut m.mesh, &mut m.vertex_positions);
    io::write(&output_file, &m)?;

    Ok(())
}
