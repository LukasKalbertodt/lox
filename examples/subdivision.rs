use failure::Error;

use lox::{
    algo,
    ds,
    fat::MiniMesh,
    io,
};

type MyMesh = MiniMesh<ds::HalfEdgeMesh>;

fn main() -> Result<(), Error> {
    // Read CLI arguments
    let input_file = std::env::args().nth(1).expect("no input filename given");
    let output_file = std::env::args().nth(2).expect("no output filename given");

    // Read, smooth, write
    let mut m: MyMesh = io::read_file(&input_file)?;
    algo::subdivision::sqrt3(&mut m.mesh, &mut m.vertex_positions);
    io::write_file(&m, &output_file)?;

    Ok(())
}
