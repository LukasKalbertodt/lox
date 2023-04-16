// use std::{env, error::Error};

// use lox::{
//     algo,
//     ds::{
//         HalfEdgeMesh,
//         half_edge::TriConfig,
//     },
//     fat::MiniMesh,
//     io,
// };

// type MyMesh = MiniMesh<HalfEdgeMesh<TriConfig>>;

// fn main() -> Result<(), Box<dyn Error>> {
//     color_backtrace::install();

//     // Quick and dirty CLI argument parsing (not lox related)
//     let num_iterations = env::args().nth(1)
//         .expect("no iteration count given (first argument)")
//         .parse::<u32>()
//         .expect("iteration count (first argument) cannot be parsed as u32");
//     let input_file = env::args().nth(2).expect("no input filename given");
//     let output_file = env::args().nth(3).expect("no output filename given");

//     // Read, smooth, write
//     let mut m: MyMesh = io::read_file(&input_file)?;
//     algo::subdivision::sqrt3(&mut m.mesh, &mut m.vertex_positions, num_iterations);
//     io::write_file(&m, &output_file)?;

//     Ok(())
// }

fn main() {}
