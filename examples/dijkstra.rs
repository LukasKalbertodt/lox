use std::env;
use failure::Error;

use lox::{
    prelude::*,
    algo,
    ds::{
        HalfEdgeMesh,
        half_edge::TriConfig,
    },
    handle::VertexHandle,
    fat::MiniMesh,
    map::FnMap,
    io,
};

type MyMesh = MiniMesh<HalfEdgeMesh<TriConfig>>;

fn main() -> Result<(), Error> {
    color_backtrace::install();

    // Quick and dirty CLI argument parsing (not lox related)
    let input_file = env::args().nth(1).expect("no input filename given");
    let output_file = env::args().nth(2).expect("no output filename given");

    // Load mesh and make sure it's not empty.
    let m: MyMesh = io::read_file(&input_file)?;
    if m.mesh.num_vertices() == 0 {
        panic!("The mesh has 0 vertices!");
    }

    // Run Dijkstra
    let vertex_data = algo::dijkstra(&m.mesh, &m.vertex_positions, VertexHandle::new(0));

    // Prepare vertex colors
    let max_distance = vertex_data.values()
        .map(|data| data.distance)
        .filter(|dist| dist.is_finite())
        .max_by(|a, b| a.partial_cmp(&b).unwrap())
        .unwrap();

    let colors = FnMap(|vh| {
        let dist = vertex_data[vh].distance;
        let color = if dist.is_finite() {
            let green = 1.0 - (dist / max_distance);
            [0.0, green, 0.0]
        } else {
            [1.0, 0.0, 0.0]
        };

        Some(color)
    });

    // Write mesh to file, including the vertex colors
    io::write_file(&m.with_vertex_colors(&colors), &output_file)?;

    Ok(())
}
