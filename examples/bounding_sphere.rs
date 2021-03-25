use std::{
    env,
    error::Error,
    path::Path,
};

use lox::{
    prelude::*,
    algo,
    cgmath::Point3,
    ds::SharedVertexMesh,
    fat::MiniMesh,
    io,
    shape::Sphere,
};

type MyMesh = MiniMesh<SharedVertexMesh>;

fn main() -> Result<(), Box<dyn Error>> {
    color_backtrace::install();

    // Quick and dirty CLI argument parsing (not lox related)
    let input_file = env::args().nth(1).expect("no input filename given");

    // Load mesh and make sure it's not empty.
    let MyMesh { vertex_positions, .. } = io::read_file(&input_file)?;
    if vertex_positions.is_empty() {
        panic!("The mesh is empty!");
    }

    let bounding_sphere = algo::bounding::ritter_sphere(vertex_positions.values().copied());
    println!("Ritter: {:#?}", bounding_sphere);
    write_sphere(&bounding_sphere, &input_file, "ritter")?;

    let bounding_sphere = algo::bounding::fast_sphere(vertex_positions.values().copied());
    println!("Fast: {:#?}", bounding_sphere);
    write_sphere(&bounding_sphere, &input_file, "fast")?;

    Ok(())
}

fn write_sphere(
    bounding_sphere: &algo::bounding::BoundingSphere<Point3<f32>>,
    input_file: &str,
    name: &str,
) -> Result<(), Box<dyn Error>> {
    // Write the bounding sphere into file
    let mesh_out = MyMesh::create_from(Sphere {
        radius: bounding_sphere.radius.into(),
        center: bounding_sphere.center.map(|s| s.into()),
        .. Sphere::default()
    })?;

    let input_file = Path::new(input_file);
    let output_file = format!(
        "{}-{}-bounding-sphere.{}",
        input_file.file_stem().map(|s| s.to_str().unwrap()).unwrap(),
        name,
        input_file.extension().map(|s| s.to_str().unwrap()).unwrap_or("ply"),
    );
    io::write_file(&mesh_out, &output_file)?;

    Ok(())
}
