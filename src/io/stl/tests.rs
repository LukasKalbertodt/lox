#![allow(unused_imports)] // TODO

use failure::Error;

use crate::{
    mesh,
    prelude::*,
    ds::SharedVertexMesh,
    map::{ConstMap, FnMap, VecMap},
};
use super::{RawResult, Reader, WriterBuilder};


// ===========================================================================
// ===== Reading
// ===========================================================================
fn check_flat_data(res: &RawResult) {
    assert_eq!(res.triangles[0].normal, [0.0, 0.0, 1.0]);
    assert_eq!(res.triangles[0].vertices, [
        [1.0, 0.0, 0.0],
        [0.5, 0.5, 0.0],
        [0.0, 0.0, 0.0],
    ]);

    assert_eq!(res.triangles[1].normal, [0.0, 0.0, 1.0]);
    assert_eq!(res.triangles[1].vertices, [
        [0.0, 0.0, 0.0],
        [0.5, 0.5, 0.0],
        [0.0, 1.0, 0.0],
    ]);

    assert_eq!(res.triangles[2].normal, [0.0, 0.0, 1.0]);
    assert_eq!(res.triangles[2].vertices, [
        [1.0, 0.0, 0.0],
        [1.0, 1.0, 0.0],
        [0.0, 1.0, 0.0],
    ]);
}


#[test]
fn read_flat_ascii() -> Result<(), Error> {
    let data = include_bytes!("test_files/flat_ascii.stl");
    let res = Reader::new(data as &[u8]).read_raw()?;

    assert_eq!(res.solid_name, Some("MYSOLID".to_string()));
    assert_eq!(res.triangles.len(), 3);

    check_flat_data(&res);

    Ok(())
}

#[test]
fn read_flat_binary() -> Result<(), Error> {
    let data = include_bytes!("test_files/flat_binary.stl");
    let res = Reader::new(data as &[u8]).read_raw()?;

    assert_eq!(res.solid_name, None);
    assert_eq!(res.triangles.len(), 3);

    check_flat_data(&res);

    Ok(())
}

fn check_cube_data(res: &RawResult) {
    // We only check the face at the very start, very end and somewhere in the
    // middle.
    assert_eq!(res.triangles[0].normal, [0.0, 0.0, 1.0]);
    assert_eq!(res.triangles[0].vertices, [
        [-0.5, -0.5, 1.0],
        [ 0.5, -0.5, 1.0],
        [ 0.5,  0.5, 1.0],
    ]);

    assert_eq!(res.triangles[5].normal, [-1.0, 0.0, 0.0]);
    assert_eq!(res.triangles[5].vertices, [
        [-0.5, -0.5, 0.0],
        [-0.5,  0.5, 1.0],
        [-0.5,  0.5, 0.0],
    ]);

    assert_eq!(res.triangles[11].normal, [0.0, 1.0, 0.0]);
    assert_eq!(res.triangles[11].vertices, [
        [-0.5, 0.5, 1.0],
        [ 0.5, 0.5, 0.0],
        [-0.5, 0.5, 0.0],
    ]);
}

#[test]
fn read_cube_ascii() -> Result<(), Error> {
    let data = include_bytes!("test_files/cube_ascii.stl");
    let res = Reader::new(data as &[u8]).read_raw()?;

    assert_eq!(res.solid_name, Some("vcg".to_string()));
    assert_eq!(res.triangles.len(), 12);

    check_cube_data(&res);

    Ok(())
}

#[test]
fn read_cube_binary() -> Result<(), Error> {
    let data = include_bytes!("test_files/cube_binary.stl");
    let res = Reader::new(data as &[u8]).read_raw()?;

    assert_eq!(res.solid_name, Some("peter".to_string()));
    assert_eq!(res.triangles.len(), 12);

    check_cube_data(&res);

    Ok(())
}


// ===========================================================================
// ===== Writing
// ===========================================================================
fn triangle_mesh() -> (
    SharedVertexMesh,
    VecMap<VertexHandle, [f32; 3]>,
    VecMap<FaceHandle, [f32; 3]>,
) {
    mesh! {*
        type: SharedVertexMesh,
        vertices: [
            v0: ([0.0f32, 0.0, 0.0]),
            v1: ([3.0, 5.0, 8.0]),
            v2: ([1.942, 152.99, 0.007]),
        ],
        faces: [
            [v0, v1, v2]: ([0.5, 0.3, 0.1]), // BS normal, but it's fine for the test
        ],
    }
}

#[test]
fn triangle_ascii() -> Result<(), Error> {
    let (mesh, positions, face_normals) = triangle_mesh();

    let res = WriterBuilder::ascii()
        .into_writer(&mesh, &positions)
        .write_to_memory()?;
    assert_eq_file!(&res, "triangle_ascii.stl");

    let res = WriterBuilder::ascii()
        .into_writer(&mesh, &positions)
        .with_face_normals(&face_normals)
        .write_to_memory()?;
    assert_eq_file!(&res, "triangle_ascii_custom_normals.stl");

    Ok(())
}

#[test]
fn triangle_binary() -> Result<(), Error> {
    let (mesh, positions, face_normals) = triangle_mesh();

    let res = WriterBuilder::binary()
        .into_writer(&mesh, &positions)
        .write_to_memory()?;
    assert_eq_file!(&res, "triangle_binary.stl");

    let res = WriterBuilder::binary()
        .into_writer(&mesh, &positions)
        .with_face_normals(&face_normals)
        .write_to_memory()?;
    assert_eq_file!(&res, "triangle_binary_custom_normals.stl");

    Ok(())
}
