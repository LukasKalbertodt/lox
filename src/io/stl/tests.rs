#![allow(unused_imports)] // TODO

use failure::Error;

use crate::{
    mesh,
    prelude::*,
    ds::SharedVertexMesh,
    map::{ConstMap, FnMap, VecMap},
};
use super::{RawResult, Reader};


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
