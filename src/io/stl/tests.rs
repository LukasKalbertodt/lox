use cgmath::Point3;
use failure::Error;

use crate as lox;
use crate::{
    mesh,
    prelude::*,
    cast::NoCast,
    fat::MiniMesh,
    ds::SharedVertexMesh,
    io::IsFormat,
    map::VecMap,
};
use super::{RawStorage, Reader, Config, is_file_start};


// ===========================================================================
// ===== Utilities
// ===========================================================================
#[test]
fn test_is_file_start() {
    assert_eq!(is_file_start(b""), IsFormat::No);
    assert_eq!(is_file_start(b"solix"), IsFormat::No);

    assert_eq!(is_file_start(&[0u8; 90]), IsFormat::Maybe);

    assert_eq!(is_file_start(b"solid"), IsFormat::Probably);
    assert_eq!(is_file_start(b"solid foo"), IsFormat::Probably);
}


// ===========================================================================
// ===== Reading
// ===========================================================================
fn check_flat_data(res: &RawStorage) {
    assert_eq!(res.triangles.len(), 3);

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
    let input = include_test_file!("flat_ascii.stl");
    let res = Reader::new(input)?.into_raw_storage()?;

    assert_eq!(res.solid_name, Some("MYSOLID".to_string()));
    check_flat_data(&res);

    Ok(())
}

#[test]
fn read_flat_binary() -> Result<(), Error> {
    let input = include_test_file!("flat_binary.stl");
    let res = Reader::new(input)?.into_raw_storage()?;

    assert_eq!(res.solid_name, None);
    check_flat_data(&res);

    Ok(())
}

fn check_cube_data(res: &RawStorage) {
    assert_eq!(res.triangles.len(), 12);

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
    let input = include_test_file!("cube_ascii.stl");
    let res = Reader::new(input)?.into_raw_storage()?;

    assert_eq!(res.solid_name, Some("vcg".to_string()));
    check_cube_data(&res);

    Ok(())
}

#[test]
fn read_cube_binary() -> Result<(), Error> {
    let input = include_test_file!("cube_binary.stl");
    let res = Reader::new(input)?.into_raw_storage()?;

    assert_eq!(res.solid_name, Some("peter".to_string()));
    check_cube_data(&res);

    Ok(())
}

// TODO:
// - reading with unification (into MiniMesh)
// - reading with half full mesh (and maybe mesh that assigns strange handles)

// ===========================================================================
// ===== Writing
// ===========================================================================
fn triangle_mesh() -> (
    SharedVertexMesh,
    VecMap<VertexHandle, Point3<f32>>,
    VecMap<FaceHandle, [f32; 3]>,
) {
    mesh! {
        type: SharedVertexMesh,
        vertices: [
            v0: Point3::new(0.0, 0.0, 0.0),
            v1: Point3::new(3.0, 5.0, 8.0),
            v2: Point3::new(1.942, 152.99, 0.007),
        ],
        faces: [
            [v0, v1, v2]: ([0.5, 0.3, 0.1]), // BS normal, but it's fine for the test
        ],
    }
}

fn to_mem(config: Config, src: &impl MemSource) -> Result<Vec<u8>, Error> {
    let mut out = Vec::new();
    config.into_writer(&mut out).transfer_from(src)?;
    Ok(out)
}

#[test]
fn write_triangle_ascii() -> Result<(), Error> {
    let (mesh, vertex_positions, face_normals) = triangle_mesh();
    let m = MiniMesh { mesh, vertex_positions };

    let res = to_mem(Config::ascii(), &m)?;
    assert_eq_file!(&res, "triangle_ascii.stl");

    let res = to_mem(Config::ascii(), &m.with_face_normals::<NoCast, _>(&face_normals))?;
    assert_eq_file!(&res, "triangle_ascii_custom_normals.stl");

    Ok(())
}

#[test]
fn write_triangle_binary() -> Result<(), Error> {
    let (mesh, vertex_positions, face_normals) = triangle_mesh();
    let m = MiniMesh { mesh, vertex_positions };

    let res = to_mem(Config::binary(), &m)?;
    assert_eq_file!(&res, "triangle_binary.stl");

    let res = to_mem(Config::binary(), &m.with_face_normals::<NoCast, _>(&face_normals))?;
    assert_eq_file!(&res, "triangle_binary_custom_normals.stl");

    Ok(())
}

// TODO:
// - multiple faces
// - reading with mesh with some deleted elements
