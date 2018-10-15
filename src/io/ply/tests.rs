use failure::Error;

use crate::{
    mesh,
    prelude::*,
    ds::SharedVertexMesh,
    map::{ConstMap, FnMap, VecMap},
};
use super::{Serializer};



fn triangle_mesh() -> (SharedVertexMesh, VecMap<VertexHandle, [f32; 3]>) {
    mesh! {*
        type: SharedVertexMesh,
        vertices: [
            v0: ([0.0f32, 0.0, 0.0]),
            v1: ([3.0, 5.0, 8.0]),
            v2: ([1.942, 152.99, 0.007]),
        ],
        faces: [
            [v0, v1, v2],
        ],
    }
}

#[test]
fn triangle_ascii() -> Result<(), Error> {
    let (mesh, positions) = triangle_mesh();

    let res = Serializer::ascii()
        .into_writer(&mesh, &positions)
        .write_to_memory()?;

    assert_eq_file!(&res, "triangle_ascii.ply");
    Ok(())
}

#[test]
fn triangle_with_comments_ascii() -> Result<(), Error> {
    let (mesh, positions) = triangle_mesh();

    let res = Serializer::ascii()
        .add_comment("My name is Tom")
        .add_comment("Yes we can have multiple comments :)")
        .add_comment(
            "Comments appear in the file in the same order as they are added with `add_comment`"
        )
        .into_writer(&mesh, &positions)
        .write_to_memory()?;

    assert_eq_file!(&res, "triangle_with_comments_ascii.ply");
    Ok(())
}

#[test]
fn triangle_with_extra_vertex_props_ascii() -> Result<(), Error> {
    let (mesh, positions, bar) = mesh! {*
        type: SharedVertexMesh,
        vertices: [
            v0: ([0.0f32, 0.0, 0.0], vec![]),
            v1: ([3.0, 5.0, 8.0], vec![-1i8]),
            v2: ([1.942, 152.99, 0.007], vec![3, 8]),
        ],
        faces: [
            [v0, v1, v2],
        ],
    };

    let res = Serializer::ascii()
        .into_writer(&mesh, &positions)
        .add_vertex_prop("foo", &ConstMap([0.93f64, 0.2, 0.3]))
        .add_vertex_prop("bar", &FnMap(|h| bar.get(h).map(|v| v.into_inner().as_slice())))
        .add_vertex_prop("baz", &ConstMap(-99.123f32))
        .write_to_memory()?;

    assert_eq_file!(&res, "triangle_with_extra_vertex_props_ascii.ply");
    Ok(())
}
