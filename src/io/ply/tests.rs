use failure::Error;

use crate::{
    mesh,
    prelude::*,
    ds::SharedVertexMesh,
};
use super::{Serializer};


#[test]
fn simple_triangle_ascii() -> Result<(), Error> {
    let (mesh, positions) = mesh! {*
        type: SharedVertexMesh,
        vertices: [
            v0: ([0.0f32, 0.0, 0.0]),
            v1: ([3.0, 5.0, 8.0]),
            v2: ([1.942, 152.99, 0.007]),
        ],
        faces: [
            [v0, v1, v2],
        ],
    };

    let res = Serializer::ascii()
        .into_writer(&mesh, &positions)
        .write_to_memory()?;

    assert_eq!(res, include_bytes!("test_files/simple_triangle_ascii.ply") as &[_]);
    Ok(())
}

#[test]
fn simple_triangle_with_comments_ascii() -> Result<(), Error> {
    let (mesh, positions) = mesh! {*
        type: SharedVertexMesh,
        vertices: [
            v0: ([0.0f32, 0.0, 0.0]),
            v1: ([3.0, 5.0, 8.0]),
            v2: ([1.942, 152.99, 0.007]),
        ],
        faces: [
            [v0, v1, v2],
        ],
    };

    let res = Serializer::ascii()
        .add_comment("My name is Tom")
        .add_comment("Yes we can have multiple comments :)")
        .add_comment(
            "Comments appear in the file in the same order as they are added with `add_comment`"
        )
        .into_writer(&mesh, &positions)
        .write_to_memory()?;

    assert_eq!(res, include_bytes!("test_files/simple_triangle_with_comments_ascii.ply") as &[_]);
    Ok(())
}
