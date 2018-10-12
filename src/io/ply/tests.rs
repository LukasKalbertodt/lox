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
        .write_to_stdout()?;

    let res = Serializer::ascii()
        .into_writer(&mesh, &positions)
        .write_to_memory()?;

    assert_eq!(res, b"\
        ply\n\
        format ascii 1.0\n\
        element vertex 3\n\
        property float x\n\
        property float y\n\
        property float z\n\
        element face 1\n\
        property list uchar uint vertex_indices\n\
        end_header\n\
        0 0 0\n\
        3 5 8\n\
        1.942 152.99 0.007\n\
        3 0 1 2\n\
        " as &[u8]
    );

    Ok(())
}
