//! TODO
//!
//! ...
//!
//!
//! # Floating point precision and performance
//!
//! All shapes will generate positions and normals in `f64` precision. One
//! could assume that it would be better to use the *type wish* of the sink,
//! but this is actually not worth it.
//!
//! Calculating with `f32` instead of `f64` will not have a big performance
//! impact. The main reasons why `f32` can be faster than `f64` on x86-64 are:
//! denser packed data (better cache friendliness) and SIMD code. Since these
//! shape creation algorithms don't store data, the first point is completely
//! irrelevant. Additionally, the algorithms are probably no good candidates
//! for vectorization, rendering the second point irrelevant as well. In case
//! that some of this code is (auto-)vectorized later, we can still measure the
//! performance impact and decide then.
//!
//! Additionally, using the type wishes makes the code way more complex *and*
//! might incur additional performance losses. Since there can be different
//! type wishes for normals and positions, the algorithms would have to
//! calculate everything twice or calculate everything in `f64` and cast
//! internally (which does not technically conform with the casting rigor of
//! the sink). So even if (in the SIMD case mentioned above) we want to offer
//! both `f32` and `f64` processing, it's probably better to add a type
//! parameter to all shape types which defaults to `f64`.

use std::{
    f64::consts,
};

use cgmath::{
    Point3, Vector3,
    prelude::*,
};

use crate::{
    handle::hsize,
    io::{Error, MemSink, StreamSource},
    util::MeshSizeHint,
};


/// A flat round disc that lies in the XY-plane and which normals point upwards
/// (+z).
#[derive(Debug)]
pub struct Disc {
    /// The number of faces generated for the disc. Has to be at least 3 or
    /// else creating a mesh will panic. *Default*: 16.
    pub faces: hsize,

    /// The center point of the disc. *Default*: `[0, 0, 0]`.
    pub center: Point3<f64>,

    /// The outer radius (with ∞ faces, this would be the real radius).
    /// *Default*: 1.0.
    pub radius: f64,
}

impl Default for Disc {
    fn default() -> Self {
        Self {
            faces: 16,
            center: Point3::origin(),
            radius: 1.0,
        }
    }
}

impl StreamSource for Disc {
    fn transfer_to<S: MemSink>(self, sink: &mut S) -> Result<(), Error> {
        assert!(
            self.faces >= 3,
            "trying to build a disc with {} faces (minimum is 3)",
            self.faces,
        );

        // We create vertices in counter clock wise order around the center,
        // the first one starting at [r, 0, 0]. This is looking top down onto
        // the disc:
        //
        //      3 _
        //      |   ‾ – _
        //      |         2
        //      |  B    ⋰  \
        //      |     ⋰     \
        //      |   ⋰   A    \
        //      | ⋰           \
        //      0 ------------ 1
        //
        // The vertex 0 is the `center` vertex, vertex 1 is the first outer one
        // (at [r, 0, 0]) and stored in `first`, vertex 2 is the second one and
        // so on. Face A is the first face, face B the second one etc.

        // Prepare the sink
        let vertex_count = self.faces + 1;
        let face_count = self.faces;

        sink.size_hint(MeshSizeHint {
            vertex_count: Some(vertex_count),
            face_count: Some(face_count),
        });

        sink.prepare_vertex_positions::<f64>(vertex_count)?;
        sink.prepare_vertex_normals::<f64>(vertex_count)?;
        sink.prepare_face_normals::<f64>(face_count)?;

        let vertex_normal = Vector3::<f64>::unit_z();
        let face_normal = Vector3::<f64>::unit_z();

        let center = sink.add_vertex();
        sink.set_vertex_position::<f64>(center, self.center);
        sink.set_vertex_normal::<f64>(center, vertex_normal);

        let first = sink.add_vertex();
        sink.set_vertex_position::<f64>(first, self.center + Vector3::new(self.radius, 0.0, 0.0));
        sink.set_vertex_normal::<f64>(first, vertex_normal);

        // The last vertex we created.
        let mut last = first;

        // Add a new vertex and a new face in each iteration.
        for i in 1..self.faces {
            let angle = ((i as f64) / (self.faces as f64)) * 2.0 * consts::PI;
            let position = self.center + Vector3::new(
                self.radius * angle.cos(),
                self.radius * angle.sin(),
                0.0,
            );

            let v = sink.add_vertex();
            sink.set_vertex_position::<f64>(v, position);
            sink.set_vertex_normal::<f64>(v, vertex_normal);

            let f = sink.add_face([center, last, v]);
            sink.set_face_normal::<f64>(f, face_normal);

            last = v;
        }

        // Add last face (with the first outer vertex)
        let f = sink.add_face([center, last, first]);
        sink.set_face_normal::<f64>(f, face_normal);

        Ok(())
    }
}
