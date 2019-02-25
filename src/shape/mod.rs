use cgmath::{
    Point3, Vector3,
    prelude::*,
};
use num_traits::{Float, FloatConst};

use crate::{
    cast,
    io::{Error, MemSink, StreamSource, util::TypeWish},
    util::MeshSizeHint,
};


/// A flat round disc that lies in the XY-plane and which normals point upwards
/// (+z).
#[derive(Debug)]
pub struct Disc {
    /// The number of faces generated for the disc. Has to be at least 3 or
    /// else creating a mesh will panic. *Default*: 16.
    pub faces: u32,

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

type VPosType<S> = <<S as MemSink>::VertexPosition as TypeWish>::Float;
type VNormalType<S> = <<S as MemSink>::VertexNormal as TypeWish>::Float;
type FNormalType<S> = <<S as MemSink>::FaceNormal as TypeWish>::Float;

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

        sink.prepare_vertex_positions::<VPosType<S>>(vertex_count)?;
        sink.prepare_vertex_normals::<VNormalType<S>>(vertex_count)?;
        sink.prepare_face_normals::<FNormalType<S>>(face_count)?;

        let vertex_normal = Vector3::<VNormalType<S>>::unit_z();
        let face_normal = Vector3::<FNormalType<S>>::unit_z();

        // Convert our config into the right type
        let radius = cast::lossy::<_, VPosType<S>>(self.radius);
        let center_pos = self.center.map(|s| cast::lossy::<_, VPosType<S>>(s));
        let lit = |v: f32| cast::lossless::<f32, VPosType<S>>(v);


        let center = sink.add_vertex();
        sink.set_vertex_position(center, center_pos);
        sink.set_vertex_normal(center, vertex_normal);

        let first = sink.add_vertex();
        sink.set_vertex_position(first, center_pos + Vector3::new(radius, lit(0.0), lit(0.0)));
        sink.set_vertex_normal(first, vertex_normal);

        // The last vertex we created.
        let mut last = first;

        // Add a new vertex and a new face in each iteration.
        for i in 1..self.faces {
            let angle = (
                cast::rounding::<u32, VPosType<S>>(i)
                    / cast::rounding::<u32, VPosType<S>>(self.faces)
            ) * lit(2.0) * VPosType::<S>::PI();
            let position = center_pos + Vector3::new(
                radius * angle.cos(),
                radius * angle.sin(),
                lit(0.0),
            );

            let v = sink.add_vertex();
            sink.set_vertex_position(v, position);
            sink.set_vertex_normal(v, vertex_normal);

            let f = sink.add_face([center, last, v]);
            sink.set_face_normal(f, face_normal);

            last = v;
        }

        // Add last face (with the first outer vertex)
        let f = sink.add_face([center, last, first]);
        sink.set_face_normal(f, face_normal);

        Ok(())
    }
}
