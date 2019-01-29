use cgmath::{
    Point3, Vector3,
    prelude::*,
};

use crate::{
    io::{Error, MemSink, StreamSource},
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


        let center = sink.add_vertex();
        sink.set_vertex_position(center, self.center);

        let first = sink.add_vertex();
        sink.set_vertex_position(first,self.center + Vector3::new(self.radius, 0.0, 0.0));

        // The last vertex we created.
        let mut last = first;

        // Add a new vertex and a new face in each iteration.
        for i in 1..self.faces {
            let angle = (i as f64 / self.faces as f64) * 2.0 * std::f64::consts::PI;
            let position = self.center + Vector3::new(
                self.radius * angle.cos(),
                self.radius * angle.sin(),
                0.0,
            );

            let v = sink.add_vertex();
            sink.set_vertex_position(v, position);
            sink.add_face([center, last, v]);

            last = v;
        }

        // Add last face (with the first outer vertex)
        sink.add_face([center, last, first]);

        Ok(())
    }
}
