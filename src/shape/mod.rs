use cgmath::{
    Point3, Vector3,
    prelude::*,
};

use crate::{
    MeshSource, MeshSink,
};


#[derive(Debug)]
#[repr(C)]
pub struct ShapeVertexInfo {
    pub position: Point3<f64>,
    pub normal: Vector3<f64>,
}

#[derive(Debug)]
#[repr(C)]
pub struct ShapeFaceInfo {
    pub normal: Vector3<f64>,
}

/// A flat round disc that lies in the XY-plane and which normals point upwards
/// (+z).
#[derive(Debug)]
pub struct Disc {
    /// The number of faces generated for the disc. *Default*: 16.
    pub faces: u32,

    /// The center point of the disc. *Default*: `[0, 0, 0]`.
    pub center: Point3<f64>,

    /// The outer radius (with ∞ faces, this would be the real radius).
    /// *Default*: 1.
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

impl MeshSource for Disc {
    type VertexInfo = ShapeVertexInfo;
    type FaceInfo = ShapeFaceInfo;

    fn build(self, sink: &mut impl MeshSink<Self::VertexInfo, Self::FaceInfo>) -> Result<(), ()> {
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

        // The normal for all vertices and faces
        let normal = Vector3::new(0.0, 0.0, 1.0);

        let center = sink.add_vertex(ShapeVertexInfo {
            position: self.center,
            normal,
        })?;
        let first = sink.add_vertex(ShapeVertexInfo {
            position: self.center + Vector3::new(self.radius, 0.0, 0.0),
            normal,
        })?;

        // The last vertex we created.
        let mut last = first;

        // Add a new vertex and a new face in each iteration.
        for i in 1..self.faces {
            let angle = (i as f64 / self.faces as f64) * 2.0 * std::f64::consts::PI;
            let position = self.center + Vector3::new(
                self.radius * angle.cos(),
                self.radius * angle.sin(),
                0.0
            );

            let v = sink.add_vertex(ShapeVertexInfo { position, normal })?;
            sink.add_face([center, last, v], ShapeFaceInfo { normal })?;

            last = v;
        }

        // Add last face (with the first outer vertex)
        sink.add_face([center, last, first], ShapeFaceInfo { normal })?;


        Ok(())
    }
}
