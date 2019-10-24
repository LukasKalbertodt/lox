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
    mem,
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


/// A regular Tetrahedron: pyramide with triangle-base and pointy top, all
/// sides are equilateral triangles.
#[derive(Debug, Clone, Copy)]
pub struct Tetrahedron {
    /// The center (centroid of vertices) point of the tetrahedron. *Default*:
    /// `[0, 0, 0]`.
    pub center: Point3<f64>,

    /// The outer "radius" (distance between vertices and center) *Default*:
    /// 1.0.
    pub radius: f64,
}

impl Default for Tetrahedron {
    fn default() -> Self {
        Self {
            center: Point3::origin(),
            radius: 1.0,
        }
    }
}

impl StreamSource for Tetrahedron {
    fn transfer_to<S: MemSink>(self, sink: &mut S) -> Result<(), Error> {
        // ASCII-art:
        //
        //             (T)
        //            / | \
        //           /  |  \
        //          /   |   \
        //         /   (B)   \
        //        / ⋰     ⋱  \
        //       (C) ------- (A)
        //
        // T is at (0, 0, r), A is at (_, 0, _).
        //

        // Prepare the sink
        let vertex_count = 4;
        let face_count = 4;

        sink.size_hint(MeshSizeHint {
            vertex_count: Some(vertex_count),
            face_count: Some(face_count),
        });

        sink.prepare_vertex_positions::<f64>(vertex_count)?;
        // sink.prepare_face_normals::<f64>(face_count)?;

        // Add vertices
        let top = sink.add_vertex();
        sink.set_vertex_position::<f64>(top, self.center + Vector3::new(0.0, 0.0, self.radius));

        let apos = Vector3::new((8.0f64 / 9.0).sqrt(), 0.0, -1.0 / 3.0);
        let bpos = Vector3::new(-(2.0f64 / 9.0).sqrt(), (2.0f64 / 3.0).sqrt(), -1.0 / 3.0);
        let cpos = Vector3::new(-(2.0f64 / 9.0).sqrt(), -(2.0f64 / 3.0).sqrt(), -1.0 / 3.0);

        let a = sink.add_vertex();
        let b = sink.add_vertex();
        let c = sink.add_vertex();
        sink.set_vertex_position::<f64>(a, self.center + self.radius * apos);
        sink.set_vertex_position::<f64>(b, self.center + self.radius * bpos);
        sink.set_vertex_position::<f64>(c, self.center + self.radius * cpos);

        // Add faces
        sink.add_triangle([a, c, b])?;
        sink.add_triangle([a, b, top])?;
        sink.add_triangle([b, c, top])?;
        sink.add_triangle([c, a, top])?;

        Ok(())
    }
}



/// A flat square that lies in the XY-plane, with its normals pointing upwards
/// (+z).
#[derive(Debug)]
pub struct Square {
    /// The number of faces generated per side. Has to be at least 1 or else
    /// creating a mesh will panic. The resulting mesh has `2 *
    /// faces_per_side²` many triangular faces. *Default*: 1.
    pub faces_per_side: hsize,

    /// The center point of the square. *Default*: `[0, 0, 0]`.
    pub center: Point3<f64>,

    /// The side length. *Default*: 1.0.
    pub side_length: f64,
}

impl Default for Square {
    fn default() -> Self {
        Self {
            faces_per_side: 1,
            center: Point3::origin(),
            side_length: 1.0,
        }
    }
}


impl StreamSource for Square {
    fn transfer_to<S: MemSink>(self, sink: &mut S) -> Result<(), Error> {
        assert!(
            self.faces_per_side >= 1,
            "trying to build a square with 0 faces per side (minimum is 1)",
        );

        // This is what we will build:
        //
        //  3   x ---- x ---- x ---- x
        //      |      |      |      |
        //      |      |      |      |
        //  2   x ---- x ---- x ---- x
        //      |      |      |      |
        //      |      |      |      |
        //  1   x ---- x ---- x ---- x
        //      |      |      |      |
        //      |      |      |      |
        //  0   x ---- x ---- x ---- x
        //  y
        //   x  0      1      2      3
        //
        //

        // Prepare the sink
        let vertex_count = (self.faces_per_side + 1).pow(2);
        let face_count = 2 * self.faces_per_side.pow(2);

        sink.size_hint(MeshSizeHint {
            vertex_count: Some(vertex_count),
            face_count: Some(face_count),
        });

        sink.prepare_vertex_positions::<f64>(vertex_count)?;
        sink.prepare_vertex_normals::<f64>(vertex_count)?;
        sink.prepare_face_normals::<f64>(face_count)?;

        let normal = Vector3::<f64>::unit_z();
        let face_length = self.side_length / self.faces_per_side as f64;

        // Here we will store the vertex of the last and current line
        let mut last_line = Vec::with_capacity(self.faces_per_side as usize + 1);
        let mut curr_line = Vec::with_capacity(self.faces_per_side as usize + 1);

        // Iterate through the vertices we need to add
        for line in 0..=self.faces_per_side {
            for col in 0..=self.faces_per_side {
                let position = self.center + Vector3::new(
                    (col as f64 * face_length) - (self.side_length / 2.0),
                    (line as f64 * face_length) - (self.side_length / 2.0),
                    0.0,
                );

                let vh = sink.add_vertex();
                curr_line.push(vh);
                sink.set_vertex_position::<f64>(vh, position);
                sink.set_vertex_normal::<f64>(vh, normal);

                // If we have at least one vertex above us and left of us, we
                // can add faces.
                if line > 0 && col > 0 {
                    //
                    //  |         ...
                    //  x ---- v
                    //  |    / |
                    //  |   /  |
                    //  |  /   |
                    //  | /    |
                    //  x ---- x--
                    //
                    let col = col as usize;
                    let fa = sink.add_triangle([vh, curr_line[col - 1], last_line[col - 1]])?;
                    let fb = sink.add_triangle([vh, last_line[col - 1], last_line[col]])?;

                    sink.set_face_normal::<f64>(fa, normal);
                    sink.set_face_normal::<f64>(fb, normal);
                }
            }

            mem::swap(&mut last_line, &mut curr_line);
            curr_line.clear();
        }

        Ok(())
    }
}


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

            let f = sink.add_triangle([center, last, v])?;
            sink.set_face_normal::<f64>(f, face_normal);

            last = v;
        }

        // Add last face (with the first outer vertex)
        let f = sink.add_triangle([center, last, first])?;
        sink.set_face_normal::<f64>(f, face_normal);

        Ok(())
    }
}


/// A UV sphere whose poles point towards +z and -z.
#[derive(Debug)]
pub struct Sphere {
    /// The center of the sphere. *Default*: `[0, 0, 0]`.
    pub center: Point3<f64>,

    /// The radius of the sphere (the distance from the vertices to the
    /// center). *Default*: 1.0.
    pub radius: f64,

    /// Number of latitudes lines (excluding poles). *Default*: 16.
    ///
    /// These are the lines parallel to the equator. A value of 1 means that
    /// there is just the equator which is connected directly to both poles.
    ///
    /// Value must not be 0 or else the `transfer_to` call will panic.
    pub num_latitudes: hsize,

    /// Number of longitude lines. *Default*: 24.
    ///
    /// These are the lines going perpendicular to the equator and going
    /// through both poles. A value of `n` means that each latitude line (for
    /// example: the equator) contains `n` vertices and `n` edges.
    ///
    /// Value must be greater than or equal to 3 or else the `transfer_to` call
    /// will panic.
    pub num_longitudes: hsize,
}

impl Default for Sphere {
    fn default() -> Self {
        Self {
            center: Point3::origin(),
            radius: 1.0,
            num_latitudes: 16,
            num_longitudes: 24,
        }
    }
}

impl StreamSource for Sphere {
    fn transfer_to<S: MemSink>(self, sink: &mut S) -> Result<(), Error> {
        assert!(
            self.num_latitudes >= 1,
            "trying to build a sphere with {} latitudes (minimum is 1)",
            self.num_latitudes,
        );
        assert!(
            self.num_longitudes >= 3,
            "trying to build a sphere with {} longitudes (minimum is 3)",
            self.num_longitudes,
        );

        // Calculate vertex and face count.
        // - Vertices: we have two poles plus a vertex whenever a latitude and
        //   longitude line meet.
        // - Faces: on each pole (times 2) we have one triangle for each
        //   longitude line. Adding to that is a quad face (two triangles) for
        //   each longitude line between two adjacent latitude lines (that's
        //   the minus 1).
        let vertex_count = 2 + self.num_latitudes * self.num_longitudes;
        let face_count = 2 * self.num_longitudes
            + 2 * ((self.num_latitudes - 1) * self.num_longitudes);

        // Prepare the sink
        sink.size_hint(MeshSizeHint {
            vertex_count: Some(vertex_count),
            face_count: Some(face_count),
        });
        sink.prepare_vertex_positions::<f64>(vertex_count)?;
        sink.prepare_vertex_normals::<f64>(vertex_count)?;

        // ----- Create mesh -------------------------------------------------

        // Stores the vertices of the last latitude that was added
        let mut last_latitude_points = Vec::new();

        let num_longs = self.num_longitudes as usize;


        // Add north pole
        let north_pole = sink.add_vertex();
        {
            let normal = Vector3::unit_z();
            sink.set_vertex_position::<f64>(north_pole, self.center + normal * self.radius);
            sink.set_vertex_normal::<f64>(north_pole, normal);
        }

        // Add latitude points (and face between latitudes + faces between
        // north pole and the first latitude).
        for lat in 0..self.num_latitudes {
            let theta = consts::PI * ((lat + 1) as f64) / ((self.num_latitudes + 1) as f64);

            // Add vertices
            let mut new_latitude_points = Vec::with_capacity(self.num_longitudes as usize);
            for long in 0..self.num_longitudes {
                let phi = 2.0 * consts::PI * (long as f64) / (self.num_longitudes as f64);

                let normal = Vector3::new(
                    theta.sin() * phi.cos(),
                    theta.sin() * phi.sin(),
                    theta.cos(),
                );

                let v = sink.add_vertex();
                sink.set_vertex_position::<f64>(v, self.center + normal * self.radius);
                sink.set_vertex_normal::<f64>(v, normal);
                new_latitude_points.push(v);
            }

            // Add faces
            if lat == 0 {
                // Connect to north pole
                for i in 0..num_longs {
                    sink.add_triangle([
                        north_pole,
                        new_latitude_points[i],
                        new_latitude_points[(i + 1) % num_longs],
                    ])?;
                }
            } else {
                // Connect to last latitude line
                for i in 0..num_longs {
                    let last_0 = last_latitude_points[i];
                    let last_1 = last_latitude_points[(i + 1) % num_longs];
                    let new_0 = new_latitude_points[i];
                    let new_1 = new_latitude_points[(i + 1) % num_longs];

                    sink.add_triangle([last_0, new_1, last_1])?;
                    sink.add_triangle([last_0, new_0, new_1])?;
                }
            }

            last_latitude_points = new_latitude_points;
        }

        // Add south pole and adjacent faces
        let south_pole = sink.add_vertex();
        {
            let normal = -Vector3::unit_z();
            sink.set_vertex_position::<f64>(south_pole, self.center + normal * self.radius);
            sink.set_vertex_normal::<f64>(south_pole, normal);
        }

        for i in 0..num_longs {
            sink.add_triangle([
                south_pole,
                last_latitude_points[(i + 1) % num_longs],
                last_latitude_points[i],
            ])?;
        }

        Ok(())
    }
}
