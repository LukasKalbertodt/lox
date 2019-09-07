//! Tests the custom derive for `MemSink`.
//!
//! We use complete path everywhere and avoid imports to make sure when the
//! derive code falsely assumes a symbol is in scope.

#![cfg(feature = "io")]

#[test]
fn disc4() {
    #[derive(lox::Empty, lox::MemSink)]
    struct MyMesh {
        #[lox(core_mesh)]
        mesh: lox::ds::SharedVertexMesh,

        #[lox(vertex_position)]
        vertex_positions: lox::map::DenseMap<lox::VertexHandle, lox::cgmath::Point3<f64>>,

        #[lox(vertex_normal)]
        vertex_normals: lox::map::DenseMap<lox::VertexHandle, lox::cgmath::Vector3<f64>>,

        #[lox(face_normal)]
        face_normals: lox::map::DenseMap<lox::FaceHandle, lox::cgmath::Vector3<f64>>,
    }


    {
        use lox::{
            VertexHandle, FaceHandle,
            prelude::*,
            cgmath::{Vector3, Point3},
            io::MemSink,
            shape::Disc,
        };

        let disc = Disc { faces: 4, .. Disc::default() };
        let m = MyMesh::create_from(disc).unwrap();

        assert_eq!(m.mesh.num_vertices(), 5);
        assert_eq!(m.mesh.num_faces(), 4);

        let vh = |n| VertexHandle::from_usize(n);
        let fh = |n| FaceHandle::from_usize(n);
        let normal = Vector3::unit_z();

        // Note: sin and cos of those multiple of pi/2 are not perfect (they
        // should be 1 or 0), that's why we don't hardcode the values.
        let half_pi = std::f64::consts::FRAC_PI_2;
        assert_eq!(m.vertex_positions[vh(0)], Point3::new(0.0, 0.0, 0.0));
        assert_eq!(
            m.vertex_positions[vh(1)],
            Point3::new((0.0 * half_pi).cos(), (0.0 * half_pi).sin(), 0.0),
        );
        assert_eq!(
            m.vertex_positions[vh(2)],
            Point3::new((1.0 * half_pi).cos(), (1.0 * half_pi).sin(), 0.0),
        );
        assert_eq!(
            m.vertex_positions[vh(3)],
            Point3::new((2.0 * half_pi).cos(), (2.0 * half_pi).sin(), 0.0),
        );
        assert_eq!(
            m.vertex_positions[vh(4)],
            Point3::new((3.0 * half_pi).cos(), (3.0 * half_pi).sin(), 0.0),
        );

        assert_eq!(m.vertex_normals[vh(0)], normal);
        assert_eq!(m.vertex_normals[vh(1)], normal);
        assert_eq!(m.vertex_normals[vh(2)], normal);
        assert_eq!(m.vertex_normals[vh(3)], normal);
        assert_eq!(m.vertex_normals[vh(4)], normal);

        assert_eq!(m.face_normals[fh(0)], normal);
        assert_eq!(m.face_normals[fh(1)], normal);
        assert_eq!(m.face_normals[fh(2)], normal);
        assert_eq!(m.face_normals[fh(3)], normal);
    }

}
