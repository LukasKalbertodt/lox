use criterion::black_box;

use lox::{
    prelude::*,
    cgmath::{Point3, Vector3},
    handle::hsize,
    VertexHandle, FaceHandle,
    io::{
        Error, Primitive,
    },
    util::MeshSizeHint,
};


pub mod ply;



/// A sink that puts all vertex positions into the `black_box`, ignores all
/// other properties.
pub struct NullSinkPos {
    vertex_count: hsize,
    face_count: hsize,
}

impl NullSinkPos {
    pub fn new() -> Self {
        Self {
            vertex_count: 0,
            face_count: 0,
        }
    }
}

impl MemSink for NullSinkPos {
    fn add_vertex(&mut self) -> VertexHandle {
        let out = VertexHandle::new(self.vertex_count);
        self.vertex_count += 1;
        out
    }
    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle {
        black_box(vertices);

        let out = FaceHandle::new(self.face_count);
        self.face_count += 1;
        out
    }

    fn size_hint(&mut self, hint: MeshSizeHint) {
        black_box(hint);
    }

    fn prepare_vertex_positions<N: Primitive>(&mut self, count: hsize) -> Result<(), Error> {
        black_box(count);
        Ok(())
    }
    fn set_vertex_position<N: Primitive>(
        &mut self,
        v: VertexHandle,
        position: Point3<N>,
    ) {
        black_box(v);
        black_box(position);
    }
}

/// A sink that puts all vertex positions and vertex normals  into the
/// `black_box`, ignores all other properties.
pub struct NullSinkPosNormal(NullSinkPos);

impl NullSinkPosNormal {
    pub fn new() -> Self {
        Self(NullSinkPos::new())
    }
}

impl MemSink for NullSinkPosNormal {
    fn add_vertex(&mut self) -> VertexHandle {
        self.0.add_vertex()
    }
    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle {
        self.0.add_face(vertices)
    }

    fn size_hint(&mut self, hint: MeshSizeHint) {
        self.0.size_hint(hint)
    }

    fn prepare_vertex_positions<N: Primitive>(&mut self, count: hsize) -> Result<(), Error> {
        self.0.prepare_vertex_positions::<N>(count)
    }
    fn set_vertex_position<N: Primitive>(
        &mut self,
        v: VertexHandle,
        position: Point3<N>,
    ) {
        self.0.set_vertex_position::<N>(v, position)
    }

    fn prepare_vertex_normals<N: Primitive>(&mut self, count: hsize) -> Result<(), Error> {
        black_box(count);
        Ok(())
    }
    fn set_vertex_normal<N: Primitive>(
        &mut self,
        v: VertexHandle,
        normal: Vector3<N>,
    ) {
        black_box(v);
        black_box(normal);
    }
}
