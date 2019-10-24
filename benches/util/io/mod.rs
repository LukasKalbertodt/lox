use criterion::black_box;

use lox::{
    prelude::*,
    VertexHandle, FaceHandle,
    cgmath::{Point3, Vector3},
    ds::SharedVertexMesh,
    fat::MiniMesh,
    handle::hsize,
    map::DenseMap,
    io::{
        Error, Primitive,
    },
    shape::Sphere,
    util::MeshSizeHint,
};


pub mod ply;
pub mod stl;



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
    fn add_face(&mut self, vertices: &[VertexHandle]) -> Result<FaceHandle, Error> {
        black_box(vertices);

        let out = FaceHandle::new(self.face_count);
        self.face_count += 1;
        Ok(out)
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

/// A sink that puts all vertex positions and vertex normals into the
/// `black_box`, ignores all other properties.
pub struct NullSinkPosVNormal(NullSinkPos);

impl NullSinkPosVNormal {
    pub fn new() -> Self {
        Self(NullSinkPos::new())
    }
}

impl MemSink for NullSinkPosVNormal {
    fn add_vertex(&mut self) -> VertexHandle {
        self.0.add_vertex()
    }
    fn add_face(&mut self, vertices: &[VertexHandle]) -> Result<FaceHandle, Error> {
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


/// A sink that puts all vertex positions and face normals into the
/// `black_box`, ignores all other properties.
pub struct NullSinkPosFNormal(NullSinkPos);

impl NullSinkPosFNormal {
    pub fn new() -> Self {
        Self(NullSinkPos::new())
    }
}

impl MemSink for NullSinkPosFNormal {
    fn add_vertex(&mut self) -> VertexHandle {
        self.0.add_vertex()
    }
    fn add_face(&mut self, vertices: &[VertexHandle]) -> Result<FaceHandle, Error> {
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

    fn prepare_face_normals<N: Primitive>(&mut self, count: hsize) -> Result<(), Error> {
        black_box(count);
        Ok(())
    }
    fn set_face_normal<N: Primitive>(
        &mut self,
        v: FaceHandle,
        normal: Vector3<N>,
    ) {
        black_box(v);
        black_box(normal);
    }
}

pub fn sphere() -> MiniMesh<SharedVertexMesh> {
    MemSink::create_from(Sphere {
        radius: 10.0,
        .. Sphere::default()
    }).expect("couldn't create sphere")
}

#[derive(Empty, MemSink, MemSource)]
#[lox(cast = "lossy")]
pub struct NormalPosMesh {
    #[lox(core_mesh)]
    mesh: SharedVertexMesh,

    #[lox(vertex_position)]
    vertex_positions: DenseMap<VertexHandle, Point3<f32>>,

    #[lox(vertex_normal)]
    vertex_normals: DenseMap<VertexHandle, Vector3<f32>>,
}

pub fn sphere_vnormals() -> NormalPosMesh {
    MemSink::create_from(Sphere {
        radius: 10.0,
        .. Sphere::default()
    }).expect("couldn't create sphere")
}
