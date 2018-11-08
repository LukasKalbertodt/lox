use crate::{
    handle::{DefaultInt, FaceHandle, VertexHandle},
    refs::{FaceRef, VertexRef},
    map::{VecMap, PropStoreMut},
};

/// The three basic elements in a polygon mesh.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MeshElement {
    Edge,
    Face,
    Vertex,
}


/// Some kind of polygon mesh.
pub trait Mesh {
    /// Returns an empty mesh instance.
    fn empty() -> Self;
}

/// A triangular mesh: all faces are triangles.
pub trait TriMesh: Mesh {}


// Alternative names:
// - HasVertices
// - ExplicitVertex
// - ContainsVertices
// - VertexIndex
// - WithVerts
pub trait ExplicitVertex {
    fn num_vertices(&self) -> DefaultInt;

    fn add_vertex(&mut self) -> VertexHandle;

    fn vertices<'s>(&'s self) -> Box<dyn Iterator<Item = VertexRef<'s, Self>> + 's>;
    // TODO: visit_mut
    // TODO: iterator over handles
    // TODO: mutable iterator?
}

pub trait ExplicitFace {
    fn num_faces(&self) -> DefaultInt;

    // CCW!
    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle;

    fn faces<'s>(&'s self) -> Box<dyn Iterator<Item = FaceRef<'s, Self>> + 's>;
    // TODO: visit_mut
    // TODO: iterator over handles
    // TODO: mutable iterator?
}


pub trait MeshUnsorted {
    /// Maybe we should return vertex refs? CCW!
    fn vertices_of_face(&self, face: FaceHandle) -> [VertexHandle; 3];
}

pub trait MeshSource {
    type VertexInfo;
    type FaceInfo;

    fn build(self, sink: &mut impl MeshSink<Self::VertexInfo, Self::FaceInfo>) -> Result<(), ()>;
}

pub trait MeshSink<VertexInfoT, FaceInfoT> {
    fn empty() -> Self;
    fn add_vertex(&mut self, info: VertexInfoT) -> Result<VertexHandle, ()>;
    fn add_face(&mut self, vertices: [VertexHandle; 3], info: FaceInfoT) -> Result<FaceHandle, ()>;

    fn build_from(
        source: impl MeshSource<VertexInfo = VertexInfoT, FaceInfo = FaceInfoT>,
    ) -> Result<Self, ()>
    where
        Self: Sized,
    {
        let mut out = Self::empty();
        source.build(&mut out)?;
        Ok(out)
    }
}

#[derive(Debug)]
pub struct MeshWithProps<MeshT, VertexT, FaceT> {
    pub mesh: MeshT,
    pub vertex_props: VecMap<VertexHandle, VertexT>,
    pub face_props: VecMap<FaceHandle, FaceT>,
}


impl<MeshT, VertexT, FaceT> MeshSink<VertexT, FaceT> for MeshWithProps<MeshT, VertexT, FaceT>
where
    MeshT: Mesh + ExplicitVertex + ExplicitFace,
{
    fn empty() -> Self {
        Self {
            mesh: MeshT::empty(),
            vertex_props: VecMap::empty(),
            face_props: VecMap::empty(),
        }
    }

    fn add_vertex(&mut self, info: VertexT) -> Result<VertexHandle, ()> {
        let handle = self.mesh.add_vertex();
        self.vertex_props.insert(handle, info);
        Ok(handle)
    }

    fn add_face(
        &mut self,
        vertices: [VertexHandle; 3],
        info: FaceT,
    ) -> Result<FaceHandle, ()> {
        let handle = self.mesh.add_face(vertices);
        self.face_props.insert(handle, info);
        Ok(handle)
    }
}
