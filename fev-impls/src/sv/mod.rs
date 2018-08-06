//! Everything related to the `SharedVertexMesh`.


use fev_core::{
    ExplicitVertex, ExplicitFace, MeshUnsorted, TriMeshBuilder, Mesh,
    prop::{FromProp, IntoProp},
    handle::{DefaultId, FaceHandle, VertexHandle},
    refs::{FaceRef, VertexRef},
};
use fev_map::{FaceVecMap, VertexVecMap, PropStore, PropStoreMut};



// TODO: Manual debug impl
#[derive(Clone, Debug)]
pub struct SharedVertexMesh<VertexT = (), FaceT = ()> {
    vertices: VertexVecMap<VertexT>,
    faces: FaceVecMap<SharedVertexFace<FaceT>>,
}

#[derive(Clone, Copy, Debug)]
struct SharedVertexFace<FaceT> {
    vertices: [VertexHandle; 3],
    prop: FaceT,
}

impl<VertexT, FaceT> SharedVertexMesh<VertexT, FaceT> {
    pub fn new() -> Self {
        Self {
            vertices: VertexVecMap::new(),
            faces: FaceVecMap::new(),
        }
    }

    pub fn add_vertex(&mut self, prop: VertexT) -> VertexHandle {
        self.vertices.push(prop)
    }

    pub fn add_face(&mut self, vertices: [VertexHandle; 3], prop: FaceT) -> FaceHandle {
        self.faces.push(SharedVertexFace { vertices, prop })
    }

    pub fn empty() -> Self where Self: Sized {
        Self::new()
    }

}

impl<VertexT, FaceT> Mesh for SharedVertexMesh<VertexT, FaceT> {
    fn empty() -> Self {
        Self::new()
    }
}


impl<VertexT, FaceT> MeshUnsorted for SharedVertexMesh<VertexT, FaceT> {
    fn vertices_of_face(
        &self,
        face: FaceHandle,
    ) -> [VertexHandle; 3] {
        self.faces[face].vertices
    }
}

impl<VertexT, FaceT> ExplicitVertex for SharedVertexMesh<VertexT, FaceT> {
    type VertexProp = VertexT;

    fn vertex_prop(&self, handle: VertexHandle) -> Option<&Self::VertexProp> {
        self.vertices.get_ref(handle)
    }

    fn vertex_prop_mut(&mut self, handle: VertexHandle) -> Option<&mut Self::VertexProp> {
        self.vertices.get_mut(handle)
    }

    fn num_vertices(&self) -> DefaultId {
        self.vertices.num_elements()
    }

    fn vertices<'s>(&'s self) -> Box<Iterator<Item = VertexRef<Self>> + 's>
    where
        Self: Sized
    {
        Box::new(self.vertices.handles().map(move |handle| {
            VertexRef::new(self, handle)
        }))
    }
}



impl<VertexT, FaceT> ExplicitFace for SharedVertexMesh<VertexT, FaceT> {
    type FaceProp = FaceT;

    fn face_prop(&self, handle: FaceHandle) -> Option<&Self::FaceProp> {
        self.faces.get_ref(handle).map(|f| &f.prop)
    }

    fn face_prop_mut(&mut self, handle: FaceHandle) -> Option<&mut Self::FaceProp> {
        self.faces.get_mut(handle).map(|f| &mut f.prop)
    }

    fn num_faces(&self) -> DefaultId {
        self.faces.num_elements()
    }

    fn faces<'s>(&'s self) -> Box<Iterator<Item = FaceRef<Self>> + 's>
    where
        Self: Sized,
    {
        Box::new(self.faces.handles().map(move |handle| {
            FaceRef::new(self, handle)
        }))
    }
}


impl<VertexT, FaceT, VertexInfoT, FaceInfoT> TriMeshBuilder<VertexInfoT, FaceInfoT>
    for SharedVertexMesh<VertexT, FaceT>
where
    VertexT: FromProp<VertexInfoT>,
    FaceT: FromProp<FaceInfoT>,
{
    fn add_vertex(&mut self, info: VertexInfoT) -> VertexHandle {
        Self::add_vertex(self, info.into_prop())
    }

    fn add_face(&mut self, vertices: [VertexHandle; 3], info: FaceInfoT) {
        Self::add_face(self, vertices, info.into_prop());
    }

    fn hint_num_faces(&mut self, num: usize) {
        // We assume that every face shares at most two vertices with any other
        // face. Thus we allocate for `num` many vertices, too. If this is a
        // very degenerate mesh (very unlikely), we only wasted a bit of
        // memory.
        self.faces.reserve(num);
        self.vertices.reserve(num);
    }
}
