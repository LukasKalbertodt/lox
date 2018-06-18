//! Everything related to the `SharedVertexMesh`.


use fev_core::{
    ExplicitVertex,
    ExplicitFace,
    handle::{DefaultId, FaceHandle, VertexHandle},
    refs::{FaceRef, VertexRef},
};
use fev_map::{FaceVecMap, VertexVecMap, PropStore, PropStoreMut};



#[derive(Clone)]
pub struct SharedVertexMesh<VertexT = (), FaceT = ()> {
    vertices: VertexVecMap<VertexT>,
    faces: FaceVecMap<SharedVertexFace<FaceT>>,
}

#[derive(Clone, Copy)]
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

    pub fn vertices_of_face(
        &self,
        face: FaceHandle,
    ) -> [VertexHandle; 3] {
        self.faces[face].vertices
    }

    // fn add_vertex(&mut self, prop: Self::VertexProp) -> VertexHandle {
    //     self.vertices.push(prop)
    // }

    // fn add_face(&mut self, vertices: [VertexHandle; 3], prop: Self::FaceProp) -> FaceHandle {
    //     self.faces.push(SharedVertexFace { vertices, prop })
    // }

    pub fn empty() -> Self where Self: Sized {
        Self::new()
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
        unimplemented!()
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
