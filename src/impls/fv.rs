use crate::{
    handle::{DefaultIndex, FaceHandle, VertexHandle},
    map::{FaceVecMap, VertexVecMap, PropMap},
    TriMesh,
};


/// A simple face-vertex mesh for triangular faces.
#[derive(Clone)]
pub struct FvTriMesh<VertexT = (), FaceT = ()> {
    vertices: VertexVecMap<VertexT>,
    faces: FaceVecMap<FvTriFace<FaceT>>,
}

#[derive(Clone, Copy)]
pub struct FvTriFace<FaceT> {
    vertices: [VertexHandle; 3],
    prop: FaceT,
}

impl<VertexT, FaceT> FvTriMesh<VertexT, FaceT> {
    pub fn new() -> Self {
        Self {
            vertices: VertexVecMap::new(),
            faces: FaceVecMap::new(),
        }
    }

    // fn face(&self, fh: FaceHandle) -> FvTriFace<FaceT> {
    //     self.faces[fh]
    // }
}

impl<VertexT, FaceT> TriMesh for FvTriMesh<VertexT, FaceT> {
    type VertexProp = VertexT;
    type FaceProp = FaceT;

    fn empty() -> Self where Self: Sized {
        Self::new()
    }

    fn num_faces(&self) -> DefaultIndex {
        self.faces.num_elements()
    }
    fn num_vertices(&self) -> DefaultIndex {
        self.vertices.num_elements()
    }

    fn add_vertex(&mut self, prop: Self::VertexProp) -> VertexHandle {
        self.vertices.push(prop)
    }

    fn add_face(&mut self, vertices: [VertexHandle; 3], prop: Self::FaceProp) -> FaceHandle {
        self.faces.push(FvTriFace { vertices, prop })
    }

    fn vertex_prop(&self, handle: VertexHandle) -> Option<&Self::VertexProp> {
        self.vertices.get(handle)
    }


    fn vertices<'a>(&'a self) -> Box<Iterator<Item = VertexHandle> + 'a> {
        Box::new(self.vertices.handles())
    }

    fn faces<'a>(&'a self) -> Box<Iterator<Item = FaceHandle> + 'a> {
        Box::new(self.faces.handles())
    }

    fn vertices_of_face(
        &self,
        face: FaceHandle,
    ) -> [VertexHandle; 3] {
        self.faces[face].vertices
    }
}
