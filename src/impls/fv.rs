use crate::{
    handle::{DefaultIndex, Handle, HandleIndex, FaceHandle, VertexHandle},
    map::{FaceVecMap, VertexVecMap},
    TriMesh,
};


/// A simple face-vertex mesh for triangular faces.
#[derive(Clone)]
pub struct FvTriMesh<Idx: HandleIndex = DefaultIndex> {
    vertices: VertexVecMap<(), Idx>,
    faces: FaceVecMap<FvTriFace<Idx>, Idx>,
}

#[derive(Clone, Copy)]
pub struct FvTriFace<Idx: HandleIndex = DefaultIndex> {
    vertices: [VertexHandle<Idx>; 3],
}

impl<Idx: HandleIndex> FvTriMesh<Idx> {
    pub fn new() -> Self {
        Self {
            vertices: VertexVecMap::new(),
            faces: FaceVecMap::new(),
        }
    }

    pub fn add_vertex(&mut self) -> VertexHandle<Idx> {
        self.vertices.push(())
    }

    pub fn add_face(&mut self, vertices: [VertexHandle<Idx>; 3]) -> FaceHandle<Idx> {
        self.faces.push(FvTriFace { vertices })
    }

    pub fn face(&self, fh: FaceHandle<Idx>) -> FvTriFace<Idx> {
        self.faces[fh]
    }
}

impl<Idx: HandleIndex> TriMesh for FvTriMesh<Idx> {
    type Idx = Idx;
    // type FaceIter = Handles<FaceHandle<Idx>>;

    fn num_faces(&self) -> Self::Idx {
        self.faces.num_elements()
    }
    fn num_vertices(&self) -> Self::Idx {
        self.vertices.num_elements()
    }

    fn vertices<'a>(&'a self) -> Box<Iterator<Item = VertexHandle<Self::Idx>> + 'a> {
        Box::new(self.vertices.handles())
    }

    fn faces<'a>(&'a self) -> Box<Iterator<Item = FaceHandle<Self::Idx>> + 'a> {
        Box::new(self.faces.handles())
    }

    fn vertices_of_face(
        &self,
        face: FaceHandle<Self::Idx>
    ) -> [VertexHandle<Self::Idx>; 3] {
        self.faces[face].vertices
    }
}
