use crate::{
    handle::{DefaultIndex, FaceHandle, VertexHandle},
    map::{FaceVecMap, VertexVecMap},
    TriMesh,
};


/// A simple face-vertex mesh for triangular faces.
#[derive(Clone)]
pub struct FvTriMesh {
    vertices: VertexVecMap<()>,
    faces: FaceVecMap<FvTriFace>,
}

#[derive(Clone, Copy)]
pub struct FvTriFace {
    vertices: [VertexHandle; 3],
}

impl FvTriMesh {
    pub fn new() -> Self {
        Self {
            vertices: VertexVecMap::new(),
            faces: FaceVecMap::new(),
        }
    }

    pub fn face(&self, fh: FaceHandle) -> FvTriFace {
        self.faces[fh]
    }
}

impl TriMesh for FvTriMesh {
    fn empty() -> Self where Self: Sized {
        Self::new()
    }

    fn num_faces(&self) -> DefaultIndex {
        self.faces.num_elements()
    }
    fn num_vertices(&self) -> DefaultIndex {
        self.vertices.num_elements()
    }

    fn add_vertex(&mut self) -> VertexHandle {
        self.vertices.push(())
    }

    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle {
        self.faces.push(FvTriFace { vertices })
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
