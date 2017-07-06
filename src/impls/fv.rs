use handle::{DefaultIndex, HandleIndex, FaceHandle, VertexHandle};
use maps::FaceMap;


/// A simple face-vertex mesh for triangular faces.
#[derive(Clone)]
pub struct FvTriMesh<Idx: HandleIndex = DefaultIndex> {
    faces: FaceMap<FvTriFace<Idx>, Idx>,
    num_vertices: usize,
}

#[derive(Clone, Copy)]
pub struct FvTriFace<Idx: HandleIndex = DefaultIndex> {
    a: VertexHandle<Idx>,
    b: VertexHandle<Idx>,
    c: VertexHandle<Idx>,
}

impl<Idx: HandleIndex> FvTriMesh<Idx> {
    pub fn new() -> Self {
        Self {
            faces: FaceMap::new(),
            num_vertices: 0,
        }
    }

    pub fn add_vertex(&mut self) -> VertexHandle<Idx> {
        let out = self.num_vertices.into();
        self.num_vertices += 1;

        out
    }

    pub fn add_face(&mut self, vertices: [VertexHandle<Idx>; 3]) -> FaceHandle<Idx> {
        self.faces.push(FvTriFace {
            a: vertices[0],
            b: vertices[1],
            c: vertices[2],
        })
    }

    pub fn face(&self, fh: FaceHandle<Idx>) -> FvTriFace<Idx> {
        self.faces[fh]
    }
}
