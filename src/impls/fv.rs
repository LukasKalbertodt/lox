use handle::{DefaultIndex, Handle, HandleIndex, FaceHandle, VertexHandle};
use maps::{FaceVecMap, Handles};
use TriMesh;


/// A simple face-vertex mesh for triangular faces.
#[derive(Clone)]
pub struct FvTriMesh<Idx: HandleIndex = DefaultIndex> {
    faces: FaceVecMap<FvTriFace<Idx>, Idx>,
    num_vertices: Idx,
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
            faces: FaceVecMap::new(),
            num_vertices: Idx::from_usize(0),
        }
    }

    pub fn add_vertex(&mut self) -> VertexHandle<Idx> {
        let out = VertexHandle::from_idx(self.num_vertices);
        self.num_vertices = self.num_vertices.next();

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

impl<Idx: HandleIndex> TriMesh for FvTriMesh<Idx> {
    type Idx = Idx;
    type FaceIter = Handles<FaceHandle<Idx>>;

    fn num_faces(&self) -> Self::Idx {
        self.faces.num_elements()
    }
    fn num_vertices(&self) -> Self::Idx {
        self.num_vertices
    }

    fn faces(&self) -> Self::FaceIter {
        self.faces.handles()
    }

    fn vertices_of_face(
        &self,
        face: FaceHandle<Self::Idx>
    ) -> [VertexHandle<Self::Idx>; 3] {
        let f = self.faces[face];
        [f.a, f.b, f.c]
    }
}
