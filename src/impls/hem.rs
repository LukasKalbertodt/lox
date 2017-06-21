use stable_vec::StableVec;
use handle::{DefaultIndex, HandleIndex, FaceHandle, EdgeHandle, VertexHandle};


#[derive(Clone)]
pub struct HalfEdgeMesh<Idx: HandleIndex = DefaultIndex> {
    faces: StableVec<HalfEdgeFace<Idx>>,
    edges: StableVec<HalfEdge<Idx>>,
    vertices: StableVec<HalfEdgeVertex<Idx>>,
}

#[derive(Clone)]
struct HalfEdgeFace<Idx: HandleIndex> {
    edge: EdgeHandle<Idx>,
}

#[derive(Clone)]
struct HalfEdge<Idx: HandleIndex> {
    target: VertexHandle<Idx>,
    twin: EdgeHandle<Idx>,
    next: EdgeHandle<Idx>,
    face: Option<FaceHandle<Idx>>,
}

#[derive(Clone)]
struct HalfEdgeVertex<Idx: HandleIndex> {
    outgoing: Option<EdgeHandle<Idx>>,
}

impl<Idx: HandleIndex> HalfEdgeVertex<Idx> {
    fn lonely() -> Self {
        Self {
            outgoing: None,
        }
    }
}


impl<Idx: HandleIndex> HalfEdgeMesh<Idx> {
    pub fn new() -> Self {
        Self {
            faces: StableVec::new(),
            edges: StableVec::new(),
            vertices: StableVec::new(),
        }
    }

    pub fn add_vertex(&mut self) -> VertexHandle<Idx> {
        let idx = self.vertices.push(HalfEdgeVertex::lonely());
        VertexHandle::from_usize(idx)
    }

    pub fn num_faces(&self) -> Idx {
        Idx::from_usize(self.faces.num_elements())
    }

    pub fn num_edges(&self) -> Idx {
        Idx::from_usize(self.edges.num_elements())
    }

    pub fn num_vertices(&self) -> Idx {
        Idx::from_usize(self.vertices.num_elements())
    }
}
