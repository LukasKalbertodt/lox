extern crate stable_vec;


pub mod handle;
pub mod impls;
pub mod maps;
pub mod io;


use handle::{HandleIndex, FaceHandle, EdgeHandle, VertexHandle};


pub trait TriMesh {
    type Idx: HandleIndex;
    // type VertexIter: Iterator<Item = VertexHandle<Self::Idx>>;
    type FaceIter: Iterator<Item = FaceHandle<Self::Idx>>;

    fn num_faces(&self) -> Self::Idx;
    fn num_vertices(&self) -> Self::Idx;

    // fn vertices(&self) -> Self::VertexIter;
    fn faces(&self) -> Self::FaceIter;

    fn vertices_of_face(&self, face: FaceHandle<Self::Idx>)
        -> [VertexHandle<Self::Idx>; 3];
}
