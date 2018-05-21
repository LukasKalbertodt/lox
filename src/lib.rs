#![feature(crate_in_paths)]
#![feature(non_modrs_mods)]


extern crate stable_vec;


pub mod handle;
pub mod impls;
pub mod map;
// pub mod io;


use handle::{HandleIndex, FaceHandle, VertexHandle};


pub trait TriMesh {
    type Idx: HandleIndex;
    // type VertexIter: Iterator<Item = VertexHandle<Self::Idx>>;

    // TODO: use once GATs are available
    // type FaceIter: Iterator<Item = FaceHandle<Self::Idx>>;

    fn num_faces(&self) -> Self::Idx;
    fn num_vertices(&self) -> Self::Idx;

    // fn vertices(&self) -> Self::VertexIter;
    // fn faces(&self) -> Self::FaceIter;
    // TODO: change once GATs are available
    fn faces<'a>(&'a self) -> Box<Iterator<Item = FaceHandle<Self::Idx>> + 'a>;

    fn vertices_of_face(&self, face: FaceHandle<Self::Idx>)
        -> [VertexHandle<Self::Idx>; 3];
}
