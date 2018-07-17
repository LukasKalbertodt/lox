#![feature(use_extern_macros)]
#![feature(crate_in_paths)]

extern crate auto_impl;
#[cfg(feature = "cgmath")]
extern crate cgmath;
pub extern crate num_traits;


pub mod handle;
pub mod prop;
pub mod refs;

use std::fmt;

use crate::{
    handle::{DefaultId, FaceHandle, VertexHandle},
    refs::{FaceRef, VertexRef},
};


/// The three basic elements in a polygon mesh.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MeshElement {
    Edge,
    Face,
    Vertex,
}

impl fmt::Display for MeshElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MeshElement::Edge => "edge",
            MeshElement::Face => "face",
            MeshElement::Vertex => "vertex",
        }.fmt(f)
    }
}

// Alternative names:
// - HasVertices
// - ExplicitVertex
// - ContainsVertices
// - VertexIndex
// - WithVerts
pub trait ExplicitVertex {
    type VertexProp;

    fn vertex_prop(&self, handle: VertexHandle) -> Option<&Self::VertexProp>;
    fn vertex_prop_mut(&mut self, handle: VertexHandle) -> Option<&mut Self::VertexProp>;

    fn num_vertices(&self) -> DefaultId;

    fn vertices<'s>(&'s self) -> Box<Iterator<Item = VertexRef<Self>> + 's>
    where
        Self: Sized;
    // TODO: visit_mut
}

pub trait ExplicitFace {
    type FaceProp;

    fn face_prop(&self, handle: FaceHandle) -> Option<&Self::FaceProp>;
    fn face_prop_mut(&mut self, handle: FaceHandle) -> Option<&mut Self::FaceProp>;

    fn num_faces(&self) -> DefaultId;

    fn faces<'s>(&'s self) -> Box<Iterator<Item = FaceRef<Self>> + 's>
    where
        Self: Sized;
    // TODO: visit_mut
}


pub trait MeshUnsorted {
    /// Maybe we should return vertex refs? CCW!
    fn vertices_of_face(&self, face: FaceHandle) -> [VertexHandle; 3];
}
