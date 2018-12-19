//! Everything related to the `SharedVertexMesh`.

use std::fmt;

use crate::{
    handle::{DefaultInt, FaceHandle, VertexHandle},
    map::{VecMap},
    traits::{Empty, TriVerticesOfFace, Mesh, TriMesh, TriMeshMut, MeshMut},
    refs::{FaceRef, VertexRef},
};



#[derive(Clone)]
pub struct SharedVertexMesh {
    vertices: VecMap<VertexHandle, ()>,
    faces: VecMap<FaceHandle, [VertexHandle; 3]>,
}

impl SharedVertexMesh {
    pub fn new() -> Self {
        Self {
            vertices: VecMap::new(),
            faces: VecMap::new(),
        }
    }
}

impl Empty for SharedVertexMesh {
    fn empty() -> Self {
        Self::new()
    }
}

impl Mesh for SharedVertexMesh {
    fn num_vertices(&self) -> DefaultInt {
        self.vertices.num_elements()
    }

    fn vertices<'s>(&'s self) -> Box<Iterator<Item = VertexRef<Self>> + 's> {
        Box::new(self.vertices.handles().map(move |handle| {
            VertexRef::new(self, handle)
        }))
    }

    fn num_faces(&self) -> DefaultInt {
        self.faces.num_elements()
    }

    fn faces<'s>(&'s self) -> Box<Iterator<Item = FaceRef<Self>> + 's> {
        Box::new(self.faces.handles().map(move |handle| {
            FaceRef::new(self, handle)
        }))
    }
}

impl MeshMut for SharedVertexMesh {
    fn add_vertex(&mut self) -> VertexHandle {
        self.vertices.push(())
    }
}


impl TriMesh for SharedVertexMesh {}

impl TriMeshMut for SharedVertexMesh {
    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle {
        self.faces.push(vertices)
    }
}

impl TriVerticesOfFace for SharedVertexMesh {
    fn vertices_of_face(&self, face: FaceHandle) -> [VertexHandle; 3] {
        self.faces[face]
    }
}


impl fmt::Debug for SharedVertexMesh {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        struct VerticesDebug<'a>(&'a VecMap<VertexHandle, ()>);
        impl fmt::Debug for VerticesDebug<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.debug_list().entries(self.0.handles()).finish()
            }
        }

        f.debug_struct("SharedVertexMesh")
            .field("vertices", &VerticesDebug(&self.vertices))
            .field("faces", &self.faces)
            .finish()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    gen_tri_mesh_tests!(SharedVertexMesh: [TriVerticesOfFace]);
}
