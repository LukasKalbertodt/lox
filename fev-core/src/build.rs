
use crate::{
    Mesh,
    handle::{VertexHandle},
};

/// Types that can used to build a triangle mesh with the given vertex and face
/// data.
///
/// This trait is primiarly implemented by mesh types with vertex and face
/// properties that can be created from the data provided to the builder. But
/// there are also some non-mesh types that implement this trait: for example,
/// an `AdhocBuilder` consists of two closures that are used to implement
/// `add_vertex` and ´add_face`. That's useful if the user wants to decide what
/// exactly happens with each face and vertex.
pub trait TriMeshBuilder<VertexInfoT, FaceInfoT> {
    fn add_vertex(&mut self, info: VertexInfoT) -> VertexHandle;
    fn add_face(&mut self, vertices: [VertexHandle; 3], info: FaceInfoT);

    fn hint_num_faces(&mut self, _num: usize) {}
}

pub struct AdhocBuilder<VertexF, FaceF> {
    pub add_vertex: VertexF,
    pub add_face: FaceF,
}

impl<VertexF, FaceF, VertexInfoT, FaceInfoT> TriMeshBuilder<VertexInfoT, FaceInfoT>
    for AdhocBuilder<VertexF, FaceF>
where
    VertexF: FnMut(VertexInfoT) -> VertexHandle,
    FaceF: FnMut([VertexHandle; 3], FaceInfoT),
{
    fn add_vertex(&mut self, info: VertexInfoT) -> VertexHandle {
        (self.add_vertex)(info)
    }

    fn add_face(&mut self, vertices: [VertexHandle; 3], info: FaceInfoT) {
        (self.add_face)(vertices, info)
    }
}



pub trait TriMeshSource: Sized {
    type Err;
    type VertexInfo;
    type FaceInfo;

    fn append(
        self,
        builder: &mut impl TriMeshBuilder<Self::VertexInfo, Self::FaceInfo>,
    ) -> Result<(), Self::Err>;

    fn build<T>(self) -> Result<T, Self::Err>
    where
        T: Mesh + TriMeshBuilder<Self::VertexInfo, Self::FaceInfo>,
    {
        let mut out = T::empty();
        self.append(&mut out)?;
        Ok(out)
    }
}
