use fev_core::{
    ExplicitFace, ExplicitVertex,
    handle::{VertexHandle, FaceHandle},
};

use super::PropMap;


/// Map representing the vertex properties of a mesh.
///
/// This is a wrapper around a mesh to use the vertex properties of that mesh
/// as `PropMap`.
///
/// # Example
///
/// ```
/// # extern crate fev_core;
/// # extern crate fev_map;
/// use fev_core::{
///     ExplicitVertex,
///     handle::VertexHandle,
/// };
/// use fev_map::{MeshVertexMap, PropMap};
///
/// fn takes_prop_map<'a>(_: &impl PropMap<'a, VertexHandle>) {}
///
/// fn foo(mesh: &impl ExplicitVertex) {
///     let vertex_props = MeshVertexMap::new(mesh);
///     takes_prop_map(&vertex_props);
/// }
/// ```
pub struct MeshVertexMap<'a, MeshT: 'a + ExplicitVertex> {
    mesh: &'a MeshT,
}

impl<'a, MeshT: ExplicitVertex> MeshVertexMap<'a, MeshT> {
    pub fn new(mesh: &'a MeshT) -> Self {
        Self { mesh }
    }
}

impl<'a, 's, MeshT> PropMap<'s, VertexHandle> for MeshVertexMap<'a, MeshT>
where
    MeshT: ExplicitVertex,
{
    type Target = &'a MeshT::VertexProp;

    fn get(&'s self, handle: VertexHandle) -> Option<Self::Target> {
        self.mesh.vertex_prop(handle)
    }
}


/// Map representing the face properties of a mesh.
///
/// This is a wrapper around a mesh to use the face properties of that mesh as
/// `PropMap`.
///
/// # Example
///
/// ```
/// # extern crate fev_core;
/// # extern crate fev_map;
/// use fev_core::{
///     ExplicitFace,
///     handle::FaceHandle,
/// };
/// use fev_map::{MeshFaceMap, PropMap};
///
/// fn takes_prop_map<'a>(_: &impl PropMap<'a, FaceHandle>) {}
///
/// fn foo(mesh: &impl ExplicitFace) {
///     let face_props = MeshFaceMap::new(mesh);
///     takes_prop_map(&face_props);
/// }
/// ```
pub struct MeshFaceMap<'a, MeshT: 'a + ExplicitFace> {
    mesh: &'a MeshT,
}

impl<'a, MeshT: ExplicitFace> MeshFaceMap<'a, MeshT> {
    pub fn new(mesh: &'a MeshT) -> Self {
        Self { mesh }
    }
}

impl<'a, 's, MeshT> PropMap<'s, FaceHandle> for MeshFaceMap<'a, MeshT>
where
    MeshT: ExplicitFace,
{
    type Target = &'a MeshT::FaceProp;

    fn get(&'s self, handle: FaceHandle) -> Option<Self::Target> {
        self.mesh.face_prop(handle)
    }
}