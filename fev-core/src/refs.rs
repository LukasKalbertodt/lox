use crate::{
    ExplicitVertex, ExplicitFace,
    handle::{EdgeHandle, FaceHandle, VertexHandle, Handle},
};


/// A handle paired with a reference to the mesh associated with that handle.
pub struct Ref<'a, HandleT: Handle, MeshT: 'a> {
    handle: HandleT,
    mesh: &'a MeshT,
}

pub type EdgeRef<'a, MeshT> = Ref<'a, EdgeHandle, MeshT>;
pub type FaceRef<'a, MeshT> = Ref<'a, FaceHandle, MeshT>;
pub type VertexRef<'a, MeshT> = Ref<'a, VertexHandle, MeshT>;

/// A handle paired with a mutable reference to the mesh associated with that
/// handle.
pub struct RefMut<'a, HandleT: Handle, MeshT: 'a> {
    handle: HandleT,
    mesh: &'a mut MeshT,
}

pub type EdgeRefMut<'a, MeshT> = RefMut<'a, EdgeHandle, MeshT>;
pub type FaceRefMut<'a, MeshT> = RefMut<'a, FaceHandle, MeshT>;
pub type VertexRefMut<'a, MeshT> = RefMut<'a, VertexHandle, MeshT>;




macro_rules! multi_impl {
    (
        [$(
            { $($header:tt)* },
        )*]
        $body:tt
    ) => {
        $(
            $($header)*
            $body
        )*
    }
}


impl<'a, HandleT: Handle, MeshT: 'a> Ref<'a, HandleT, MeshT> {
    pub fn new(mesh: &'a MeshT, handle: HandleT) -> Self {
        Self { mesh, handle }
    }
}

impl<'a, HandleT: Handle, MeshT: 'a> RefMut<'a, HandleT, MeshT> {
    pub fn new(mesh: &'a mut MeshT, handle: HandleT) -> Self {
        Self { mesh, handle }
    }
}


multi_impl!{
    [
        { impl<'a, HandleT: Handle, MeshT: 'a> Ref<'a, HandleT, MeshT> },
        { impl<'a, HandleT: Handle, MeshT: 'a> RefMut<'a, HandleT, MeshT> },
    ]
    {
        pub fn handle(&self) -> HandleT {
            self.handle
        }
    }
}

// ===========================================================================
// ===== With VertexHandle
// ===========================================================================
multi_impl!{
    [
        { impl<'a, MeshT: 'a> VertexRef<'a, MeshT> },
        { impl<'a, MeshT: 'a> VertexRefMut<'a, MeshT> },
    ]
    {
        pub fn prop(&self) -> &MeshT::VertexProp
        where
            MeshT: ExplicitVertex,
        {
            self.mesh.vertex_prop(self.handle)
                .expect("invalid handle in VertexRef")
        }
    }
}

impl<'a, MeshT: 'a> VertexRefMut<'a, MeshT> {
    pub fn prop_mut(&mut self) -> &mut MeshT::VertexProp
    where
        MeshT: ExplicitVertex,
    {
        self.mesh.vertex_prop_mut(self.handle)
            .expect("invalid handle in VertexRef")
    }
}



// ===========================================================================
// ===== With FaceHandle
// ===========================================================================
multi_impl!{
    [
        { impl<'a, MeshT: 'a> FaceRef<'a, MeshT> },
        { impl<'a, MeshT: 'a> FaceRefMut<'a, MeshT> },
    ]
    {
        pub fn prop(&self) -> &MeshT::FaceProp
        where
            MeshT: ExplicitFace,
        {
            self.mesh.face_prop(self.handle)
                .expect("invalid handle in FaceRef")
        }
    }
}

impl<'a, MeshT: 'a> FaceRefMut<'a, MeshT> {
    pub fn prop_mut(&mut self) -> &mut MeshT::FaceProp
    where
        MeshT: ExplicitFace,
    {
        self.mesh.face_prop_mut(self.handle)
            .expect("invalid handle in FaceRef")
    }
}
