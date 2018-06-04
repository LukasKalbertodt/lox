use std::{
    f64,
};

use crate::{
    TriMesh,
    handle::{VertexHandle, FaceHandle},
};


pub struct SpheroidVertexInfo {
    pub position: [f64; 3],
    pub normal: [f64; 3],
}

impl HasPosition for SpheroidVertexInfo {
    fn position(&self) -> &[f64; 3] {
        &self.position
    }
}

impl HasNormal for SpheroidVertexInfo {
    fn normal(&self) -> &[f64; 3] {
        &self.normal
    }
}

impl<'a, T> From<&'a T> for SpheroidVertexInfo
where
    T: HasPosition + HasNormal,
{
    fn from(src: &'a T) -> Self {
        Self {
            position: *src.position(),
            normal: *src.normal(),
        }
    }
}

pub struct NoInfo;

impl<'a> From<&'a NoInfo> for () {
    fn from(_: &'a NoInfo) -> Self {
        ()
    }
}


// TODO: maybe make parameters associated types
pub trait MeshBuilder<VertexInfoT, FaceInfoT> {
    fn add_vertex(&mut self, info: VertexInfoT) -> VertexHandle;
    fn add_face(&mut self, vertices: [VertexHandle; 3], info: FaceInfoT) -> FaceHandle;
}

pub struct AdhocBuilder<AddVertexF, AddFaceF, SharedT> {
    pub shared: SharedT,
    pub add_vertex: AddVertexF,
    pub add_face: AddFaceF,
}

impl<AddVertexF, AddFaceF, SharedT, VertexInfoT, FaceInfoT> MeshBuilder<VertexInfoT, FaceInfoT>
    for AdhocBuilder<AddVertexF, AddFaceF, SharedT>
where
    AddVertexF: FnMut(&mut SharedT, VertexInfoT) -> VertexHandle,
    AddFaceF: FnMut(&mut SharedT, [VertexHandle; 3], FaceInfoT) -> FaceHandle,
{
    fn add_vertex(&mut self, info: VertexInfoT) -> VertexHandle {
        (self.add_vertex)(&mut self.shared, info)
    }
    fn add_face(&mut self, vertices: [VertexHandle; 3], info: FaceInfoT) -> FaceHandle {
        (self.add_face)(&mut self.shared, vertices, info)
    }
}


impl<MeshT, VertexInfoT, FaceInfoT> MeshBuilder<VertexInfoT, FaceInfoT> for MeshT
where
    MeshT: TriMesh,
    for<'a> MeshT::VertexProp: From<&'a VertexInfoT>,
    for<'a> MeshT::FaceProp: From<&'a FaceInfoT>,
{
    fn add_vertex(&mut self, info: VertexInfoT) -> VertexHandle {
        <Self as TriMesh>::add_vertex(self, (&info).into())
    }
    fn add_face(&mut self, vertices: [VertexHandle; 3], info: FaceInfoT) -> FaceHandle {
        <Self as TriMesh>::add_face(self, vertices, (&info).into())
    }
}

pub trait HasPosition {
    fn position(&self) -> &[f64; 3];
}

pub trait HasNormal {
    fn normal(&self) -> &[f64; 3];
}


pub fn append_sphere<BuilderT>(builder: &mut BuilderT)
where
    BuilderT: MeshBuilder<SpheroidVertexInfo, NoInfo>,
{
    fn ring_pos(step: u64, num_steps: u64) -> [f64; 3] {
        // Walk around the circle
        let around_circle = 2.0 * step as f64 * (f64::consts::PI / num_steps as f64);
        let x = around_circle.sin();
        let y = around_circle.cos();

        [x, y, 0.0]
    };

    const NUM_STEPS: u64 = 6;

    let normal_up = [0.0, 0.0, 1.0];

    let center = builder.add_vertex(SpheroidVertexInfo {
        position: [0.0, 0.0, 0.0],
        normal: normal_up,
    });

    let first = builder.add_vertex(SpheroidVertexInfo {
        position: ring_pos(0, NUM_STEPS),
        normal: normal_up,
    });

    let mut last = first;

    for step in 1..=NUM_STEPS {
        let curr = if step == NUM_STEPS {
            first
        } else {
            builder.add_vertex(SpheroidVertexInfo {
                position: ring_pos(step, NUM_STEPS),
                normal: normal_up,
            })
        };

        builder.add_face([center, last, curr], NoInfo);
        last = curr;
    }
}
