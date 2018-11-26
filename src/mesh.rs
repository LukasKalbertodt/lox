use std::marker::PhantomData;

use failure::Fail;

use crate::{
    handle::{DefaultInt, FaceHandle, VertexHandle},
    refs::{FaceRef, VertexRef},
    map::{VecMap, PropStoreMut},
};

/// The three basic elements in a polygon mesh.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MeshElement {
    Edge,
    Face,
    Vertex,
}

pub trait Empty {
    /// Returns an empty value of this type.
    fn empty() -> Self;
}

/// Some kind of polygon mesh.
pub trait Mesh: Empty {}

/// A triangular mesh: all faces are triangles.
pub trait TriMesh: Mesh {}


// Alternative names:
// - HasVertices
// - ExplicitVertex
// - ContainsVertices
// - VertexIndex
// - WithVerts
pub trait ExplicitVertex {
    fn num_vertices(&self) -> DefaultInt;

    fn add_vertex(&mut self) -> VertexHandle;

    fn vertices<'s>(&'s self) -> Box<dyn Iterator<Item = VertexRef<'s, Self>> + 's>;
    // TODO: visit_mut
    // TODO: iterator over handles
    // TODO: mutable iterator?
}

pub trait ExplicitFace {
    fn num_faces(&self) -> DefaultInt;

    // CCW!
    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle;

    fn faces<'s>(&'s self) -> Box<dyn Iterator<Item = FaceRef<'s, Self>> + 's>;
    // TODO: visit_mut
    // TODO: iterator over handles
    // TODO: mutable iterator?
}


pub trait MeshUnsorted {
    /// Maybe we should return vertex refs? CCW!
    fn vertices_of_face(&self, face: FaceHandle) -> [VertexHandle; 3];
}

/// Errors that can happen when transfering data from a mesh source to a mesh
/// sink. This is either a source error or a sink error.
#[derive(Debug, Fail)]
pub enum TransferError<SrcE: Fail, SinkE: Fail> {
    #[fail(display = "mesh source error: {}", _0)]
    Source(SrcE),

    #[fail(display = "mesh sink error: {}", _0)]
    Sink(SinkE),
}

impl<SrcE: Fail> TransferError<SrcE, !> {
    pub fn into_source(self) -> SrcE {
        match self {
            TransferError::Source(e) => e,
            TransferError::Sink(n) => n,
        }
    }
}

impl<SinkE: Fail> TransferError<!, SinkE> {
    pub fn into_sink(self) -> SinkE {
        match self {
            TransferError::Source(n) => n,
            TransferError::Sink(e) => e,
        }
    }
}

pub trait MeshSource {
    type VertexInfo;
    type FaceInfo;
    type Error: Fail;

    fn build<S>(self, sink: &mut S) -> Result<(), TransferError<Self::Error, S::Error>>
    where
        S: MeshSink<Self::VertexInfo, Self::FaceInfo>;

    fn map_vertex<F, O>(self, map: F) -> MappedVertexSource<Self, F>
    where
        F: FnMut(Self::VertexInfo) -> O,
        Self: Sized,
    {
        MappedVertexSource {
            source: self,
            map,
        }
    }

    fn map_face<F, O>(self, map: F) -> MappedFaceSource<Self, F>
    where
        F: FnMut(Self::FaceInfo) -> O,
        Self: Sized,
    {
        MappedFaceSource {
            source: self,
            map,
        }
    }
}

pub trait MeshSink<VertexInfoT, FaceInfoT> {
    type Error: Fail;

    fn add_vertex(&mut self, info: VertexInfoT) -> Result<VertexHandle, Self::Error>;
    fn add_face(
        &mut self,
        vertices: [VertexHandle; 3],
        info: FaceInfoT,
    ) -> Result<FaceHandle, Self::Error>;

    fn build_from<SrcT>(
        source: SrcT,
    ) -> Result<Self, TransferError<SrcT::Error, Self::Error>>
    where
        Self: Sized + Empty,
        SrcT: MeshSource<VertexInfo = VertexInfoT, FaceInfo = FaceInfoT>,
    {
        let mut out = Self::empty();
        source.build(&mut out)?;
        Ok(out)
    }
}

#[derive(Debug)]
pub struct MappedVertexSource<SrcT, F> {
    source: SrcT,
    map: F,
}

impl<SrcT, F, O> MeshSource for MappedVertexSource<SrcT, F>
where
    SrcT: MeshSource,
    F: FnMut(SrcT::VertexInfo) -> O,
{
    type VertexInfo = O;
    type FaceInfo = SrcT::FaceInfo;
    type Error = SrcT::Error;

    fn build<S>(self, original_sink: &mut S) -> Result<(), TransferError<Self::Error, S::Error>>
    where
        S: MeshSink<Self::VertexInfo, Self::FaceInfo>
    {
        self.source.build(&mut AdhocMeshSink {
            state: (self.map, original_sink),
            add_vertex: |(map, sink), info| sink.add_vertex(map(info)),
            add_face: |(_, sink), vertices, info| sink.add_face(vertices, info),
            _dummy: PhantomData,
        })
    }
}

#[derive(Debug)]
pub struct MappedFaceSource<SrcT, F> {
    source: SrcT,
    map: F,
}

impl<SrcT, F, O> MeshSource for MappedFaceSource<SrcT, F>
where
    SrcT: MeshSource,
    F: FnMut(SrcT::FaceInfo) -> O,
{
    type VertexInfo = SrcT::VertexInfo;
    type FaceInfo = O;
    type Error = SrcT::Error;

    fn build<S>(self, original_sink: &mut S) -> Result<(), TransferError<Self::Error, S::Error>>
    where
        S: MeshSink<Self::VertexInfo, Self::FaceInfo>
    {
        self.source.build(&mut AdhocMeshSink {
            state: (self.map, original_sink),
            add_vertex: |(_, sink), info| sink.add_vertex(info),
            add_face: |(map, sink), vertices, info| sink.add_face(vertices, map(info)),
            _dummy: PhantomData,
        })
    }
}

struct AdhocMeshSink<S, V, F, E, VertexT, FaceT>
where
    V: FnMut(&mut S, VertexT) -> Result<VertexHandle, E>,
    F: FnMut(&mut S, [VertexHandle; 3], FaceT) -> Result<FaceHandle, E>,
    E: Fail,
{
    state: S,
    add_vertex: V,
    add_face: F,
    _dummy: PhantomData<(E, VertexT, FaceT)>,
}

impl<S, V, F, E, VertexT, FaceT> MeshSink<VertexT, FaceT> for AdhocMeshSink<S, V, F, E, VertexT, FaceT>
where
    V: FnMut(&mut S, VertexT) -> Result<VertexHandle, E>,
    F: FnMut(&mut S, [VertexHandle; 3], FaceT) -> Result<FaceHandle, E>,
    E: Fail,
{
    type Error = E;

    fn add_vertex(&mut self, info: VertexT) -> Result<VertexHandle, Self::Error> {
        (self.add_vertex)(&mut self.state, info)
    }

    fn add_face(
        &mut self,
        vertices: [VertexHandle; 3],
        info: FaceT,
    ) -> Result<FaceHandle, Self::Error> {
        (self.add_face)(&mut self.state, vertices, info)

    }
}

#[derive(Debug)]
pub struct MeshWithProps<MeshT, VertexT, FaceT> {
    pub mesh: MeshT,
    pub vertex_props: VecMap<VertexHandle, VertexT>,
    pub face_props: VecMap<FaceHandle, FaceT>,
}

impl<MeshT, VertexT, FaceT> Empty for MeshWithProps<MeshT, VertexT, FaceT>
where
    MeshT: Empty,
{
    fn empty() -> Self {
        Self {
            mesh: MeshT::empty(),
            vertex_props: VecMap::empty(),
            face_props: VecMap::empty(),
        }
    }
}


impl<MeshT, VertexT, FaceT> MeshSink<VertexT, FaceT> for MeshWithProps<MeshT, VertexT, FaceT>
where
    MeshT: Mesh + ExplicitVertex + ExplicitFace,
{
    type Error = !;

    fn add_vertex(&mut self, info: VertexT) -> Result<VertexHandle, Self::Error> {
        let handle = self.mesh.add_vertex();
        self.vertex_props.insert(handle, info);
        Ok(handle)
    }

    fn add_face(
        &mut self,
        vertices: [VertexHandle; 3],
        info: FaceT,
    ) -> Result<FaceHandle, Self::Error> {
        let handle = self.mesh.add_face(vertices);
        self.face_props.insert(handle, info);
        Ok(handle)
    }
}
