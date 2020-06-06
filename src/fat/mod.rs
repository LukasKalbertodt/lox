//! Fat meshes: types that store mesh connectivity and element properties.
//!
//! The core meshes in this library (e.g.
//! [`SharedVertexMesh`][crate::ds::SharedVertexMesh] and
//! [`HalfEdgeMesh`][crate::ds::HalfEdgeMesh]) only store mesh connectivity.
//! That in itself is usually not sufficient to do anything useful. At the very
//! least, one usually wants to store vertex positions. Often, normals and other
//! properties need to be stored as well. Types that store connectivity and
//! properties are called "*fat* meshes" in this library.
//!
//! This library is designed so that users can very easily create their own fat
//! mesh types that store exactly the properties they need. Just define a struct
//! and derive [`Empty`][crate::traits::Empty], [`MemSink`][crate::io::MemSink]
//! and [`MemSource`][crate::io::MemSource] for it. However, there are some very
//! common requirements (e.g. "just `f32` vertex positions"), so that it's worth
//! offering some very common fat mesh types here.
//!
//! Overview of the fat mesh types in this module:
//!
//! | Type | Core Mesh | v-position | v-normal | v-color | f-normal | f-color |
//! | ---- | --------- | ---------- | -------- | ------- | -------- | ------- |
//! | [`MiniMesh`] | `<T: Mesh>` | `f32` | - | - | - | - |
//! | [`AnyMesh`] | `SharedVertexMesh` | *any* | *any* | *any* | *any* | *any* |
//!

use cgmath::{Point3, Vector3};

use crate::{
    prelude::*,
    ds::{HalfEdgeMesh, half_edge},
    handle::hsize,
    io::{self, ColorType, Error, Primitive, PrimitiveType},
    map::DenseMap,
};
use self::any::{AnyColorMap, AnyPointMap, AnyVectorMap};
pub mod any;


// ===============================================================================================
// ===== MiniMesh
// ===============================================================================================

/// A fat mesh that only stores connectivity and `f32` vertex positions.
///
/// The type of the core mesh is specified by the type parameter `M`. The cast
/// rigor for the vertex positions is "lossy", i.e. all kinds of casts are
/// allowed.
#[derive(Empty, MemSink, MemSource, Debug, Clone)]
pub struct MiniMesh<M: MeshMut + BasicAdj> {
    #[lox(core_mesh)]
    pub mesh: M,

    #[lox(vertex_position, cast = "lossy")]
    pub vertex_positions: DenseMap<VertexHandle, Point3<f32>>,
}


// ===============================================================================================
// ===== AnyMesh
// ===============================================================================================

/// A fat mesh with dynamically typed properties intended to losslessly store
/// IO mesh data.
///
/// This is a rather special purpose type. For most cases, you want to use a
/// fat mesh that stores properties with specific convenient types. Instead,
/// this type is intended for use where you deal with very generic inputs and
/// you want to ensure lossless and exact storage. Notably, the dynamic typing
/// incurs some runtime overhead you usually want to avoid. Additionally, the
/// core mesh is always a `SharedVertexMesh` which is not very powerful but
/// sufficient for IO operations.
///
/// The `MemSource` implementation always casts to the requested type, so the
/// property getters never fail.
#[derive(Debug, Empty, Clone)]
pub struct AnyMesh<M: Mesh = HalfEdgeMesh<half_edge::PolyConfig>> {
    pub mesh: M,
    pub vertex_positions: Option<AnyPointMap<VertexHandle>>,
    pub vertex_normals: Option<AnyVectorMap<VertexHandle>>,
    pub vertex_colors: Option<AnyColorMap<VertexHandle>>,
    pub face_normals: Option<AnyVectorMap<FaceHandle>>,
    pub face_colors: Option<AnyColorMap<FaceHandle>>,
}

impl<M: MeshMut + BasicAdj> MemSink for AnyMesh<M> {
    fn add_vertex(&mut self) -> VertexHandle {
        self.mesh.add_vertex()
    }
    fn add_face(&mut self, vertices: &[VertexHandle]) -> Result<FaceHandle, Error> {
        io::util::try_add_face(&mut self.mesh, vertices)
    }

    fn prepare_vertex_positions<N: Primitive>(&mut self, count: hsize) -> Result<(), Error> {
        let mut map = AnyPointMap::new::<N>();
        map.reserve(count);
        self.vertex_positions = Some(map);
        Ok(())
    }
    fn set_vertex_position<N: Primitive>(
        &mut self,
        handle: VertexHandle,
        position: Point3<N>,
    ) {
        self.vertex_positions.as_mut().unwrap().insert(handle, position);
    }

    fn prepare_vertex_normals<N: Primitive>(&mut self, count: hsize) -> Result<(), Error> {
        let mut map = AnyVectorMap::new::<N>();
        map.reserve(count);
        self.vertex_normals = Some(map);
        Ok(())
    }
    fn set_vertex_normal<N: Primitive>(
        &mut self,
        handle: VertexHandle,
        normal: Vector3<N>,
    ) {
        self.vertex_normals.as_mut().unwrap().insert(handle, normal);
    }

    fn prepare_vertex_colors<C>(&mut self, count: hsize) -> Result<(), Error>
    where
        C: ColorLike<Channel: Primitive>,
    {
        let mut map = AnyColorMap::new::<C>();
        map.reserve(count);
        self.vertex_colors = Some(map);
        Ok(())
    }
    fn set_vertex_color<C>(&mut self, handle: VertexHandle, color: C)
    where
        C: ColorLike<Channel: Primitive>,
    {
        self.vertex_colors.as_mut().unwrap().insert(handle, color);
    }

    fn prepare_face_normals<N: Primitive>(&mut self, count: hsize) -> Result<(), Error> {
        let mut map = AnyVectorMap::new::<N>();
        map.reserve(count);
        self.face_normals = Some(map);
        Ok(())
    }
    fn set_face_normal<N: Primitive>(
        &mut self,
        handle: FaceHandle,
        normal: Vector3<N>,
    ) {
        self.face_normals.as_mut().unwrap().insert(handle, normal);
    }

    fn prepare_face_colors<C>(&mut self, count: hsize) -> Result<(), Error>
    where
        C: ColorLike<Channel: Primitive>,
    {
        let mut map = AnyColorMap::new::<C>();
        map.reserve(count);
        self.face_colors = Some(map);
        Ok(())
    }
    fn set_face_color<C>(&mut self, handle: FaceHandle, color: C)
    where
        C: ColorLike<Channel: Primitive>,
    {
        self.face_colors.as_mut().unwrap().insert(handle, color);
    }
}

impl<M: MeshMut + BasicAdj> MemSource for AnyMesh<M> {
    type CoreMesh = M;
    fn core_mesh(&self) -> &Self::CoreMesh {
        &self.mesh
    }

    fn vertex_position_type(&self) -> Option<PrimitiveType> {
        self.vertex_positions.as_ref().map(|m| m.primitive_type())
    }
    fn vertex_position<T: Primitive>(&self, v: VertexHandle) -> Result<Option<Point3<T>>, Error> {
        let out = self.vertex_positions
            .as_ref()
            .expect("requested non-existent vertex position from `AnyMesh`")
            .get_casted_lossy(v);

        Ok(out)
    }

    fn vertex_normal_type(&self) -> Option<PrimitiveType> {
        self.vertex_normals.as_ref().map(|m| m.primitive_type())
    }
    fn vertex_normal<T: Primitive>(&self, v: VertexHandle) -> Result<Option<Vector3<T>>, Error> {
        let out = self.vertex_normals
            .as_ref()
            .expect("requested non-existent vertex normal from `AnyMesh`")
            .get_casted_lossy(v);

        Ok(out)
    }

    fn vertex_color_type(&self) -> Option<ColorType> {
        self.vertex_colors.as_ref().map(|m| m.color_type())
    }
    fn vertex_color<C>(&self, v: VertexHandle) -> Result<Option<C>, Error>
    where
        C: ColorLike<Channel: Primitive>,
    {
        let out = self.vertex_colors
            .as_ref()
            .expect("requested non-existent vertex color from `AnyMesh`")
            .get_casted_lossy(v);

        Ok(out)
    }

    fn face_normal_type(&self) -> Option<PrimitiveType> {
        self.face_normals.as_ref().map(|m| m.primitive_type())
    }
    fn face_normal<T: Primitive>(&self, f: FaceHandle) -> Result<Option<Vector3<T>>, Error> {
        let out = self.face_normals
            .as_ref()
            .expect("requested non-existent face normal from `AnyMesh`")
            .get_casted_lossy(f);

        Ok(out)
    }

    fn face_color_type(&self) -> Option<ColorType> {
        self.face_colors.as_ref().map(|m| m.color_type())
    }
    fn face_color<C>(&self, v: FaceHandle) -> Result<Option<C>, Error>
    where
        C: ColorLike<Channel: Primitive>,
    {
        let out = self.face_colors
            .as_ref()
            .expect("requested non-existent face color from `AnyMesh`")
            .get_casted_lossy(v);

        Ok(out)
    }
}
