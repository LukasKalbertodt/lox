//! Polygon mesh data structures for storing connectivity information
//! (the "core" of a mesh).
//!
//! This module contains different data structures for the representation of
//! triangle or polygon meshes, as well as traits for abstracting over them.
//! Note that these data structures are *only* concerned with connectivity
//! information and cannot store additional properties, like face normals or
//! even vertex positions. Use [prop maps][crate::map] for that purpose.
//!
//!
//! # Introduction
//!
//! To algorithmically work with polygon meshes, we need a way to represent the
//! meshes. Usually, we require certain adjacency information from the mesh to
//! do something with it. As the most basic example, if we want to render a
//! triangle mesh, we usually iterate over all faces and then need to obtain
//! the three vertices of that face. But more advanced algorithms require all
//! kinds of adjacency information about a mesh, like for example: "all faces
//! around this vertex", "is this face on a boundary?" and "what is the valence
//! of this vertex?"
//!
//! To answer these questions, the mesh data structure has to store and
//! maintain some kind of adjacency information. Unfortunately, the more
//! adjacency information we store, the slower the data structure becomes and
//! the more memory it consumes. Thus, existing data structures roughly fall on
//! a spectrum from "small and fast, but not powerful" to "large and slower,
//! but powerful".
//!
//! For each situation, you should use the fastest data structure that is able
//! to satisfy all your algorithmic requirements. For example, if you only want
//! to render triangle meshes, it does not make sense to use a complex half
//! edge mesh. The mesh traits abstract over the different capabilities of the
//! different data structures.
//!
//!
//! # Available data structures
//!
//! The following table shows the data structures that are currently implemented
//! in `lox`. Refer to their documentation to learn more about how these data
//! structures work internally.
//!
//!
//! | Name         | Memory | Face kind | [`BasicAdj`] | [`FullAdj`] | [`EdgeAdj`] | [`EdgeMesh`] |
//! | ------------ | ------ | --------- | ------------ | ----------- | ----------- | ------------ |
//! | [`SharedVertexMesh`] | 24     | Triangles        | ✅ | ❌     | ❌          | ❌          |
//! | [`DirectedEdgeMesh`] | 52 – 100 | Triangles      | ✅ | ✅     | ❌          | ❌          |
//! | [`HalfEdgeMesh`]     | 84 – 108 | *configurable* | ✅ | ✅     | ✅          | ✅          |
//!
//!
//! *Note about the "memory" column*: this value is given in bytes and is
//! calculated assuming that handles have a size of 4 bytes (which is the case
//! without the `large-handle` feature). Furthermore, the value represents the
//! memory usage per vertex in a standard triangle mesh. In such a typical
//! triangle mesh, the number of faces is roughly equal to twice the number of
//! vertices; there are also roughly three times as many edges than vertices.
//! As such, the memory value is calculated as `⟨bytes per vertex⟩ + 2 ⋅ ⟨bytes
//! per face⟩ + 3 ⋅ ⟨bytes per full edge⟩`. A range is given if the mesh has
//! optional fields.
//!
//! More data structures will be added in the future.
//!
//!
//! # Compile time configurations
//!
//! Many data structures can be configured at compile time. Some allow you to
//! enable or disable optional fields. Enabling these fields increases memory
//! consumption, but can speed up certain operations.
//!
//! Other configurations allow you to decide what mesh features the data
//! structure supports. For example, the `HalfEdgeMesh` can be configured to
//! only support triangular faces *or* to support mixed faces of arbitrary
//! valence. Only supporting triangular meshes speeds up some operations.
//!
//! The configuration is done at compile time by passing it as a generic
//! parameter. That parameter is a type that implements the `Config` trait of
//! that particular data structure. In order to create your own configuration,
//! create a new enum type without any variants (e.g. `enum MyConfig {}`) and
//! then implement `Config` for it.
//!
//!
//! # Mesh Traits
//!
//! There are various traits that describe different capabilities of mesh data
//! structures. The most important ones are the three basic ones [`Mesh`],
//! [`MeshMut`] and [`EdgeMesh`], plus the three adjacency-related ones
//! [`BasicAdj`], [`FullAdj`] and [`EdgeAdj`]. These six traits are connected
//! via super trait bounds like so:
//!
//! ```text
//!   ┌──────── EdgeMesh ◄─────────────────────┐
//!   │                                        │
//!   ▼                                        │
//! Mesh  ◄──── BasicAdj ◄──── FullAdj ◄──── EdgeAdj
//!   ▲
//!   │
//!   └──────── MeshMut
//! ```
//!
//! Further, there are:
//!
//! - [`PolyMesh`] and [`TriMesh`] are trait aliases/shorthands for `Mesh` with
//!   a fixed [`FaceKind`][Mesh::FaceKind].
//! - [`Orientable`] and [`NonOrientable`] are trait aliases/shorthands for
//!   `Mesh` with a fixed [`Orientable`][Mesh::Orientable] value.
//! - [`SupportsMultiBlade`] is a marker trait for data structures that support
//!   multi fan-blade vertices.
//!

use crate::{
    hsize, Handle, EdgeHandle, FaceHandle, VertexHandle, ElementRef,
    sealed::Sealed,
    util::HSizeExt,
};


#[cfg(test)]
#[macro_use]
mod tests;

pub mod directed_edge;
pub mod half_edge;
pub mod shared_vertex;

mod checked;
mod traits;
mod util;

pub use self::{
    directed_edge::DirectedEdgeMesh,
    half_edge::HalfEdgeMesh,
    shared_vertex::SharedVertexMesh,
    traits::{
        Mesh, MeshMut, BasicAdj, FullAdj, EdgeAdj, EdgeMesh, Orientable,
        NonOrientable, SupportsMultiBlade, PolyMesh, TriMesh,
    },
    util::{OptionalField, StoreField, OmitField},
};

pub(crate) use self::checked::Checked;


// ===========================================================================
// ===== `FaceKind`
// ===========================================================================

/// A kind of faces a mesh data structure can store. Either [`TriFaces`] or
/// [`PolyFaces`].
///
/// This is a sealed trait, meaning you cannot implement it for your own types.
/// This library provides exactly two types that implement this trait.
pub trait FaceKind: Sealed {
    const ONLY_TRIANGLES: bool;
}

/// Only triangular faces are supported.
#[allow(missing_debug_implementations)]
pub enum TriFaces {}
impl Sealed for TriFaces {}
impl FaceKind for TriFaces {
    const ONLY_TRIANGLES: bool = true;
}

/// Arbitrary polygons are allowed as faces.
#[allow(missing_debug_implementations)]
pub enum PolyFaces {}
impl Sealed for PolyFaces {}
impl FaceKind for PolyFaces {
    const ONLY_TRIANGLES: bool = false;
}


// ===========================================================================
// ===== Iterators over handles and refs
// ===========================================================================

/// An iterator over the handles of the elements of a mesh. Yields handles with
/// increasing index value.
///
/// Instances of this type are returned by:
/// - [`Mesh::vertex_handles`]
/// - [`Mesh::face_handles`]
/// - [`Mesh::edge_handles`]
#[derive(Debug, Clone)]
pub struct HandleIter<'a, M: Mesh + ?Sized, H: Handle> {
    current: H,
    mesh: &'a M,
    count: hsize,
}

macro_rules! impl_handle_iter {
    ($mesh_trait:ident, $handle:ident, $method:ident, $num_fn:ident) => {
        impl<'a, M: $mesh_trait + ?Sized> HandleIter<'a, M, $handle> {
            pub(crate) fn new(mesh: &'a M) -> Self {
                Self {
                    current: $handle::new(0),
                    mesh,
                    count: mesh.$num_fn(),
                }
            }
        }

        impl<M: $mesh_trait + ?Sized> Iterator for HandleIter<'_, M, $handle> {
            type Item = $handle;

            fn next(&mut self) -> Option<Self::Item> {
                let out = self.mesh.$method(self.current);
                if let Some(out) = out {
                    self.current = $handle::new(out.idx().next());
                    self.count -= 1;
                }

                out
            }

            fn size_hint(&self) -> (usize, Option<usize>) {
                (self.count as usize, Some(self.count as usize))
            }
        }

        impl<M: $mesh_trait + ?Sized> ExactSizeIterator for HandleIter<'_, M, $handle> {}
    }
}

impl_handle_iter!(Mesh, VertexHandle, next_vertex_handle_from, num_vertices);
impl_handle_iter!(Mesh, FaceHandle, next_face_handle_from, num_faces);
impl_handle_iter!(EdgeMesh, EdgeHandle, next_edge_handle_from, num_edges);



/// An iterator over the handles of the elements of a mesh. Yields handles with
/// increasing index value. Can give mutable access to the underlying mesh.
///
/// Note that using this iterator is a bit tricky and you have to pay attention
/// to not shoot yourself in the foot. This iterator iterates through all
/// existing element handles from handle index 0 to the initial
/// `self.last_{element}_handle` (that is: what this method returned when this
/// iterator was created). That means that:
///
/// - Adding new elements with a handle higher than the "initial last handle"
///   won't affect the iteration.
/// - Adding or removing elements with handle indices lower than the handle
///   that was last yielded by this iterator, won't affect the iteration.
/// - Adding or removing elements with handle indices higher than the handle
///   last yielded by this iterator but smaller than the "initial last handle"
///   *will* affect the iteration. You should avoid doing that.
#[derive(Debug)]
pub struct HandleIterMut<'a, M: Mesh + ?Sized, H: Handle> {
    current: H,
    last: H,
    mesh: &'a mut M,
}

impl<'a, M: Mesh + ?Sized, H: Handle> HandleIterMut<'a, M, H> {
    pub fn mesh(&mut self) -> &mut M {
        self.mesh
    }
}

macro_rules! impl_handle_iter_mut {
    ($mesh_trait:ident, $handle:ident, $next:ident, $last:ident) => {
        impl<'a, M: $mesh_trait + ?Sized> HandleIterMut<'a, M, $handle> {
            pub(crate) fn new(mesh: &'a mut M) -> Self {
                // If the mesh has none of these elements, the `last` value is
                // not important.
                Self {
                    current: $handle::new(0),
                    last: mesh.$last().unwrap_or($handle::new(0)),
                    mesh,
                }
            }
        }

        impl<M: $mesh_trait + ?Sized> Iterator for HandleIterMut<'_, M, $handle> {
            type Item = $handle;

            fn next(&mut self) -> Option<Self::Item> {
                let out = self.mesh.$next(self.current);

                if let Some(out) = out {
                    if out.idx() > self.last.idx() {
                        return None;
                    }

                    self.current = $handle::new(out.idx().next());
                }

                out
            }
        }
    }
}

impl_handle_iter_mut!(Mesh, VertexHandle, next_vertex_handle_from, last_vertex_handle);
impl_handle_iter_mut!(Mesh, FaceHandle, next_face_handle_from, last_face_handle);
impl_handle_iter_mut!(EdgeMesh, EdgeHandle, next_edge_handle_from, last_edge_handle);

/// An iterator over elements of a mesh. Yields elements with increasing handle
/// index value.
///
/// Instances of this type are returned by:
/// - [`Mesh::vertices`]
/// - [`Mesh::faces`]
/// - [`Mesh::edges`]
#[derive(Debug, Clone)]
pub struct ElementRefIter<'a, M: Mesh + ?Sized, H: Handle> {
    handles: HandleIter<'a, M, H>,
}

impl<'a, M, H> Iterator for ElementRefIter<'a, M, H>
where
    M: Mesh + ?Sized,
    H: Handle,
    HandleIter<'a, M, H>: Iterator<Item = H>,
{
    type Item = ElementRef<'a, H, M>;

    fn next(&mut self) -> Option<Self::Item> {
        self.handles.next().map(|h| ElementRef::new(self.handles.mesh, h))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.handles.size_hint()
    }
}

impl<'a, M, H> ExactSizeIterator for ElementRefIter<'a, M, H>
where
    M: Mesh + ?Sized,
    H: Handle,
    HandleIter<'a, M, H>: Iterator<Item = H>,
{}

macro_rules! impl_element_iter {
    ($mesh_trait:ident, $handle:ident) => {
        impl<'a, M: $mesh_trait + ?Sized> ElementRefIter<'a, M, $handle> {
            pub(crate) fn new(mesh: &'a M) -> Self {
                Self {
                    handles: HandleIter::<M, $handle>::new(mesh),
                }
            }
        }
    }
}

impl_element_iter!(Mesh, VertexHandle);
impl_element_iter!(Mesh, FaceHandle);
impl_element_iter!(EdgeMesh, EdgeHandle);


// ===========================================================================
// ===== Other stuff
// ===========================================================================

/// Utility struct, return type of [`MeshMut::split_edge_with_faces`].
#[derive(Debug, Copy, Clone)]
pub struct SplitEdgeWithFacesResult {
    pub vertex: VertexHandle,
    pub replacement_edges: [EdgeHandle; 2],
}
