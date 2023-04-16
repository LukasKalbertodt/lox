//! Polygon mesh **d**ata **s**tructures.
//!
//! This module contains different data structures for the representation of
//! triangle or polygon meshes. The data structures implement the mesh traits
//! of this libary. Note that these data structures only represent the *core
//! mesh*, i.e. which mesh elements exist and how they are connected. Mesh
//! properties, including vertex positions, are not stored in these core
//! meshes. In order to store adjacency information, you have to use [property
//! maps][crate::map]. There are also a few so called *fat meshes* defined in
//! the `fat` module (TODO) which store a core mesh plus additional
//! properties.
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
//! # Overview of available data structures
//!
//! The following table shows the data structures that are currently
//! implemented in `lox`. Also refer to the guide (TODO) to learn more about
//! how these data structures work internally.
//!
//!
//! | Name | Memory | Face kind | `EdgeMesh` | `BasicAdj` | `FullAdj` | `EdgeAdj` |
//! | ---- | ------ | --------- | ---------- | ---------- | --------- | --------- |
//! | [`SharedVertexMesh`] | 24 | Triangles | ✘ | ✔ | ✘ | ✘ |
//! | [`DirectedEdgeMesh`] | 52 – 100 | Triangles | ✘ | ✔ | ✔ | ✘ |
//! | [`HalfEdgeMesh`]     | 84 – 108 | *configurable* | ✔ | ✔ | ✔ | ✔ |
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

#[cfg(test)]
#[macro_use]
mod tests;

mod checked;
pub mod directed_edge;
// mod face_delegate;
pub mod half_edge;
mod shared_vertex;
mod util;

pub use self::{
    directed_edge::DirectedEdgeMesh,
    // face_delegate::FaceDelegateMesh,
    half_edge::HalfEdgeMesh,
    shared_vertex::SharedVertexMesh,
    util::{OptionalField, StoreField, OmitField},
};

pub(crate) use self::checked::Checked;

