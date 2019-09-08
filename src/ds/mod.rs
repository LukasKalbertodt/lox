//! Polygon mesh **d**ata **s**tructures.
//!
//! This module contains different data structures for the representation of
//! triangle or polygon meshes. The data structures implement the mesh traits
//! of this libary. Note that these data structures only represent the *core
//! mesh*, i.e. which mesh elements exist and how they are connected. Mesh
//! properties, including vertex positions, are not stored in these core
//! meshes. In order to store adjacency information, you have to use [property
//! maps][crate::map]. There are also a few so called *fat meshes* defined in
//! [the `fat` module][crate::fat] which store a core mesh plus additional
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
//! | [`SharedVertexMesh`][SVM] | 24 | Triangles | ✘ | ✔ | ✘ | ✘ |
//! | [`DirectedEdgeMesh`][DEM] | 52 – 100 | Triangles | ✘ | ✔ | ✔ | ✘ |
//! | [`HalfEdgeMesh`][HEM]     | 84 – 108 | *configurable* | ✔ | ✔ | ✔ | ✔ |
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
//! [SVM]: ds::SharedVertexMesh
//! [DEM]: ds::DirectedEdgeMesh
//! [HEM]: ds::HalfEdgeMesh

use crate::{
    traits::marker::{Bool, True, False},
};

#[cfg(test)]
#[macro_use]
mod tests;

mod checked;
pub mod directed_edge;
// mod face_delegate;
pub mod half_edge;
mod shared_vertex;

pub use self::{
    directed_edge::DirectedEdgeMesh,
    // face_delegate::FaceDelegateMesh,
    half_edge::HalfEdgeMesh,
    shared_vertex::SharedVertexMesh,
};

pub(crate) use self::checked::Checked;


// ===== Utilities for optional fields in mesh data structures -----------------------------------

/// This is a helper for `TypeOpt`.
///
/// With this, we build a type system function from a Boolean and a type `T` to
/// a type. That output type is either `()` or `T`. Unfortunately, we have to
/// use a few hacks to actually make this work.
trait TypeOrVoid<T: Copy>: Bool {
    /// The output type. `T` for `True`, and `()` for `False`.
    type Output: Copy;

    /// Convert `T` to the output type. Either the value is just returned or
    /// discarded.
    fn new(t: T) -> Self::Output;

    /// Depending on the Boolean, we return `None` or `Some<T>`. This does look
    /// like we move compile-time decisions to the runtime. This is true, but
    /// the compiler can always optimize this away. It's just more convenient
    /// this way.
    fn into_option(v: Self::Output) -> Option<T>;
}

impl<B: Bool, T: Copy> TypeOrVoid<T> for B {
    // Unreachable. The compiler doesn't know that `True` and `False` are the
    // only types implementing `Bool` (because well, we could add new ones). So
    // the compiler doesn't know that this impl will never be active.
    default type Output = !;
    default fn new(_: T) -> Self::Output {
        unreachable!()
    }
    default fn into_option(_: Self::Output) -> Option<T> {
        unreachable!()
    }
}

// Specialization for `True`
impl<T: Copy> TypeOrVoid<T> for True {
    type Output = T;
    fn new(t: T) -> Self::Output {
        t
    }
    fn into_option(v: Self::Output) -> Option<T> {
        Some(v)
    }
}

// Specialization for `False`
impl<T: Copy> TypeOrVoid<T> for False {
    type Output = ();
    fn new(_: T) -> Self::Output {
        ()
    }
    fn into_option(_: Self::Output) -> Option<T> {
        None
    }
}

/// An optional value of type `T` which presence is determined by the compile
/// time Boolean `B`.
///
/// If `B` is `False`, this type is always zero sized, otherwise it has the
/// same size as `T` and contains an instance of that type.
///
/// Due to limitations of the Rust compiler we already have to specify `Copy`
/// and `Debug` bounds here although they have nothing to do with the `TypeOpt`
/// type itself. That's because we need those bounds later where we use
/// `TypeOpt`.
struct TypeOpt<T: Copy, B: Bool>(<B as TypeOrVoid<T>>::Output);

impl<T: Copy, B: Bool> TypeOpt<T, B> {
    /// Create a new instance of this optional. If `B` is `False`, the value
    /// `v` is just discarded. You can also use the `From<T>` impl.
    fn new(v: T) -> Self {
        Self(<B as TypeOrVoid<T>>::new(v))
    }

    /// Converts this optional in a standard `Option`. While this seems like we
    /// lose the compile-time advantage here, the compiler can easily optimize
    /// this. Depending on `B`, either `None` or `Some` is returned without
    /// depending on any runtime value.
    fn into_option(self) -> Option<T> {
        <B as TypeOrVoid<T>>::into_option(self.0)
    }
}

impl<T: Copy, B: Bool> From<T> for TypeOpt<T, B> {
    fn from(t: T) -> Self {
        Self::new(t)
    }
}

impl<T: Copy, B: Bool> Copy for TypeOpt<T, B> {}
impl<T: Copy, B: Bool> Clone for TypeOpt<T, B> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}
