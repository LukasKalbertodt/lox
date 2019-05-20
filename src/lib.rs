//! Everything related to meshes.
//!
//! **TODO**: Everything.

#![feature(trivial_bounds)]
#![feature(never_type)]
#![feature(doc_cfg)]
#![feature(proc_macro_hygiene)]
#![feature(try_blocks)]
#![feature(bind_by_move_pattern_guards)]
#![feature(specialization)]
#![feature(non_exhaustive)]
#![feature(associated_type_defaults)]

#![deny(missing_debug_implementations)]
#![deny(intra_doc_link_resolution_failure)]


pub extern crate cgmath;

// This is done for proc macros from `lox-macros`. These use paths starting
// with `lox`. This makes sense for all crates using `lox` as dependency. But
// we also want to use proc macros in this library. So we alias `crate` with
// `lox`.
extern crate self as lox;


#[cfg(test)]
#[macro_use]
mod test_utils;

pub mod algo;
pub mod cast;
pub mod ds;
#[cfg(feature = "io")]
pub mod fat;
pub mod handle;
#[cfg(feature = "io")]
pub mod io;
pub mod map;
pub mod math;
pub mod mesh;
pub mod prop;
pub mod traits;
pub mod prelude;
pub mod refs;
#[cfg(feature = "io")]
pub mod shape;
pub mod util;

pub use crate::handle::{EdgeHandle, FaceHandle, VertexHandle};


/// The three basic elements in a polygon mesh.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MeshElement {
    Edge,
    Face,
    Vertex,
}

// ===========================================================================
// ===== `Sealed` trait
// ===========================================================================
pub(crate) mod sealed {
    /// A trait that cannot be implemented outside of this crate.
    ///
    /// This is helpful for all "real" traits in this library that only
    /// abstract over a closed set of types. Thus, users shouldn't be able to
    /// implement those traits for their types. Adding `Sealed` as supertrait
    /// solves this problem.
    pub trait Sealed {}
}


// ===========================================================================
// ===== Macros
// ===========================================================================

pub use lox_macros::Empty;
pub use lox_macros::MemSink;
pub use lox_macros::MemSource;


// Sadly, rustdoc is a bit buggy when it comes to reexporting proc macros. So
// when rustdoc runs, we use a dummy macro. When the crate is compiled as
// usual, we will reexport the proc macro. TODO: check if fixed
pub use lox_macros::mesh as mesh;

/// Convenience macro to quickly create a small mesh.
///
/// (This is just a dummy macro to add documentation to the actual proc-macro
/// reexported from `lox-macros`. See [#58700][i58700] and [#58696][i58696] for
/// more information.)
///
/// **Note about unstable features**: this proc macro needs to be invoked in
/// expression context, which is still unstable. So your crate needs to enable
/// the `proc_macro_hygiene` feature for this to work.
///
/// [i58700]: https://github.com/rust-lang/rust/issues/58700
/// [i58696]: https://github.com/rust-lang/rust/issues/58696
///
/// # Examples
///
/// Here we create two triangles:
///
/// ```
/// #![feature(proc_macro_hygiene)]
/// use lox::{
///     mesh,
///     prelude::*,
///     ds::SharedVertexMesh,
/// };
///
///
/// let (mesh, positions, distances, face_colors) = mesh! {
///     type: SharedVertexMesh,
///     vertices: [
///         v0: ([0.0, 0.0, 0.0], 0.0),
///         v1: ([0.0, 1.0, 0.0], 1.0),
///         v2: ([1.0, 0.0, 0.0], 1.0),
///         v3: ([1.0, 1.0, 0.0], 1.414),
///     ],
///     faces: [
///         [v0, v2, v1]: ("red"),
///         [v3, v1, v2]: ("green"),
///     ],
/// };
///
/// assert_eq!(mesh.num_vertices(), 4);
/// assert_eq!(mesh.num_faces(), 2);
/// ```
///
/// In the code above, we associate a position and a scalar value with each
/// vertex and a color (or rather, a color name) with each face. Properties of
/// vertices and faces are specified after a colon (`:`) in parenthesis (like a
/// tuple).
///
/// For each property you add in those parenthesis, the macro returns an
/// additional property map. The full return value is:
///
/// ```text
/// (mesh, /* vertex property maps */, /* face property maps*/)
/// ```
///
/// ## Without properties
///
/// We don't need to specify any properties. We can either write empty
/// parenthesis (`()`) or just omit the colon and the parenthesis:
///
/// ```
/// #![feature(proc_macro_hygiene)]
/// use lox::{
///     mesh,
///     ds::SharedVertexMesh,
/// };
///
///
/// let mesh = mesh! {
///     type: SharedVertexMesh,
///     vertices: [
///         v0: (),  // <-- this is equivalent to:
///         v1,      // <-- this
///         v2,
///         v3,
///     ],
///     faces: [
///         [v0, v2, v1],
///         [v3, v1, v2],
///     ],
/// };
/// ```
///
/// Of course, you can also add properties to the vertices, but not the faces,
/// or the other way around. However, you always have to specify the same
/// number of properties for all vertices and the same number of properties for
/// all faces.
///
/// ## An empty mesh
///
/// This is not particularly useful in itself, but it works. You can use this
/// syntax when you haven't yet decided how your mesh should look like.
///
/// ```
/// #![feature(proc_macro_hygiene)]
/// use lox::{
///     mesh,
///     ds::SharedVertexMesh,
/// };
///
///
/// let empty_mesh = mesh! {
///     type: SharedVertexMesh,
///     vertices: [],
///     faces: [],
/// };
/// ```
#[cfg(rustdoc)]
#[macro_export]
macro_rules! mesh_macro {
    // The real implementation is in `lox_macros` and it's reexported above.
    (/* proc macro */) => ( /* proc macro */ )
}
