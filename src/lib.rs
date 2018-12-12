//! Everything related to meshes.
//!
//! **TODO**: Everything.

#![feature(never_type)]
#![feature(doc_cfg)]
#![feature(proc_macro_hygiene)]
#![feature(try_blocks)]
#![feature(bind_by_move_pattern_guards)]
#![feature(try_from)]
#![feature(specialization)]

#![deny(missing_debug_implementations)]

pub extern crate cgmath;


#[cfg(test)]
#[macro_use]
mod test_utils;

pub mod ds;
pub mod handle;
pub mod io;
pub mod macros;
pub mod map;
pub mod math;
mod mesh;
pub mod prelude;
pub mod refs;
pub mod shape;

pub use self::{
    handle::{EdgeHandle, FaceHandle, VertexHandle},
    mesh::{
        Mesh, TriMesh, MeshElement, MeshMut, TriMeshMut, MeshUnsorted,
        MeshSource, MeshSink, MeshWithProps, TransferError, Empty,
    },
};


// ===========================================================================
// ===== Macros
// ===========================================================================

// Sadly, rustdoc is a bit buggy when it comes to reexporting proc macros. So
// when rustdoc runs, we use a dummy macro. When the crate is compiled as
// usual, we will reexport the proc macro. TODO: check if fixed
#[cfg(not(rustdoc))]
pub use lox_macros::mesh as mesh;

/// Convenience macro to quickly create a small mesh.
///
/// **Note about unstable features**: this proc macro needs to be invoked in
/// expression context, which is still unstable. So your crate needs to enable
/// the `proc_macro_hygiene` feature for this to work.
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
/// ```ignore
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
macro_rules! mesh {
    // The real implementation is in `lox_macros` and it's reexported above.
    (/* proc macro */) => ( /* proc macro */ )
}
