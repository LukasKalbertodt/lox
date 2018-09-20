//! Everything related to meshes.
//!
//! **TODO**: Everything.

#![feature(never_type)]
#![feature(doc_cfg)]


pub mod ds;
pub mod handle;
pub mod macros;
pub mod map;
pub mod math;
mod mesh;
pub mod refs;

pub use self::{
    handle::{EdgeHandle, FaceHandle, VertexHandle},
    mesh::{Mesh, TriMesh, MeshElement, ExplicitFace, ExplicitVertex},
};


// ===========================================================================
// ===== Macros
// ===========================================================================

// Sadly, rustdoc is a bit buggy when it comes to reexporting proc macros. So
// when rustdoc runs, we use a dummy macro. When the crate is compiled as
// usual, we will reexport the proc macro.
#[cfg(not(rustdoc))]
pub use lox_macros::mesh as mesh;

/// Convenience macro to quickly create a small mesh.
///
/// **Note about unstable features**: this proc macro needs to be invoked in
/// expression context, which is still unstable. So your crate need to enable
/// the `proc_macro_non_items` feature for this to work.
///
/// # Examples
///
/// ## An empty mesh
///
/// This is not particularly useful in itself, but it works. You can use this
/// syntax when you haven't yet decided how your mesh should look like.
///
/// ```
/// #![feature(proc_macro_non_items)]
/// # use lox_macros::mesh;
/// use lox::ds::SharedVertexMesh;
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
