//! Everything related to meshes.
//!
//! **TODO**: Everything.

#![feature(associated_type_bounds)]

#![deny(missing_debug_implementations)]
#![deny(rustdoc::broken_intra_doc_links)]


// Reexport crates which are publicly used in this crate.
pub extern crate lina;
pub extern crate leer;

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
pub mod core;
// #[cfg(feature = "io")]
// pub mod fat;
pub mod handle;
// #[cfg(feature = "io")]
// pub mod io;
pub mod map;
pub mod math;
pub mod mesh;
pub mod prop;
pub mod prelude;
pub mod refs;
// #[cfg(feature = "io")]
// pub mod shape;
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

// /// Derive macro for [the `MemSink` trait][io::MemSink].
// ///
// /// You can easily derive `MemSink` for your own types. To do that, you have to
// /// attach `#[derive(MemSink)]` to your struct definition (note: currently, the
// /// trait can only be derived for structs with named fields). You also have to
// /// annotate your fields with `#[lox(...)]` attributes to tell the derive macro
// /// what a field should be used for. Example:
// ///
// /// ```
// /// use lox::{
// ///     MemSink, VertexHandle,
// ///     cgmath::Point3,
// ///     core::HalfEdgeMesh,
// ///     map::DenseMap,
// /// };
// ///
// ///
// /// #[derive(MemSink)]
// /// struct MyMesh {
// ///     #[lox(core_mesh)]
// ///     mesh: HalfEdgeMesh,
// ///
// ///     #[lox(vertex_position)]
// ///     positions: DenseMap<VertexHandle, Point3<f32>>,
// /// }
// /// ```
// ///
// /// There is one required field: the core mesh field. That field's type has to
// /// implement several mesh traits, in particular `MeshMut` and `TriMeshMut`.
// /// You have to annotate that mesh with `#[lox(core_mesh)]`.
// ///
// /// Additionally, you can have fields for each mesh property, like vertex
// /// position or face colors. The type of those fields has to implement
// /// `PropStoreMut` with a compatible element type. You have to annotate these
// /// property fields with the corresponding attribute. The available properties
// /// are:
// ///
// /// - `vertex_position`
// /// - `vertex_normal`
// /// - `vertex_color`
// /// - `face_normal`
// /// - `face_color`
// ///
// /// Furthermore, there are some configurations (like the cast mode) that can be
// /// configured via `lox(...)` attributes as well. See below for more
// /// information.
// ///
// ///
// /// ## Cast modes
// ///
// /// You can set a *cast mode* for each field. A `MemSink` has to be able to
// /// "handle" any primitive type as the source is allowed to call the property
// /// methods with any type. The sink can handle types either by casting or by
// /// returning an error. The field's cast mode determines which casts are
// /// allowed and which are not. Possible cast modes:
// ///
// /// - `cast = "none"`
// /// - `cast = "lossless"`
// /// - `cast = "rounding"`
// /// - `cast = "clamping"`
// /// - `cast = "lossy"` (*default*)
// ///
// /// The `none` mode does not allow casting at all. If the type provided by the
// /// source does not match the type in your struct, an error is returned. All
// /// other modes correspond to the cast modes in the [`cast`
// /// module][crate::cast].
// ///
// /// Note that the cast modes are used by `derive(MemSource)` as well.
// ///
// /// You can specify the cast mode either per field or globally on the whole
// /// struct. The mode of the struct applies to all fields that don't have a
// /// field-specific mode.
// ///
// /// ```
// /// # use lox::{
// /// #     MemSink, VertexHandle,
// /// #     cgmath::{Point3, Vector3},
// /// #     core::HalfEdgeMesh,
// /// #     map::DenseMap,
// /// # };
// /// #
// /// #[derive(MemSink)]
// /// #[lox(cast = "none")]
// /// struct MyMesh {
// ///     #[lox(core_mesh)]
// ///     mesh: HalfEdgeMesh,
// ///
// ///     #[lox(vertex_position)]
// ///     positions: DenseMap<VertexHandle, Point3<f32>>,
// ///
// ///     #[lox(vertex_normal, cast = "lossy")]
// ///     normals: DenseMap<VertexHandle, Vector3<f32>>,
// /// }
// /// ```
// ///
// /// In this example, the vertex positions inherit the "struct global" cast mode
// /// (`none`), while the vertex normals override that mode to `lossy`.
// ///
// ///
// /// ### Exact traits required for each field
// ///
// /// Traits required for the `core_mesh` field:
// /// - TODO
// ///
// /// Traits required for property fields. For type `T` of the field:
// /// - `T` must implement [`PropStoreMut`][crate::map::PropStoreMut] (with
// ///   fitting handle type). Additionally:
// ///     - For `vertex_position`: `T::Target` must implement
// ///       [`Pos3Like`][crate::prop::Pos3Like].
// ///     - For `*_normal`: `T::Target` must implement
// ///       [`Vec3Like`][crate::prop::Vec3Like].
// ///     - For `*_color`: `T::Target` must implement
// ///       [`ColorLike`][crate::prop::ColorLike] and `T::Target::Channel` must
// ///       implement [`Primitive`][io::Primitive].
// #[cfg(feature= "io")]
// pub use lox_macros::MemSink;

// /// Derive macro for [the `MemSource` trait][io::MemSource].
// ///
// /// You can easily derive `MemSource` for your own types. To do that, you have
// /// to attach `#[derive(MemSource)]` to your struct definition (note:
// /// currently, the trait can only be derived for structs with named fields).
// /// You also have to annotate your fields with `#[lox(...)]` attributes to tell
// /// the derive macro what a field should be used for. Example:
// ///
// /// ```
// /// use lox::{
// ///     MemSource, VertexHandle,
// ///     cgmath::Point3,
// ///     core::SharedVertexMesh,
// ///     map::DenseMap,
// /// };
// ///
// ///
// /// #[derive(MemSource)]
// /// struct MyMesh {
// ///     #[lox(core_mesh)]
// ///     mesh: SharedVertexMesh,
// ///
// ///     #[lox(vertex_position)]
// ///     positions: DenseMap<VertexHandle, Point3<f32>>,
// /// }
// /// ```
// ///
// /// Deriving this trait works very similar to deriving [`MemSink`]. See its
// /// documentation for more information on the custom derive.
// ///
// ///
// /// ### Exact traits required for each field
// ///
// /// Traits required for the `core_mesh` field:
// /// - TODO
// ///
// /// Traits required for property fields. For type `T` of the field:
// /// - `T` must implement [`PropStore`][crate::map::PropStore] (with fitting
// ///   handle type). Additionally:
// ///     - For `vertex_position`: `T::Target` must implement
// ///       [`Pos3Like`][crate::prop::Pos3Like] and `T::Target::Scalar` must
// ///       implement [`Primitive`][io::Primitive].
// ///     - For `*_normal`: `T::Target` must implement
// ///       [`Vec3Like`][crate::prop::Vec3Like] and `T::Target::Scalar` must
// ///       implement [`Primitive`][io::Primitive].
// ///     - For `*_color`: `T::Target` must implement
// ///       [`ColorLike`][crate::prop::ColorLike] and `T::Target::Channel` must
// ///       implement [`Primitive`][io::Primitive].
// #[cfg(feature= "io")]
// pub use lox_macros::MemSource;

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
///     core::SharedVertexMesh,
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
///     core::SharedVertexMesh,
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
///     core::SharedVertexMesh,
/// };
///
///
/// let empty_mesh = mesh! {
///     type: SharedVertexMesh,
///     vertices: [],
///     faces: [],
/// };
/// ```
pub use lox_macros::mesh;
