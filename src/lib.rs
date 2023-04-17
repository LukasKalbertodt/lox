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
// #[cfg(feature = "io")]
// pub mod io;
pub mod map;
pub mod math;
pub mod prop;
pub mod prelude;
// #[cfg(feature = "io")]
// pub mod shape;
pub mod util;

mod refs;

use std::fmt;


pub use refs::{ElementRef, EdgeRef, FaceRef, VertexRef};


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




// ===========================================================================
// ===== Handles and `hsize`
// ===========================================================================

/// The integer used in all [handle types][Handle].
///
/// The type is either `u32` or `u64` depending on whether the Cargo feature
/// `large-handle` is enabled.
///
///
/// # `hsize` and Arena Allocators
///
/// A handle type is just a wrapper around a simple integer, namely `hsize`.
/// This integer is usually used as an index to an array-like thing (like a
/// `Vec`) -- that way, we can refer to data. It is called `hsize` because it
/// is very similar to `usize` in many regards. The *h* is for *handle*.
///
/// Note that handles share some properties with pointers (they refer to a
/// thing and are actually just an integer), but have some important
/// differences:
///
/// - `hsize` (and thus handles) are always 32 bit (or 64 bit with the
///   `large-handle` feature enabled) wide, unlike pointers which might vary
///   with target platform.
/// - While all pointers exist in one "universe" (they all refer to the global
///   memory view of the running process), handles often refer to many
///   different universes. For example, it's perfectly fine to have two
///   `FaceHandle`s with the value 0 that refer to different faces: one handle
///   belongs to the "universe" of one mesh and the other to a different mesh.
///   The user has to remember which universe (mesh) a handle belongs to.
/// - Pointers are usually handled as references in Rust. With that, the borrow
///   checker makes sure we don't do bad things with those. With handles, all
///   those bad things can still happen, e.g. referring to an already deleted
///   thing.
///
/// When looking closer at it, this system can look a lot like an "arena
/// allocator": it's a type that reserves a big chunk of memory at its creation
/// and serves as a memory allocator with the advantage that all its memory can
/// be freed at once. In many cases, simple integers are used to refer to data
/// within such an arena. There are some more "high level" allocators for this,
/// e.g. `slab`. The easiest allocator that works like that would be `Vec`: you
/// can add elements and can refer to them via `usize`.
///
/// It's a common criticism that this way of allocating things is used to
/// sidestep the borrow checker and all the nice guarantees Rust gives us. And
/// as such, that this pattern should be discouraged. However, there are some
/// situations where it cannot be avoided or where this has very large
/// advantages.
///
/// For example, in mesh processing, meshes with more than 2<sup>32</sup>
/// elements are extremely rare and most of the memory in a mesh data structure
/// is taken by references to other elements. So we can drastically reduce the
/// overall memory consumption (and due to caching: the execution time). It
/// would also be very hard to use this library if every element reference
/// would be a real reference. So it does make sense for this library to use
/// such a system.
///
/// Of course, we would like to avoid annoying bugs due to errors like "use
/// after free". The crate `slotmap` has really great ideas regarding this.
/// `lox will try out some ideas to avoid some common mistakes in the future.
///
/// # The size of `hsize`
///
/// Since `lox` is currently not generic over the integer type, we have to
/// choose a good default. `u32` is fitting for most use cases.
///
/// Since the ID is always used to refer to some data, exhausting `u32` means
/// that we have more than 2<sup>32</sup> instances of that data. If one
/// instance is only 1 byte big, this results in 4GiB memory usage. However, in
/// practice 1 byte is not enough to store anything useful for meshes. Usually,
/// you at least store some kind of position (e.g. `[f32; 3]` = 12 bytes) per
/// vertex plus the connectivity information, which is at something like 3
/// handles per face. So the useful minimum of stored information is:
///
/// - 12 bytes per vertex
/// - 12 bytes per face
///
/// From [here][1] we can see that in a typical triangular mesh, there are
/// around twice as many faces as vertices. The effective size per face is thus
/// around 18 bytes. To have more than 2<sup>32</sup> faces, the mesh would
/// occupy around 2<sup>32</sup> · 18 bytes = 72 GiB of memory. In other data
/// structures which store more connectivity information, this would be even
/// more. There do exist rare situations (mostly in research) where one has to
/// deal with huge meshes of that size. But again, it's rather rare.
///
/// On the other side are use cases where a smaller ID type, like `u16` would
/// be sufficient. Here, one could save memory by using a smaller ID type.
/// Making `u16` the default ID type is not OK though: 2<sup>16</sup> = 65536
/// is not a huge number and there are many situations in which meshes have way
/// more than 65536 elements.
///
/// This crate offers the Cargo feature `large-handle` which makes `hsize` 64
/// bit large. This can be easily enabled if you actually need to deal with
/// such huge meshes.
///
/// [1]: https://math.stackexchange.com/q/425968/340615
#[allow(non_camel_case_types)]
#[cfg(not(feature = "large-handle"))]
pub type hsize = HsizeImpl;

#[cfg(not(feature = "large-handle"))]
type HsizeImpl = u32;

#[cfg(feature = "large-handle")]
type HsizeImpl = u64;




/// Types that can be used to refer to some data.
///
/// A handle is some kind of identifier which allows you to retrieve the data
/// associated with that handle. It's a bit like the key in a hashmap. There
/// are different kinds of handles to refer to different data. For example, a
/// [`FaceHandle`] can be used to refer to a face.
///
/// Different kinds of handles (e.g. [`FaceHandle`] and [`VertexHandle`]) are
/// identical on machine level and only serve to catch programming errors via
/// strong typing.
pub trait Handle: 'static + Copy + fmt::Debug + Eq + Ord {
    /// Create a handle from the given index. The index must not be
    /// `hsize::max_value()` as this value is reserved!
    fn new(idx: hsize) -> Self;

    /// Return the index of the current handle.
    fn idx(&self) -> hsize;

    /// Helper method to create a handle directly from an `usize`.
    ///
    /// If `raw` cannot be represented by `hsize`, this function either panics
    /// or returns a nonsensical ID. In debug mode, this function is guaranteed
    /// to panic in this case.
    #[inline(always)]
    fn from_usize(raw: usize) -> Self {
        // If `usize` is bigger than `hsize`, we assert that the value is fine.
        #[cfg(all(target_pointer_width = "64", not(feature = "large-handle")))]
        debug_assert!(raw <= hsize::max_value() as usize);

        Self::new(raw as hsize)
    }

    /// Helper method to get the ID as a usize directly from an handle.
    ///
    /// If the index cannot be represented by `usize`, this function either
    /// panics or returns a nonsensical value. In debug mode, this function is
    /// guaranteed to panic in this case. Note however, that this usually won't
    /// happen, because `hsize` is in almost all cases smaller than or equal to
    /// `usize`.
    #[inline(always)]
    fn to_usize(&self) -> usize {
        // If `usize` is smaller than `hsize`, we assert that the value is fine.
        #[cfg(any(
            all(target_pointer_width = "32", feature = "large-handle"),
            target_pointer_width = "16",
            target_pointer_width = "8",
        ))]
        debug_assert!(self.idx() <= usize::max_value() as hsize);

        self.idx() as usize
    }
}


macro_rules! make_handle_type {
    ($(#[$attr:meta])* $name:ident = $short:expr;) => {
        $(#[$attr])*
        #[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub struct $name(hsize);

        impl Handle for $name {
            #[inline(always)]
            fn new(id: hsize) -> Self {
                $name(id)
            }

            #[inline(always)]
            fn idx(&self) -> hsize {
                self.0
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", $short)?;
                self.idx().fmt(f)
            }
        }
    }
}

make_handle_type!{
    /// A [handle][Handle] referring to a face.
    FaceHandle = "F";
}
make_handle_type!{
    /// A [handle][Handle] referring to an edge.
    EdgeHandle = "E";
}
make_handle_type!{
    /// A [handle][Handle] referring to a vertex.
    VertexHandle = "V";
}
