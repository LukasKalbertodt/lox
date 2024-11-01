//! A polygon mesh library with different data structures and traits to abstract
//! over those.
//!
//! `lox` can be used to create, generate, process, and analyze polygon meshes.
//! This is part of the field "geometry processing", relevant for developing
//! real-time 3D applications, simulations, 3D-printing, and much more.
//!
//! **Main features**:
//!
//! - Multiple optimized and well tested mesh data structures, including *half
//!   edge mesh* and *directed edge mesh*.
//! - Ability to abstract over different data structures without overhead.
//! - *BlAzInGlY fAsT*. Ok no actually, it's [pretty fast](#speed) and I have
//!    benchmarks to prove it.
//! - *Prop maps* as flexible solution for storing and managing additional mesh
//!   properties (e.g. vertex positions, face colors, ...).
//! - Built-in algorithms (only very few right now).
//! - **Notably missing**: IO. [Explanation](#background-and-missing-features).
//!
//!
//!
//! # Quick start
//!
//! Important facts:
//!
//! - `lox` offers [multiple data structures][core#available-data-structures] to
//!   store mesh connectivity information. It has [traits][core#mesh-traits] to
//!   abstract over those. You can chose the one that best fits your needs.
//!
//! - Data associated with mesh elements (e.g. vertex positions, face
//!   colors, ...) are called "**props**". Props are stored separately from the
//!   connectivity information, in [**prop maps**][map]. Yes, even the vertex
//!   positions.
//!
//! - To refer to mesh elements, use [`VertexHandle`], [`FaceHandle`] and
//!   [`EdgeHandle`].
//!
//! - You can find some provided algorithms in [`algo`].
//!
//! - You likely want to `use lox::prelude::*;` to import all important traits.
//!
//!
//! # Examples
//!
//! Basic creation and usage of some mesh methods:
//!
//! ```
//! use lox::{
//!     core::DirectedEdgeMesh,
//!     prelude::*,
//! };
//!
//! let mut mesh = <DirectedEdgeMesh>::empty();
//!
//! let v0 = mesh.add_vertex();
//! let v1 = mesh.add_vertex();
//! let v2 = mesh.add_vertex();
//! let v3 = mesh.add_vertex();
//!
//! let f0 = mesh.add_triangle([v0, v1, v2]);
//! let f1 = mesh.add_triangle([v0, v2, v3]);
//!
//! let v_center = mesh.split_face(f0);
//!
//! for neighbor in mesh.vertices_around_vertex(v_center) {
//!     assert!(neighbor == v0 || neighbor == v1 || neighbor == v2);
//! }
//!
//! assert_eq!(mesh.num_faces(), 4);
//! ```
//!
//! Creation of a mesh with vertex positions, then subdividing it, finally
//! printing the positions of all boundary vertices.
//!
//! ```
//! use lox::{
//!     core::{HalfEdgeMesh, half_edge::TriConfig},
//!     algo,
//!     prelude::*,
//! };
//!
//! let (mut mesh, mut positions) = lox::mesh! {
//!     type: HalfEdgeMesh<TriConfig>,
//!     vertices: [
//!         v0: [0.0, 0.0, 0.0],
//!         v1: [0.0, 1.0, 0.0],
//!         v2: [1.0, 0.0, 0.0],
//!         v3: [1.0, 1.0, 0.5],
//!     ],
//!     faces: [
//!         [v0, v2, v1],
//!         [v3, v1, v2],
//!     ],
//! };
//!
//! algo::subdivision::sqrt3(&mut mesh, &mut positions, 2);
//!
//! for v in mesh.vertices().filter(|v| v.is_boundary()) {
//!     println!("{:?}", positions[v.handle()]);
//! }
//! ```
//!
//! Working with a non-triangular mesh:
//!
//! ```
//! use lox::{
//!     core::{HalfEdgeMesh, half_edge::PolyConfig},
//!     prelude::*,
//! };
//!
//! let mesh = lox::mesh! {
//!     type: HalfEdgeMesh<PolyConfig>,
//!     vertices: [v0, v1, v2, v3, v4, v5, v6],
//!     faces: [
//!         [v0, v3, v2, v1],
//!         [v0, v5, v3, v3],
//!         [v0, v1, v6, v5],
//!         [v1, v2, v3, v4, v5, v6],
//!     ],
//! };
//!
//! for face in mesh.faces() {
//!     for vertex in face.adjacent_vertices() {
//!         println!("{:?} is a vertex of {:?}", vertex.handle(), face.handle());
//!     }
//! }
//! ```
//!
//!
//! # Speed
//!
//! The library was specifically designed with performance in mind, trying to
//! beat or at least meet the performance of existing C++ libraries like
//! [OpenMesh]. This was evaluated constantly with benchmarks as part of my
//! [master's thesis][thesis]. See chapter 5.1 in the rendered PDF for details.
//!
//! The benchmarks are in need of updating (see the next section) and the
//! benchmark results are 4 years old. But I am pretty confident that not much
//! has changed since then.
//!
//!
//! [OpenMesh]: https://www.graphics.rwth-aachen.de/software/openmesh/
//!
//!
//! # Background and missing features
//!
//! This library started as part of [my master's thesis][thesis] which I
//! finished in summer 2019. Thesis title: "*Designing and Implementing a
//! Polygon Mesh Library: Can Rust Improve the Status Quo in the Domain of
//! Geometry Processing?*" The chapter "Background: Geometry processing" might
//! be useful to read, if you are not very familiar with the field. It also
//! explains the data structures implemented in `lox` in some detail.
//!
//! Anyway, the plan was to just polish the library and then release it.
//! Naturally, that didn't happen. I started multiple attempts over the years
//! to finish the work and get it out the door, but until 2023 I failed. To be
//! fair, only with Rust's stabilization of GATs, was I able to create a
//! somewhat nice API.
//!
//! In 2023 I looked at the crate again and made the decision to cut down its
//! scope for the initial release, to finally get it released at all. So I
//! threw out all IO-related code as it added a whole lot of complexity. That's
//! why IO is completely missing from `lox` right now.
//!
//! Of course, I plan on adding all that functionality back. I already did lots
//! of API design and implementation work. Well, it was working already. But
//! polishing the API and making it maintainable is a different beast. So let's
//! see what the future brings. If you have interest in this, feel free to
//! reach out, I'm happy to hear from you!
//!
//!
//! [thesis]: https://github.com/LukasKalbertodt/masters-thesis
//!
//!
//! # Crate features
//!
//! - `large-handle`: makes the crate us 64 bit integers instead of 32 bit
//!   integers for handles.
//!


#![deny(missing_debug_implementations)]
#![deny(rustdoc::broken_intra_doc_links)]


// Reexport crates which are publicly used in this crate.
pub extern crate lina;
pub extern crate leer;
pub extern crate typebool;

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
pub mod map;
pub mod prelude;
pub mod util;

mod refs;

use std::fmt;

pub use lox_macros::mesh;

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
    /// `hsize::MAX` as this value is reserved!
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
        debug_assert!(raw <= hsize::MAX as usize);

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
        ))]
        debug_assert!(self.idx() <= usize::MAX as hsize);

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
