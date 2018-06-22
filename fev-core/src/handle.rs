//! Types and functions related to handles (e.g. face handle, vertex handle).
//!
//! A handle is some kind of identifier which allows you to retrieve the data
//! associated with that handle. It's a bit like the key in a hashmap. There
//! are different kinds of handles to refer to different data. For example, a
//! `FaceHandle` can be used to retrieve a face.
//!
//! Note that different kinds of handles usually only exist to use strong
//! typing and are identical on machine level. This allows you to get a
//! compiler error when you pass a `VertexHandle` where a `FaceHandle` is
//! expected. Without strong handle types, it's easy to get very strange and
//! hard to debug runtime errors.
//!
//! # IDs and `DefaultId`
//!
//! Handles are built around IDs. Usually, a handle type simply stores the id
//! and functions as a (strongly typed) wrapper. An ID is just data that is
//! able to uniquly identify with other data. However, the ID does not need to
//! be globally unique, but only in a specific *universe* (like a mesh).
//!
//! The last paragraph sounds like an ID is pretty abstract, but in fact, right
//! now, there is only one ID type: `u32`.
//!
//! It is planned to make this library generic over the ID type, but sadly,
//! this makes large parts of the API a lot more complicated. Additionally,
//! without the "GAT" feature of Rust, many things are not really possible. So
//! for now, `u32` is the ID type used everywhere. This choice is probably
//! absolutely OK for most uses cases. See the documentation of `DefaultId` for
//! more details.

use std::fmt;


/// The ID used in all handle types. See [the module documentation][self] for
/// more information on IDs.
///
/// Since we can't be generic over the ID type right now (as explained in [the
/// module documentation][self]), we have to choose a good default. `u32` is
/// fitting for most use cases.
///
/// Since the ID is always used to refer to some data, exhausting `u32` means
/// that we have more than 2<sup>32</sup> instances of that data. If one
/// instance is only 1 byte big, this results in 4GB memory usage. However, in
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
/// occupy around 2<sup>32</sup> · 18 bytes = 72 GB of memory. In other data
/// structures that store more connectivity information, this would be even
/// more. There do exist rare situations (mostly in research) where one has to
/// deal with huge meshes of that size. But again, it's rather rare.
///
/// On the other side are use cases where a smaller ID type, like `u16` would
/// be sufficient. Here, one could save memory by using a smaller ID type.
/// Making `u16` the default ID type is not OK though: 2<sup>16</sup> = 65536
/// is not a huge number and there are many situations in which meshes have way
/// more than 65536 elements.
///
///
/// [1]: https://math.stackexchange.com/q/425968/340615
pub type DefaultId = u32;

/// Trait to pretend being generic over ID types.
///
/// As explained in [the module documentation][self], this library is not
/// generic over the ID type yet. But since this generalization is planned,
/// it's already useful to code most part of the library as if the ID would
/// be generic.
///
/// Right now the trait is only implemented for [`DefaultId`].
pub trait HandleId: Copy {
    /// The number of bytes needed to store the ID.
    const NUM_BYTES: u8;

    /// Create the ID from a `usize`.
    ///
    /// If `raw` cannot be represented by this ID, this function either panics
    /// or returns a nonsensical ID. In debug mode, this function is guaranteed
    /// to panic in this case.
    fn from_usize(raw: usize) -> Self;

    /// The ID represented as `usize`.
    ///
    /// If the ID cannot be represented by `usize`, this function either panics
    /// or returns a nonsensical value. In debug mode, this function is
    /// guaranteed to panic in this case. Note however, that this usually won't
    /// happen, because the ID type is usually smaller than or equal to
    /// `usize`.
    fn to_usize(&self) -> usize;

    /// Returns a new ID.
    ///
    /// When the ID type has been exhausted and there is no new ID, this
    /// function either panics or returns an old ID. In debug mode, this
    /// function is guaranteed to panic in this case.
    fn next(&self) -> Self;
}

impl HandleId for DefaultId {
    const NUM_BYTES: u8 = ::std::mem::size_of::<DefaultId>() as u8;

    fn from_usize(raw: usize) -> Self {
        // If `usize` is bigger than `u32`, we assert that the value is fine.
        #[cfg(target_pointer_width = "64")]
        assert!(raw <= Self::max_value() as usize);

        raw as Self
    }

    fn to_usize(&self) -> usize {
        // If `usize` is smaller than `u32`, we assert that the value is fine.
        #[cfg(any(target_pointer_width = "16", target_pointer_width = "8"))]
        assert!(*self <= usize::max_value() as Self);

        *self as usize
    }

    fn next(&self) -> Self {
        self + 1
    }
}

/// Types that can be used as a handle to some data. See [the module
/// documentation][self] for more information on handles.
///
/// This trait basically represents types that can be created from and
/// converted to an ID type. So handles are just strongly typed IDs.
pub trait Handle: Copy + fmt::Debug + Eq {
    /// Create a handle from the given ID.
    fn from_id(id: DefaultId) -> Self;

    /// Return the ID of the current handle.
    fn id(&self) -> DefaultId;

    /// Helper method to create a handle directly from an `usize`. See
    /// [`HandleId::from_usize`] for details.
    fn from_usize(raw: usize) -> Self {
        Self::from_id(DefaultId::from_usize(raw))
    }

    /// Helper method to get the ID as a usize directly from an handle. See
    /// [`HandleId::to_usize`] for details.
    fn to_usize(&self) -> usize {
        self.id().to_usize()
    }
}

macro_rules! make_handle_type {
    ($(#[$attr:meta])* $name:ident = $short:expr;) => {
        $(#[$attr])*
        #[derive(Clone, Copy, PartialEq, Eq)]
        pub struct $name(DefaultId);

        impl Handle for $name {
            fn from_id(id: DefaultId) -> Self {
                $name(id)
            }
            fn id(&self) -> DefaultId {
                self.0
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", $short)?;
                self.id().fmt(f)
            }
        }
    }
}

make_handle_type!{
    /// A handle that is associated with a face.
    FaceHandle = "F";
}
make_handle_type!{
    /// A handle that is associated with an edge.
    EdgeHandle = "E";
}
make_handle_type!{
    /// A handle that is associated with a vertex.
    VertexHandle = "V";
}
