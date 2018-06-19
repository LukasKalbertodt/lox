//! Traits to describe properties of mesh elements.
//!
//! In general, you can associate arbitrary data with elements of the mesh.
//! However, in some cases, this library needs to have some information about
//! the semantics of properties. This is usually required when you serialize
//! your mesh or create a mesh.
//!
//! As example, suppose you work with a mesh and use your own type as property
//! for the vertices: `VertexProp = Mine`. Your type is defined like this:
//!
//! ```
//! struct Mine {
//!     position: (f32, f32, f32),
//!     normal: (f32, f32, f32),
//!     hotness: u32,
//! }
//! ```
//!
//! So you store the position, the normal and a custom value `hotness` for each
//! vertex. Now suppose you want to serialize a mesh with these vertex
//! properties. Many mesh formats require some special handling of important
//! properties, like position and normal. Sure, in mesh formats that allow
//! arbitrary properties (like PLY), you could just treat `position` and
//! `normal` like every other property. But this is usually not what you want!
//! For example, the PLY format expects you to store the position in three
//! properties called `x`, `y` and `z`.
//!
//! So you want to tell the serializer which semantic function your properties
//! serve. Of course, this library only knows about a couple of semantic
//! classes, like position and normal. Thus it's possible to assign no semantic
//! class to a part of your property (like `hotness` in your example).
//!
//! TODO: talk about creating meshes.
//!
//! TODO: talk about derive.



/// Types that can be interpreted to represent some kind of 3D position.
///
/// This type is implemented for strongly typed "position"-types, like
/// `cgmath::Point3`, as well as for generic "weaker" types such as tuples
/// `(T, T, T)` and arrays `[T; 3]`. However, to avoid logic errors, you should
/// try to use strong types to represent points in 3D space instead of simple
/// tuples.
pub trait Pos3Like {
    /// The type of each component.
    type Scalar;

    /// Creates the position type from the given three scalar values.
    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self;

    /// Returns the `x` component of this position.
    fn x(&self) -> &Self::Scalar;

    /// Returns the `y` component of this position.
    fn y(&self) -> &Self::Scalar;

    /// Returns the `z` component of this position.
    fn z(&self) -> &Self::Scalar;
}

impl<T> Pos3Like for (T, T, T) {
    type Scalar = T;

    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        (x, y, z)
    }

    fn x(&self) -> &Self::Scalar {
        &self.0
    }

    fn y(&self) -> &Self::Scalar {
        &self.1
    }

    fn z(&self) -> &Self::Scalar {
        &self.2
    }
}

impl<T> Pos3Like for [T; 3] {
    type Scalar = T;

    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        [x, y, z]
    }

    fn x(&self) -> &Self::Scalar {
        &self[0]
    }

    fn y(&self) -> &Self::Scalar {
        &self[1]
    }

    fn z(&self) -> &Self::Scalar {
        &self[2]
    }
}


/// Types that can be interpreted to represent some kind of 3D direction
/// vector.
///
/// This type is implemented for strongly typed "vector"-types, like
/// `cgmath::Vector3`, as well as for generic "weaker" types such as tuples
/// `(T, T, T)` and arrays `[T; 3]`. However, to avoid logic errors, you should
/// try to use strong types to represent direction vectors instead of simple
/// tuples.
pub trait Vec3Like {
    /// The type of each component.
    type Scalar;

    /// Creates the direction vector type from the given three scalar values.
    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self;

    /// Returns the `x` component of this vector.
    fn x(&self) -> &Self::Scalar;

    /// Returns the `y` component of this vector.
    fn y(&self) -> &Self::Scalar;

    /// Returns the `z` component of this vector.
    fn z(&self) -> &Self::Scalar;
}

impl<T> Vec3Like for (T, T, T) {
    type Scalar = T;

    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        (x, y, z)
    }

    fn x(&self) -> &Self::Scalar {
        &self.0
    }

    fn y(&self) -> &Self::Scalar {
        &self.1
    }

    fn z(&self) -> &Self::Scalar {
        &self.2
    }
}

impl<T> Vec3Like for [T; 3] {
    type Scalar = T;

    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        [x, y, z]
    }

    fn x(&self) -> &Self::Scalar {
        &self[0]
    }

    fn y(&self) -> &Self::Scalar {
        &self[1]
    }

    fn z(&self) -> &Self::Scalar {
        &self[2]
    }
}
