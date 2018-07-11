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


use std::fmt::Debug;

use auto_impl::auto_impl;
use num_traits::{AsPrimitive, Num, NumAssign, NumCast};



/// Types that can be interpreted to represent some kind of 3D position.
///
/// This type is implemented for strongly typed "position"-types, like
/// `cgmath::Point3`, as well as for generic "weaker" types such as tuples
/// `(T, T, T)` and arrays `[T; 3]`. However, to avoid logic errors, you should
/// try to use strong types to represent points in 3D space instead of simple
/// tuples.
pub trait Pos3Like: Copy {
    /// The type of each component.
    type Scalar: PrimitiveNum;

    /// Creates the position type from the given three scalar values.
    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self;

    /// Returns the `x` component of this position.
    fn x(&self) -> &Self::Scalar;

    /// Returns the `y` component of this position.
    fn y(&self) -> &Self::Scalar;

    /// Returns the `z` component of this position.
    fn z(&self) -> &Self::Scalar;
}

impl<T: PrimitiveNum> Pos3Like for (T, T, T) {
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

impl<T: PrimitiveNum> Pos3Like for [T; 3] {
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
pub trait Vec3Like: Copy {
    /// The type of each component.
    type Scalar: PrimitiveNum;

    /// Creates the direction vector type from the given three scalar values.
    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self;

    /// Returns the `x` component of this vector.
    fn x(&self) -> &Self::Scalar;

    /// Returns the `y` component of this vector.
    fn y(&self) -> &Self::Scalar;

    /// Returns the `z` component of this vector.
    fn z(&self) -> &Self::Scalar;
}

impl<T: PrimitiveNum> Vec3Like for (T, T, T) {
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

impl<T: PrimitiveNum> Vec3Like for [T; 3] {
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


/// Property sets that store a 3D position.
#[auto_impl(&)]
pub trait HasPosition {
    type Position: Pos3Like;
    fn position(&self) -> &Self::Position;
}

/// Property sets that store a 3D normal.
#[auto_impl(&)]
pub trait HasNormal {
    type Normal: Vec3Like;
    fn normal(&self) -> &Self::Normal;
}


/// Primitive numerical types, like `f64` and `u32`.
pub trait PrimitiveNum: 'static + Copy + Debug + Num + NumCast + PartialOrd + NumAssign {
    fn cast_as<T>(self) -> T
    where
        T: PrimitiveNum,
        Self: AsPrimitive<T>,
    {
        self.as_()
    }
}

impl<T> PrimitiveNum for T
where
    T: 'static + Copy + Debug + Num + NumCast + PartialOrd + NumAssign,
{}


/// Describes what a property can semantically represent.
///
/// This categorization is useful for several purposes, but it's particularly
/// important for serialization. This can be seen as the counter-part to the
/// `HasPosition`, `HasNormal`, ... traits. The traits allow us to assert that
/// specific semantic properties are available at compile time. On the other
/// hand, this type can be used to describe a property at runtime.
///
/// These labels are returned by [`LabeledPropList::labels`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PropLabel {
    Position,
    Normal,
    // TODO: rename to Custom
    Named(String),
}

/// A list of labeled properties.
///
/// Each property list should contain at most one property for each label (so,
/// a prop list cannot contain to properties with the label `Position`).
/// Implementations of this trait should be consistent with implementations of
/// the `HasPosition`, `HasNormal`, ... traits. That means that if `label()` of
/// a specific property list returns a `Position` label for an index, this
/// property list also has to implement `HasPosition`.
///
/// This trait can and should also be implemented for single properties that
/// are labeled. In other words: it's fine when the list contains only one
/// property.
#[auto_impl(&)]
pub trait LabeledPropList {
    // TODO: Maybe this belongs into an own trait
    /// The number of properties in this list.
    fn num_props() -> usize;

    /// The label of the property with the given index.
    ///
    /// The ordering of the properties within the list has to be stable, i.e.
    /// non changing between calls. This method has to return a correct
    /// `PropLabel` for all indices in `0..Self::num_props()` and panic otherwise.
    fn label_of(prop_index: usize) -> PropLabel;

    /// Returns the labels of all properties in this list as vector.
    fn labels() -> Vec<PropLabel> {
        (0..Self::num_props())
            .map(|i| Self::label_of(i))
            .collect()
    }
}

impl LabeledPropList for () {
    fn num_props() -> usize {
        0
    }

    fn label_of(_: usize) -> PropLabel {
        panic!() // TODO
    }
}
