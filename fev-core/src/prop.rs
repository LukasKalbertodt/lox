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


use std::{
    any::Any,
    fmt::Debug,
};

use auto_impl::auto_impl;
use num_traits::{Num, NumAssign, NumCast};

#[cfg(feature = "cgmath")]
use cgmath::{Point3};



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

    fn convert<T: Pos3Like>(self) -> T {
        T::from_coords(self.x().cast(), self.y().cast(), self.z().cast())
    }

    #[cfg(feature = "cgmath")]
    fn to_point3(self) -> Point3<Self::Scalar> {
        self.convert()
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

    fn convert<T: Pos3Like>(self) -> T {
        T::from_coords(self.x().cast(), self.y().cast(), self.z().cast())
    }
}


/// Property sets that store a 3D position.
#[auto_impl(&)]
pub trait HasPosition {
    type Position: Pos3Like;
    fn position(&self) -> &Self::Position;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position<T: Pos3Like>(pub T);
impl<T: Pos3Like> HasPosition for Position<T> {
    type Position = T;
    fn position(&self) -> &Self::Position {
        &self.0
    }
}

/// Property sets that store a 3D normal.
#[auto_impl(&)]
pub trait HasNormal {
    type Normal: Vec3Like;
    fn normal(&self) -> &Self::Normal;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Normal<T: Vec3Like>(pub T);
impl<T: Vec3Like> HasNormal for Normal<T> {
    type Normal = T;
    fn normal(&self) -> &Self::Normal {
        &self.0
    }
}

/// Primitive numerical types, like `f64` and `u32`.
pub trait PrimitiveNum: 'static + Copy + Debug + Num + NumCast + PartialOrd + NumAssign {
    /// Casts into another primitive numerical type, possibly with loss of
    /// precision.
    ///
    /// # Panics, precision and rounding
    ///
    /// This method panics if this value is too big or too small to be
    /// representable in the target type. Examples:
    ///
    /// ```should-panic
    /// use fev_core::prop::PrimitiveNum;
    ///
    /// // All of these will panic
    /// 300u16.cast::<u8>();
    /// -5i8.cast::<u8>();
    /// 1e300f64.cast::<f32>();
    /// ```
    ///
    /// However, if the target type can approximately represent the value, the
    /// cast succeeds:
    ///
    /// ```
    /// use fev_core::prop::PrimitiveNum;
    ///
    /// // All of these will "succeed", but the resulting value might not
    /// // exactly represent the original value.
    /// u32::max_value().cast::<f32>();
    /// 1e30f64.cast::<f32>();
    /// 3.14f64.cast::<u32>();
    /// ```
    fn cast<T: PrimitiveNum>(self) -> T {
        match T::from(self) {
            Some(v) => v,
            None => {
                // Panic with a nice error message
                panic!(
                    "failed to cast '{}' into smaller numerical type: value '{:?}' doesn't fit \
                        into '{}'",
                    primitive_type_name::<Self>(),
                    self,
                    primitive_type_name::<T>(),
                )
            }
        }
    }
}

impl<T> PrimitiveNum for T
where
    T: 'static + Copy + Debug + Num + NumCast + PartialOrd + NumAssign,
{}

fn primitive_type_name<T: Any>() -> &'static str {
    match () {
        () if Any::is::<T>(&0u8) => "u8",
        () if Any::is::<T>(&0i8) => "i8",
        () if Any::is::<T>(&0u16) => "u16",
        () if Any::is::<T>(&0i16) => "i16",
        () if Any::is::<T>(&0u32) => "u32",
        () if Any::is::<T>(&0i32) => "i32",
        () if Any::is::<T>(&0u64) => "u64",
        () if Any::is::<T>(&0i64) => "i64",
        () if Any::is::<T>(&0f32) => "f32",
        () if Any::is::<T>(&0f64) => "f64",
        _ => "??",
    }
}

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



// ----------------


#[cfg(feature = "cgmath")]
impl<T: PrimitiveNum> Pos3Like for Point3<T> {
    type Scalar = T;
    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        Self::new(x, y, z)
    }
    fn x(&self) -> &Self::Scalar { &self.x }
    fn y(&self) -> &Self::Scalar { &self.y }
    fn z(&self) -> &Self::Scalar { &self.z }
}

#[cfg(feature = "cgmath")]
impl<T: PrimitiveNum> HasPosition for Point3<T> {
    type Position = Self;
    fn position(&self) -> &Self::Position { self }
}

impl<T: PrimitiveNum> Pos3Like for (T, T, T) {
    type Scalar = T;
    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        (x, y, z)
    }
    fn x(&self) -> &Self::Scalar { &self.0 }
    fn y(&self) -> &Self::Scalar { &self.1 }
    fn z(&self) -> &Self::Scalar { &self.2 }
}


impl<T: PrimitiveNum> HasPosition for (T, T, T) {
    type Position = Self;
    fn position(&self) -> &Self::Position { self }
}

impl<T: PrimitiveNum> Pos3Like for [T; 3] {
    type Scalar = T;
    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        [x, y, z]
    }
    fn x(&self) -> &Self::Scalar { &self[0] }
    fn y(&self) -> &Self::Scalar { &self[1] }
    fn z(&self) -> &Self::Scalar { &self[2] }
}


impl<T: PrimitiveNum> HasPosition for [T; 3] {
    type Position = Self;
    fn position(&self) -> &Self::Position { self }
}


impl<T: PrimitiveNum> Vec3Like for (T, T, T) {
    type Scalar = T;
    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        (x, y, z)
    }
    fn x(&self) -> &Self::Scalar { &self.0 }
    fn y(&self) -> &Self::Scalar { &self.1 }
    fn z(&self) -> &Self::Scalar { &self.2 }
}

impl<T: PrimitiveNum> Vec3Like for [T; 3] {
    type Scalar = T;
    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        [x, y, z]
    }
    fn x(&self) -> &Self::Scalar { &self[0] }
    fn y(&self) -> &Self::Scalar { &self[1] }
    fn z(&self) -> &Self::Scalar { &self[2] }
}


pub trait FromProp<P> {
    fn from_prop(src: P) -> Self;
}

pub trait IntoProp<P> {
    fn into_prop(self) -> P;
}

impl<SrcT, DstT: FromProp<SrcT>> IntoProp<DstT> for SrcT {
    fn into_prop(self) -> DstT {
        DstT::from_prop(self)
    }
}
