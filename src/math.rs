//! Abstractions over numerical types and vector-like types.


use std::{
    fmt::Debug,
};

use num_traits::{Num, NumAssign};
#[cfg(feature = "cgmath")]
use cgmath::{Point3, Vector3};



/// Primitive numerical types, like `f64` and `u32`.
///
/// This trait is automatically implemented for all types that satisfy the
/// super-trait constraints.
pub trait PrimitiveNum: 'static + Copy + Debug + Num + PartialOrd + NumAssign {}

impl<T> PrimitiveNum for T
where
    T: 'static + Copy + Debug + Num + PartialOrd + NumAssign,
{}


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

    #[cfg(feature = "cgmath")]
    fn to_point3(self) -> Point3<Self::Scalar> {
        Point3::new(*self.x(), *self.y(), *self.z())
    }

    // TODO: cast
}

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

impl<T: PrimitiveNum> Pos3Like for (T, T, T) {
    type Scalar = T;
    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        (x, y, z)
    }
    fn x(&self) -> &Self::Scalar { &self.0 }
    fn y(&self) -> &Self::Scalar { &self.1 }
    fn z(&self) -> &Self::Scalar { &self.2 }
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

    // TODO: cast()
}


#[cfg(feature = "cgmath")]
impl<T: PrimitiveNum> Vec3Like for Vector3<T> {
    type Scalar = T;
    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        Self::new(x, y, z)
    }
    fn x(&self) -> &Self::Scalar { &self.x }
    fn y(&self) -> &Self::Scalar { &self.y }
    fn z(&self) -> &Self::Scalar { &self.z }
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
