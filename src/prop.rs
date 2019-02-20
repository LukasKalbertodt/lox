use cgmath::{Point3, Vector3};

use crate::{
    math::PrimitiveNum,
};

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
    fn x(&self) -> Self::Scalar;

    /// Returns the `y` component of this position.
    fn y(&self) -> Self::Scalar;

    /// Returns the `z` component of this position.
    fn z(&self) -> Self::Scalar;

    /// Converts this value into another `Pos3Like` value with the same scalar
    /// type.
    fn convert<P: Pos3Like<Scalar = Self::Scalar>>(&self) -> P {
        P::from_coords(self.x(), self.y(), self.z())
    }

    /// Maps all three scalar values with the given function and creates a new
    /// value of type `P`.
    ///
    /// Sadly Rust can't handle HKTs yet, so this method is a bit shitty. It
    /// would be nice to only map the scalars and not change the outer type.
    /// But since that's not possible, the output is not `Self<T>`, but this
    /// `P`. So you probably have to use type annotations somewhere.
    ///
    /// If you have a value of `Point3` you can simply use its `map` function
    /// which is easier to use.
    fn map_scalar<P: Pos3Like>(&self, mut f: impl FnMut(Self::Scalar) -> P::Scalar) -> P {
        P::from_coords(
            f(self.x()),
            f(self.y()),
            f(self.z()),
        )
    }

    fn to_point3(&self) -> Point3<Self::Scalar> {
        self.convert()
    }
}

impl<T: PrimitiveNum> Pos3Like for Point3<T> {
    type Scalar = T;
    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        Self::new(x, y, z)
    }
    fn x(&self) -> Self::Scalar { self.x }
    fn y(&self) -> Self::Scalar { self.y }
    fn z(&self) -> Self::Scalar { self.z }
}

impl<T: PrimitiveNum> Pos3Like for (T, T, T) {
    type Scalar = T;
    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        (x, y, z)
    }
    fn x(&self) -> Self::Scalar { self.0 }
    fn y(&self) -> Self::Scalar { self.1 }
    fn z(&self) -> Self::Scalar { self.2 }
}

impl<T: PrimitiveNum> Pos3Like for [T; 3] {
    type Scalar = T;
    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        [x, y, z]
    }
    fn x(&self) -> Self::Scalar { self[0] }
    fn y(&self) -> Self::Scalar { self[1] }
    fn z(&self) -> Self::Scalar { self[2] }
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
    fn x(&self) -> Self::Scalar;

    /// Returns the `y` component of this vector.
    fn y(&self) -> Self::Scalar;

    /// Returns the `z` component of this vector.
    fn z(&self) -> Self::Scalar;

    /// Converts this value into another `Vec3Like` value with the same scalar
    /// type.
    fn convert<V: Vec3Like<Scalar = Self::Scalar>>(&self) -> V {
        V::from_coords(self.x(), self.y(), self.z())
    }

    fn to_vector3(&self) -> Vector3<Self::Scalar> {
        self.convert()
    }
}


impl<T: PrimitiveNum> Vec3Like for Vector3<T> {
    type Scalar = T;
    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        Self::new(x, y, z)
    }
    fn x(&self) -> Self::Scalar { self.x }
    fn y(&self) -> Self::Scalar { self.y }
    fn z(&self) -> Self::Scalar { self.z }
}


impl<T: PrimitiveNum> Vec3Like for (T, T, T) {
    type Scalar = T;
    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        (x, y, z)
    }
    fn x(&self) -> Self::Scalar { self.0 }
    fn y(&self) -> Self::Scalar { self.1 }
    fn z(&self) -> Self::Scalar { self.2 }
}

impl<T: PrimitiveNum> Vec3Like for [T; 3] {
    type Scalar = T;
    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        [x, y, z]
    }
    fn x(&self) -> Self::Scalar { self[0] }
    fn y(&self) -> Self::Scalar { self[1] }
    fn z(&self) -> Self::Scalar { self[2] }
}
