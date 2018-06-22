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
pub trait HasPosition {
    type Position: Pos3Like;
    fn position(&self) -> &Self::Position;
}

/// Property sets that store a 3D normal.
pub trait HasNormal {
    type Normal: Vec3Like;
    fn normal(&self) -> &Self::Normal;
}

/// The width of a primitive type.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrimitiveWidth {
    Bits8,
    Bits16,
    Bits32,
    Bits64,
}

/// The kind of a primitive numerical type.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumKind {
    Float,
    UnsignedInt,
    SignedInt,
}

/// Primitive numerical types, like `f64` and `u32`.
pub trait PrimitiveNum: Copy {
    /// The kind of this number (float, unsigned int or signed int).
    const KIND: NumKind;

    /// The width of this number.
    const WIDTH: PrimitiveWidth;

    /// Returns the best/closest representation of `v` as `Self`.
    fn from_float(v: f64) -> Self;

    /// Returns the best/closest representation of `v` as `Self`.
    fn from_unsigned_int(v: u64) -> Self;

    /// Returns the best/closest representation of `v` as `Self`.
    fn from_signed_int(v: i64) -> Self;
}

macro_rules! impl_primitive_num {
    ($name:ident, $kind:ident, $width:ident) => {
        impl PrimitiveNum for $name {
            const KIND: NumKind = NumKind::$kind;
            const WIDTH: PrimitiveWidth = PrimitiveWidth::$width;

            fn from_float(v: f64) -> Self {
                v as $name
            }

            fn from_unsigned_int(v: u64) -> Self {
                v as $name
            }

            fn from_signed_int(v: i64) -> Self {
                v as $name
            }
        }
    }
}

impl_primitive_num!( u8, UnsignedInt, Bits8);
impl_primitive_num!(u16, UnsignedInt, Bits16);
impl_primitive_num!(u32, UnsignedInt, Bits32);
impl_primitive_num!(u64, UnsignedInt, Bits64);
impl_primitive_num!( i8,   SignedInt, Bits8);
impl_primitive_num!(i16,   SignedInt, Bits16);
impl_primitive_num!(i32,   SignedInt, Bits32);
impl_primitive_num!(i64,   SignedInt, Bits64);
impl_primitive_num!(f32,       Float, Bits32);
impl_primitive_num!(f64,       Float, Bits64);
