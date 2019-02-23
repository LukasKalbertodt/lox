use cgmath::{Point3, Vector3};

use crate::{
    cast,
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


// ============================================================================
// ===== Color
// ============================================================================
/// Types that can be used as a color channel.
///
/// Implemented for `u8`, `u16`, `u32`, `f32` and `f64`. For integer types, the
/// whole range is used (0 to `T::max_value()`) while for floats, the values
/// are always between 0 and 1.
pub trait PrimitiveColorChannel: PrimitiveNum {
    /// The value representing maximum intensity.
    ///
    /// `T::max_value()` for integer, `1.0` for floats. The minimum intensity
    /// is always 0.
    const MAX_INTENSITY: Self;

    /// Creates one channel type from another one, converting correctly between
    /// values.
    ///
    /// This is not a simple `as` cast or even `cast::lossy`. For example,
    /// `255u8` becomes `1.0f32` or `u16::max_value()`. These casts are not
    /// lossless, but might round the value a bit (like casting rigor
    /// `AllowRounding`).
    fn color_cast_from<SrcT: PrimitiveColorChannel>(src: SrcT) -> Self {
        // We use `lossy` here, but we know that clamping won't happen: casting
        // to `f64` never clamps for types like `u32`. Casting `f64` to `Self`
        // also doesn't clamp because at that point we know that the value is
        // in the limits of `Self`.
        let mult = cast::lossy::<_, f64>(Self::MAX_INTENSITY)
            / cast::lossy::<_, f64>(SrcT::MAX_INTENSITY);
        cast::lossy(mult * cast::lossy::<_, f64>(src))
    }
}

impl PrimitiveColorChannel for u8 {
    const MAX_INTENSITY: Self = u8::max_value();
}
impl PrimitiveColorChannel for u16 {
    const MAX_INTENSITY: Self = u16::max_value();
}
impl PrimitiveColorChannel for u32 {
    const MAX_INTENSITY: Self = u32::max_value();
}
impl PrimitiveColorChannel for f32 {
    const MAX_INTENSITY: Self = 1.0;
}
impl PrimitiveColorChannel for f64 {
    const MAX_INTENSITY: Self = 1.0;
}

/// Types that can be interpreted to represent some kind of color as RGB or
/// RGBA.
///
/// If the `Channel` type is an integer, the full integer range is used. If
/// it's a floating point type, all values have to be between 0.0 and 1.0.
///
/// This trait is implemented for weak types like `[T; 3]`, `[T; 4]` and
/// tuples. However, you should probably use strong color types instead.
pub trait ColorLike: Copy {
    /// The type of each channel.
    type Channel: PrimitiveColorChannel;

    /// Creates a color value with the specified values for the RGB channels.
    ///
    /// If the color type stores an alpha channel, it should be set to an
    /// appropriate default value (e.g. for fully opaque).
    fn from_rgb(r: Self::Channel, g: Self::Channel, b: Self::Channel) -> Self;

    /// Creates a color value with the specified values for the RGBA channels.
    ///
    /// If the color type doesn't store the alpha channel, its value can just
    /// be discarded.
    fn from_rgba(r: Self::Channel, g: Self::Channel, b: Self::Channel, _a: Self::Channel) -> Self {
        Self::from_rgb(r, g, b)
    }

    /// Returns the red channel of this color value.
    fn red(&self) -> Self::Channel;

    /// Returns the green channel of this color value.
    fn green(&self) -> Self::Channel;

    /// Returns the blue channel of this color value.
    fn blue(&self) -> Self::Channel;

    /// Returns the alpha channel of this color value, if that channel is
    /// stored.
    fn alpha(&self) -> Option<Self::Channel> {
        None
    }

    /// Converts from one color type to another but keeping the same channel
    /// type.
    fn convert<T: ColorLike<Channel = Self::Channel>>(&self) -> T {
        let [r, g, b] = [self.red(), self.green(), self.blue()];
        if let Some(a) = self.alpha() {
            T::from_rgba(r, g, b, a)
        } else {
            T::from_rgb(r, g, b)
        }
    }

    /// Casting the channel type via `PrimitiveColorChannel::color_cast_from`.
    ///
    /// Due to Rust not having HKTs, we can't enforce that the color type is
    /// the same as `Self`. Thus you have to rely on type inference or
    /// explicitly annotate the type.
    fn cast<T: ColorLike>(&self) -> T {
        self.map_channel(T::Channel::color_cast_from)
    }

    /// Maps all channels with the given function and creates a new value of
    /// type `T`.
    ///
    /// Sadly Rust can't handle HKTs yet, so this method is a bit shitty. It
    /// would be nice to only map the scalars and not change the outer type.
    /// But since that's not possible, the output is not `Self<_>`, but this
    /// `T`. So you probably have to use type annotations somewhere.
    ///
    /// If this color has an alpha channel, it is mapped as well and the output
    /// color is created via `from_rgba`.
    fn map_channel<T: ColorLike>(&self, mut f: impl FnMut(Self::Channel) -> T::Channel) -> T {
        let [r, g, b] = [
            f(self.red()),
            f(self.green()),
            f(self.blue())
        ];
        if let Some(a) = self.alpha() {
            T::from_rgba(r, g, b, f(a))
        } else {
            T::from_rgb(r, g, b)
        }
    }
}

impl<T: PrimitiveColorChannel> ColorLike for (T, T, T) {
    type Channel = T;
    fn from_rgb(r: Self::Channel, g: Self::Channel, b: Self::Channel) -> Self {
        (r, g, b)
    }
    fn red(&self) -> Self::Channel { self.0 }
    fn green(&self) -> Self::Channel { self.1 }
    fn blue(&self) -> Self::Channel { self.2 }
}

impl<T: PrimitiveColorChannel> ColorLike for [T; 3] {
    type Channel = T;
    fn from_rgb(r: Self::Channel, g: Self::Channel, b: Self::Channel) -> Self {
        [r, g, b]
    }
    fn red(&self) -> Self::Channel { self[0] }
    fn green(&self) -> Self::Channel { self[1] }
    fn blue(&self) -> Self::Channel { self[2] }
}

impl<T: PrimitiveColorChannel> ColorLike for (T, T, T, T) {
    type Channel = T;
    fn from_rgb(r: Self::Channel, g: Self::Channel, b: Self::Channel) -> Self {
        Self::from_rgba(r, g, b, T::MAX_INTENSITY)
    }
    fn from_rgba(r: Self::Channel, g: Self::Channel, b: Self::Channel, a: Self::Channel) -> Self {
        (r, g, b, a)
    }
    fn red(&self) -> Self::Channel { self.0 }
    fn green(&self) -> Self::Channel { self.1 }
    fn blue(&self) -> Self::Channel { self.2 }
    fn alpha(&self) -> Option<Self::Channel> { Some(self.3) }
}

impl<T: PrimitiveColorChannel> ColorLike for [T; 4] {
    type Channel = T;
    fn from_rgb(r: Self::Channel, g: Self::Channel, b: Self::Channel) -> Self {
        Self::from_rgba(r, g, b, T::MAX_INTENSITY)
    }
    fn from_rgba(r: Self::Channel, g: Self::Channel, b: Self::Channel, a: Self::Channel) -> Self {
        [r, g, b, a]
    }
    fn red(&self) -> Self::Channel { self[0] }
    fn green(&self) -> Self::Channel { self[1] }
    fn blue(&self) -> Self::Channel { self[2] }
    fn alpha(&self) -> Option<Self::Channel> { Some(self[3]) }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn color_cast() {
        assert_eq!(u8::color_cast_from(0u8), 0);
        assert_eq!(u8::color_cast_from(u8::max_value()), 255);
        assert_eq!(u8::color_cast_from(0u16), 0);
        assert_eq!(u8::color_cast_from(u16::max_value()), 255);
        assert_eq!(u8::color_cast_from(0u32), 0);
        assert_eq!(u8::color_cast_from(u32::max_value()), 255);
        assert_eq!(u8::color_cast_from(0.0f32), 0);
        assert_eq!(u8::color_cast_from(1.0f32), 255);
        assert_eq!(u8::color_cast_from(0.0f64), 0);
        assert_eq!(u8::color_cast_from(1.0f64), 255);

        assert_eq!(u16::color_cast_from(0u8), 0);
        assert_eq!(u16::color_cast_from(u8::max_value()), u16::max_value());
        assert_eq!(u16::color_cast_from(0u16), 0);
        assert_eq!(u16::color_cast_from(u16::max_value()), u16::max_value());
        assert_eq!(u16::color_cast_from(0u32), 0);
        assert_eq!(u16::color_cast_from(u32::max_value()), u16::max_value());
        assert_eq!(u16::color_cast_from(0.0f32), 0);
        assert_eq!(u16::color_cast_from(1.0f32), u16::max_value());
        assert_eq!(u16::color_cast_from(0.0f64), 0);
        assert_eq!(u16::color_cast_from(1.0f64), u16::max_value());

        assert_eq!(u32::color_cast_from(0u8), 0);
        assert_eq!(u32::color_cast_from(u8::max_value()), u32::max_value());
        assert_eq!(u32::color_cast_from(0u16), 0);
        assert_eq!(u32::color_cast_from(u16::max_value()), u32::max_value());
        assert_eq!(u32::color_cast_from(0u32), 0);
        assert_eq!(u32::color_cast_from(u32::max_value()), u32::max_value());
        assert_eq!(u32::color_cast_from(0.0f32), 0);
        assert_eq!(u32::color_cast_from(1.0f32), u32::max_value());
        assert_eq!(u32::color_cast_from(0.0f64), 0);
        assert_eq!(u32::color_cast_from(1.0f64), u32::max_value());

        assert_eq!(f32::color_cast_from(0u8), 0.0);
        assert_eq!(f32::color_cast_from(u8::max_value()), 1.0);
        assert_eq!(f32::color_cast_from(0u16), 0.0);
        assert_eq!(f32::color_cast_from(u16::max_value()), 1.0);
        assert_eq!(f32::color_cast_from(0u32), 0.0);
        assert_eq!(f32::color_cast_from(u32::max_value()), 1.0);
        assert_eq!(f32::color_cast_from(0.0f32), 0.0);
        assert_eq!(f32::color_cast_from(1.0f32), 1.0);
        assert_eq!(f32::color_cast_from(0.0f64), 0.0);
        assert_eq!(f32::color_cast_from(1.0f64), 1.0);

        assert_eq!(f64::color_cast_from(0u8), 0.0);
        assert_eq!(f64::color_cast_from(u8::max_value()), 1.0);
        assert_eq!(f64::color_cast_from(0u16), 0.0);
        assert_eq!(f64::color_cast_from(u16::max_value()), 1.0);
        assert_eq!(f64::color_cast_from(0u32), 0.0);
        assert_eq!(f64::color_cast_from(u32::max_value()), 1.0);
        assert_eq!(f64::color_cast_from(0.0f32), 0.0);
        assert_eq!(f64::color_cast_from(1.0f32), 1.0);
        assert_eq!(f64::color_cast_from(0.0f64), 0.0);
        assert_eq!(f64::color_cast_from(1.0f64), 1.0);
    }
}
