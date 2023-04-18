//! Dynamically typed property maps (for all IO primitive types). Mainly used
//! for [`AnyMesh`][super::AnyMesh].

use std::any::Any;

use cgmath::{Point3, Vector3};

use crate::{
    prelude::*,
    cast,
    handle::hsize,
    map::DenseMap,
    io::{ColorType, Primitive, PrimitiveType, PrimitiveColorChannelType},
};


macro_rules! gen_vec3_any_map {
    ($(#[$attr:meta])* $name:ident => $vec_type:ident) => {
        $(#[$attr])*
        #[derive(Debug, Clone)]
        pub enum $name<H: Handle> {
            Uint8(DenseMap<H, $vec_type<u8>>),
            Uint16(DenseMap<H, $vec_type<u16>>),
            Uint32(DenseMap<H, $vec_type<u32>>),
            Int8(DenseMap<H, $vec_type<i8>>),
            Int16(DenseMap<H, $vec_type<i16>>),
            Int32(DenseMap<H, $vec_type<i32>>),
            Float32(DenseMap<H, $vec_type<f32>>),
            Float64(DenseMap<H, $vec_type<f64>>),
        }

        impl<H: Handle> $name<H> {
            /// Creates a new instance of this map with the given scalar type.
            pub fn new<T: Primitive>() -> Self {
                match T::TY {
                    PrimitiveType::Uint8 => $name::Uint8(DenseMap::new()),
                    PrimitiveType::Int8 => $name::Int8(DenseMap::new()),
                    PrimitiveType::Uint16 => $name::Uint16(DenseMap::new()),
                    PrimitiveType::Int16 => $name::Int16(DenseMap::new()),
                    PrimitiveType::Uint32 => $name::Uint32(DenseMap::new()),
                    PrimitiveType::Int32 => $name::Int32(DenseMap::new()),
                    PrimitiveType::Float32 => $name::Float32(DenseMap::new()),
                    PrimitiveType::Float64 => $name::Float64(DenseMap::new()),
                }
            }

            /// Reserves memory for at least `additional` new properties.
            pub fn reserve(&mut self, additional: hsize) {
                match self {
                    $name::Uint8(map) => map.reserve(additional),
                    $name::Int8(map) => map.reserve(additional),
                    $name::Uint16(map) => map.reserve(additional),
                    $name::Int16(map) => map.reserve(additional),
                    $name::Uint32(map) => map.reserve(additional),
                    $name::Int32(map) => map.reserve(additional),
                    $name::Float32(map) => map.reserve(additional),
                    $name::Float64(map) => map.reserve(additional),
                }
            }

            /// Returns the scalar type of the data currently stored in this
            /// map.
            pub fn primitive_type(&self) -> PrimitiveType {
                match self {
                    $name::Uint8(_) => PrimitiveType::Uint8,
                    $name::Int8(_) => PrimitiveType::Int8,
                    $name::Uint16(_) => PrimitiveType::Uint16,
                    $name::Int16(_) => PrimitiveType::Int16,
                    $name::Uint32(_) => PrimitiveType::Uint32,
                    $name::Int32(_) => PrimitiveType::Int32,
                    $name::Float32(_) => PrimitiveType::Float32,
                    $name::Float64(_) => PrimitiveType::Float64,
                }
            }

            /// Returns the property with the given handle, or `None` if no
            /// property is associated with that handle.
            ///
            /// The scalar type `T` must match the type that is currently
            /// stored in the map! Otherwise, this method panics.
            pub fn get<T: Primitive>(&self, handle: H) -> Option<$vec_type<T>> {
                macro_rules! get {
                    ($map:ident) => {{
                        $map.get(handle).map(|p| {
                            p.map(|s| *<dyn Any>::downcast_ref(&s).unwrap())
                        })
                    }}
                }

                // Make sure the inserted type matches the type of the map
                if T::TY != self.primitive_type() {
                    panic!(
                        "type mismatch requesting '{:?}' from an {} with type '{:?}'",
                        T::TY,
                        stringify!($name),
                        self.primitive_type(),
                    )
                }

                // Since we know here that the types match, all those `to_*`
                // convert functions won't ever return `None`. In fact, the
                // compiler can probably prove that since it has static type
                // information about `T`.
                match self {
                    $name::Uint8(map) => get!(map),
                    $name::Int8(map) => get!(map),
                    $name::Uint16(map) => get!(map),
                    $name::Int16(map) => get!(map),
                    $name::Uint32(map) => get!(map),
                    $name::Int32(map) => get!(map),
                    $name::Float32(map) => get!(map),
                    $name::Float64(map) => get!(map),
                }
            }

            pub fn get_casted_lossy<T: Primitive>(&self, handle: H) -> Option<$vec_type<T>> {
                macro_rules! get {
                    ($map:ident) => {{
                        $map.get(handle).map(|p| {
                            p.map(cast::lossy)
                        })
                    }}
                }

                match self {
                    $name::Uint8(map) => get!(map),
                    $name::Int8(map) => get!(map),
                    $name::Uint16(map) => get!(map),
                    $name::Int16(map) => get!(map),
                    $name::Uint32(map) => get!(map),
                    $name::Int32(map) => get!(map),
                    $name::Float32(map) => get!(map),
                    $name::Float64(map) => get!(map),
                }
            }

            /// Inserts a new property for the given handle. Overwrites old
            /// value if there was already something associated with `handle`.
            ///
            /// The scalar type `T` must match the type that is currently
            /// stored in the map! Otherwise, this method panics.
            pub fn insert<T: Primitive>(&mut self, handle: H, prop: $vec_type<T>) {
                // This function optimizes very well! As seen [here][godbolt],
                // it is just one check of our discriminant against a constant.
                // If the check fails, jump to panic, otherwise the value is
                // just pushed to the vector. Optimal, so to speak!
                //
                // [godbolt]: https://rust.godbolt.org/z/dyWROg

                macro_rules! insert {
                    ($map:ident, $convert:ident) => {{
                        $map.insert(handle, prop.map(|s| *<dyn Any>::downcast_ref(&s).unwrap()));
                    }}
                }

                // Make sure the inserted type matches the type of the map
                if T::TY != self.primitive_type() {
                    panic!(
                        "type mismatch inserting '{:?}' into an {} with type '{:?}'",
                        T::TY,
                        stringify!($name),
                        self.primitive_type(),
                    )
                }

                // Since we know here that the types match, all those `to_*`
                // convert functions won't ever return `None`. In fact, the
                // compiler can probably prove that since it has static type
                // information about `T`.
                match self {
                    $name::Uint8(map) => insert!(map, to_u8),
                    $name::Int8(map) => insert!(map, to_i8),
                    $name::Uint16(map) => insert!(map, to_u16),
                    $name::Int16(map) => insert!(map, to_i16),
                    $name::Uint32(map) => insert!(map, to_u32),
                    $name::Int32(map) => insert!(map, to_i32),
                    $name::Float32(map) => insert!(map, to_f32),
                    $name::Float64(map) => insert!(map, to_f64),
                }
            }

            /// A alias for `primitive_type` for use in macros generic over
            /// vector any maps and color any maps.
            #[allow(unused)]
            pub(crate) fn ty(&self) -> impl std::fmt::Debug {
                self.primitive_type()
            }
        }
    }
}


gen_vec3_any_map!(
    /// A property map that contains `Point3` values with a dynamic primitive
    /// scalar type.
    ///
    /// This map is only used to losslessly store IO data. For most purposes,
    /// you don't want to use this map.
    AnyPointMap => Point3
);
gen_vec3_any_map!(
    /// A property map that contains `Vector3` values with a dynamic primitive
    /// scalar type.
    ///
    /// This map is only used to losslessly store IO data. For most purposes,
    /// you don't want to use this map.
    AnyVectorMap => Vector3
);

/// A property map that contains color values with a dynamic primitive
/// channel type and with an optional alpha channel.
///
/// This map is only used to losslessly store IO data. For most purposes,
/// you don't want to use this map.
#[derive(Debug, Clone)]
pub enum AnyColorMap<H: Handle> {
    RgbUint8(DenseMap<H, [u8; 3]>),
    RgbUint16(DenseMap<H, [u16; 3]>),
    RgbUint32(DenseMap<H, [u32; 3]>),
    RgbFloat32(DenseMap<H, [f32; 3]>),
    RgbFloat64(DenseMap<H, [f64; 3]>),
    RgbaUint8(DenseMap<H, [u8; 4]>),
    RgbaUint16(DenseMap<H, [u16; 4]>),
    RgbaUint32(DenseMap<H, [u32; 4]>),
    RgbaFloat32(DenseMap<H, [f32; 4]>),
    RgbaFloat64(DenseMap<H, [f64; 4]>),
}

impl<H: Handle> AnyColorMap<H> {
    /// Creates a new instance of this map with the given color type.
    pub fn new<C>() -> Self
    where
        C: ColorLike<Channel: Primitive>,
    {
        match (C::HAS_ALPHA, C::Channel::channel_type()) {
            (false, PrimitiveColorChannelType::Uint8) => AnyColorMap::RgbUint8(DenseMap::new()),
            (false, PrimitiveColorChannelType::Uint16) => AnyColorMap::RgbUint16(DenseMap::new()),
            (false, PrimitiveColorChannelType::Uint32) => AnyColorMap::RgbUint32(DenseMap::new()),
            (false, PrimitiveColorChannelType::Float32) => AnyColorMap::RgbFloat32(DenseMap::new()),
            (false, PrimitiveColorChannelType::Float64) => AnyColorMap::RgbFloat64(DenseMap::new()),
            (true, PrimitiveColorChannelType::Uint8) => AnyColorMap::RgbaUint8(DenseMap::new()),
            (true, PrimitiveColorChannelType::Uint16) => AnyColorMap::RgbaUint16(DenseMap::new()),
            (true, PrimitiveColorChannelType::Uint32) => AnyColorMap::RgbaUint32(DenseMap::new()),
            (true, PrimitiveColorChannelType::Float32) => AnyColorMap::RgbaFloat32(DenseMap::new()),
            (true, PrimitiveColorChannelType::Float64) => AnyColorMap::RgbaFloat64(DenseMap::new()),
        }
    }

    /// Reserves memory for at least `additional` new properties.
    pub fn reserve(&mut self, additional: hsize) {
        match self {
            AnyColorMap::RgbUint8(map) => map.reserve(additional),
            AnyColorMap::RgbUint16(map) => map.reserve(additional),
            AnyColorMap::RgbUint32(map) => map.reserve(additional),
            AnyColorMap::RgbFloat32(map) => map.reserve(additional),
            AnyColorMap::RgbFloat64(map) => map.reserve(additional),
            AnyColorMap::RgbaUint8(map) => map.reserve(additional),
            AnyColorMap::RgbaUint16(map) => map.reserve(additional),
            AnyColorMap::RgbaUint32(map) => map.reserve(additional),
            AnyColorMap::RgbaFloat32(map) => map.reserve(additional),
            AnyColorMap::RgbaFloat64(map) => map.reserve(additional),
        }
    }

    /// Returns the color type of the data currently stored in this map.
    pub fn color_type(&self) -> ColorType {
        macro_rules! imp {
            ($( $variant:ident => ($alpha:literal, $ty:ident), )*) => {
                match self {
                    $(
                        AnyColorMap::$variant(_) => ColorType {
                            alpha: $alpha,
                            channel_type: PrimitiveColorChannelType::$ty,
                        },
                    )*
                }
            }
        }

        imp! {
            RgbUint8 => (false, Uint8),
            RgbUint16 => (false, Uint16),
            RgbUint32 => (false, Uint32),
            RgbFloat32 => (false, Float32),
            RgbFloat64 => (false, Float64),
            RgbaUint8 => (true, Uint8),
            RgbaUint16 => (true, Uint16),
            RgbaUint32 => (true, Uint32),
            RgbaFloat32 => (true, Float32),
            RgbaFloat64 => (true, Float64),
        }
    }

    /// Returns the property with the given handle, or `None` if no property is
    /// associated with that handle.
    ///
    /// The color type `C` must match the type that is currently stored in the
    /// map! Otherwise, this method panics.
    pub fn get<C>(&self, handle: H) -> Option<C>
    where
        C: ColorLike<Channel: Primitive>,
    {
        macro_rules! get {
            ($map:ident, $n:expr) => {{
                $map.get(handle).map(|c| {
                    ColorLike::convert(<dyn Any>::downcast_ref::<[C::Channel; $n]>(&*c).unwrap())
                })
            }}
        }

        // Make sure the inserted type matches the type of the map
        if ColorType::from_color_like::<C>() != self.color_type() {
            panic!(
                "type mismatch requesting '{:?}' from an AnyColorMap with type '{:?}'",
                ColorType::from_color_like::<C>(),
                self.color_type(),
            )
        }

        // Since we know here that the types match, all those `to_*`
        // convert functions won't ever return `None`. In fact, the
        // compiler can probably prove that since it has static type
        // information about `C`.
        match self {
            AnyColorMap::RgbUint8(map) => get!(map, 3),
            AnyColorMap::RgbUint16(map) => get!(map, 3),
            AnyColorMap::RgbUint32(map) => get!(map, 3),
            AnyColorMap::RgbFloat32(map) => get!(map, 3),
            AnyColorMap::RgbFloat64(map) => get!(map, 3),
            AnyColorMap::RgbaUint8(map) => get!(map, 4),
            AnyColorMap::RgbaUint16(map) => get!(map, 4),
            AnyColorMap::RgbaUint32(map) => get!(map, 4),
            AnyColorMap::RgbaFloat32(map) => get!(map, 4),
            AnyColorMap::RgbaFloat64(map) => get!(map, 4),
        }
    }

    /// Returns the property with the given handle casted into type `C`, or
    /// `None` if no property is associated with that handle.
    ///
    /// The actual stored type is casted via `ColorLike::cast` into the target
    /// type `C`.
    pub fn get_casted_lossy<C: ColorLike>(&self, handle: H) -> Option<C> {
        macro_rules! get {
            ($map:ident) => {{
                $map.get(handle).map(|c| ColorLike::cast(&*c))
            }}
        }

        match self {
            AnyColorMap::RgbUint8(map) => get!(map),
            AnyColorMap::RgbUint16(map) => get!(map),
            AnyColorMap::RgbUint32(map) => get!(map),
            AnyColorMap::RgbFloat32(map) => get!(map),
            AnyColorMap::RgbFloat64(map) => get!(map),
            AnyColorMap::RgbaUint8(map) => get!(map),
            AnyColorMap::RgbaUint16(map) => get!(map),
            AnyColorMap::RgbaUint32(map) => get!(map),
            AnyColorMap::RgbaFloat32(map) => get!(map),
            AnyColorMap::RgbaFloat64(map) => get!(map),
        }
    }

    /// Inserts a new property for the given handle. Overwrites old value if
    /// there was already something associated with `handle`.
    ///
    /// The color type `C` must match the type that is currently stored in the
    /// map! Otherwise, this method panics.
    pub fn insert<C>(&mut self, handle: H, prop: C)
    where
        C: ColorLike<Channel: Primitive>,
    {
        macro_rules! insert {
            ($map:ident, $n:expr) => {{
                let value = *<dyn Any>::downcast_ref(&ColorLike::convert::<[_; $n]>(&prop))
                    .unwrap();
                $map.insert(handle, value);
            }}
        }

        // Make sure the inserted type matches the type of the map
        if ColorType::from_color_like::<C>() != self.color_type() {
            panic!(
                "type mismatch inserting '{:?}' into an AnyColorMap with type '{:?}'",
                ColorType::from_color_like::<C>(),
                self.color_type(),
            )
        }

        match self {
            AnyColorMap::RgbUint8(map) => insert!(map, 3),
            AnyColorMap::RgbUint16(map) => insert!(map, 3),
            AnyColorMap::RgbUint32(map) => insert!(map, 3),
            AnyColorMap::RgbFloat32(map) => insert!(map, 3),
            AnyColorMap::RgbFloat64(map) => insert!(map, 3),
            AnyColorMap::RgbaUint8(map) => insert!(map, 4),
            AnyColorMap::RgbaUint16(map) => insert!(map, 4),
            AnyColorMap::RgbaUint32(map) => insert!(map, 4),
            AnyColorMap::RgbaFloat32(map) => insert!(map, 4),
            AnyColorMap::RgbaFloat64(map) => insert!(map, 4),
        }
    }

    /// A alias for `color_type` for use in macros generic over vector any maps
    /// and color any maps.
    #[allow(unused)]
    pub(crate) fn ty(&self) -> impl std::fmt::Debug {
        self.color_type()
    }
}
