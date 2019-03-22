use cgmath::{Point3, Vector3};

use crate::{
    self as lox, // for proc macros
    prelude::*,
    cast,
    ds::SharedVertexMesh,
    handle::{hsize},
    map::VecMap,
    io::{Error, Primitive, PrimitiveType},
    util::downcast_as,
};


/// A fat mesh with dynamically typed properties intended to losslessly store
/// IO mesh data.
///
/// This is a rather special purpose type. For most cases, you want to use a
/// fat mesh that stores properties with specific convenient types. Instead,
/// this type is intended for use where you deal with very generic inputs and
/// you want to ensure lossless and exact storage. Notably, the dynamic typing
/// incurs some runtime overhead you usually want to avoid. Additionally, the
/// core mesh is always a `SharedVertexMesh` which is not very powerful but
/// sufficient for IO operations.
///
/// The `MemSource` implementation always casts to the requested type, so the
/// property getters never fail.
#[derive(Debug, Empty)]
pub struct AnyMesh {
    pub mesh: SharedVertexMesh,
    pub vertex_positions: Option<AnyPointMap<VertexHandle>>,
}

impl MemSink for AnyMesh {
    fn add_vertex(&mut self) -> VertexHandle {
        self.mesh.add_vertex()
    }
    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle {
        self.mesh.add_face(vertices)
    }

    fn prepare_vertex_positions<N: Primitive>(&mut self, count: hsize) -> Result<(), Error> {
        let mut map = AnyPointMap::new::<N>();
        map.reserve(count);
        self.vertex_positions = Some(map);
        Ok(())
    }

    fn set_vertex_position<N: Primitive>(
        &mut self,
        handle: VertexHandle,
        position: Point3<N>,
    ) {
        self.vertex_positions.as_mut().unwrap().insert(handle, position);
    }
}


impl MemSource for AnyMesh {
    type CoreMesh = SharedVertexMesh;
    fn core_mesh(&self) -> &Self::CoreMesh {
        &self.mesh
    }

    fn vertex_position_type(&self) -> Option<PrimitiveType> {
        self.vertex_positions.as_ref().map(|m| m.primitive_type())
    }
    fn vertex_position<T: Primitive>(&self, v: VertexHandle) -> Result<Option<Point3<T>>, Error> {
        let out = self.vertex_positions
            .as_ref()
            .expect("requested non-existent vertex position from `AnyMesh`")
            .get_casted_lossy(v);

        Ok(out)
    }
}


macro_rules! gen_vec3_any_map {
    ($(#[$attr:meta])* $name:ident => $vec_type:ident) => {
        $(#[$attr])*
        #[derive(Debug, Clone)]
        pub enum $name<H: Handle> {
            Uint8(VecMap<H, $vec_type<u8>>),
            Uint16(VecMap<H, $vec_type<u16>>),
            Uint32(VecMap<H, $vec_type<u32>>),
            Int8(VecMap<H, $vec_type<i8>>),
            Int16(VecMap<H, $vec_type<i16>>),
            Int32(VecMap<H, $vec_type<i32>>),
            Float32(VecMap<H, $vec_type<f32>>),
            Float64(VecMap<H, $vec_type<f64>>),
        }

        impl<H: Handle> $name<H> {
            /// Creates a new instance of this map with the given scalar type.
            pub fn new<T: Primitive>() -> Self {
                match T::TY {
                    PrimitiveType::Uint8 => $name::Uint8(VecMap::new()),
                    PrimitiveType::Int8 => $name::Int8(VecMap::new()),
                    PrimitiveType::Uint16 => $name::Uint16(VecMap::new()),
                    PrimitiveType::Int16 => $name::Int16(VecMap::new()),
                    PrimitiveType::Uint32 => $name::Uint32(VecMap::new()),
                    PrimitiveType::Int32 => $name::Int32(VecMap::new()),
                    PrimitiveType::Float32 => $name::Float32(VecMap::new()),
                    PrimitiveType::Float64 => $name::Float64(VecMap::new()),
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
                            p.map(|s| downcast_as(s).unwrap())
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
                        $map.insert(handle, prop.map(|s| downcast_as(s).unwrap()));
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