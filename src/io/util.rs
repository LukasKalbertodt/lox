//! Small utility items used in the `io` module.

use std::{
    marker::PhantomData,
};

use cgmath::{Point3, Vector3};
use stable_vec::StableVec;

use crate::{
    cast::{try_cast, is_cast_possible, CastRigor},
    handle::{hsize, Handle, VertexHandle, FaceHandle},
    io::{Error, MemSource, PrimitiveType, Primitive, PropKind},
    map::{PropMap, PropStoreMut, VecMap},
    math::{PrimitiveFloat},
    prop::{Pos3Like, Vec3Like},
};


/// Specifies preferred types for floating point numbers and integers.
///
/// This is used by [`MemSink`][super::MemSink] to signal preferred types to
/// the source. This is used in situations where the source does not have a
/// specific type but has a choice. If, for example, the source reads an ASCII
/// file in which positions are specified in standard `3.14` notation, it's not
/// immediately clear how the source should parse those numbers: parsing as
/// `f32` could loose precision; parsing as `f64` could be useless overhead if
/// the sink converts it back to `f32`. Similarly, if the source generates
/// values (e.g. a shape description), the same is true: it would be great if
/// the source would know the preferred type.
///
/// This trait is only implemented by [`WishFor<F, I>`][WishFor] where `F` is
/// the float type and `I` is the integer type. I don't think implementing this
/// trait for your own types makes any sense. The default for most things is
/// `WishFor<f32, i32>`, see [`DefaultTypeWishes`].
pub trait TypeWish {
    /// The specific type that should be used, if floating point numbers are
    /// available.
    type Float: PrimitiveFloat + Primitive;

    /// The specific type that should be used, if integers are available.
    type Integer: Primitive;
}

/// The default type wish: `f32` as float type, `i32` as integer type.
pub type DefaultTypeWishes = WishFor<f32, i32>;

/// Implements [`TypeWish`] with the float type `F` and the integer type `I`.
///
/// This type is only used at the type level and cannot be created nor used at
/// runtime.
#[allow(unreachable_code)] // TODO: see #38885
#[derive(Debug)]
pub struct WishFor<F: PrimitiveFloat + Primitive, I: Primitive>(!, PhantomData<F>, PhantomData<I>);

impl<F: PrimitiveFloat + Primitive, I: Primitive> TypeWish for WishFor<F, I> {
    type Float = F;
    type Integer = I;
}

/// Type to only overwrite the wish for the float type and keep the default
/// integer type.
pub type WishFloat<F> = WishFor<F, <DefaultTypeWishes as TypeWish>::Integer>;

/// Type to only overwrite the wish for the integer type and keep the default
/// float type.
pub type WishInteger<I> = WishFor<<DefaultTypeWishes as TypeWish>::Float, I>;


/// A type wish which overwrites either `Float` or `Integer` (depending on `P`)
/// and keeps the other type as default.
///
/// If `P` is an integer, the `Integer` type is overwritten, otherwise the
/// `Float` type is overwritten.
#[allow(unreachable_code)] // TODO: see #38885
#[derive(Debug)]
pub struct OverwriteFor<P: Primitive>(!, PhantomData<P>);

macro_rules! impl_overwrite {
    ($($ty:ident => ($f:ty, $i:ty),)*) => {
        $(
            impl TypeWish for OverwriteFor<$ty> {
                type Float = $f;
                type Integer = $i;
            }
        )*
    }
}

impl_overwrite!(
    u8 => (<DefaultTypeWishes as TypeWish>::Float, u8),
    i8 => (<DefaultTypeWishes as TypeWish>::Float, i8),
    u16 => (<DefaultTypeWishes as TypeWish>::Float, u16),
    i16 => (<DefaultTypeWishes as TypeWish>::Float, i16),
    u32 => (<DefaultTypeWishes as TypeWish>::Float, u32),
    i32 => (<DefaultTypeWishes as TypeWish>::Float, i32),
    f32 => (f32, <DefaultTypeWishes as TypeWish>::Integer),
    f64 => (f64, <DefaultTypeWishes as TypeWish>::Integer),
);


/// The result of inspecting the start of the file to check if it's a file of a
/// specific format.
///
/// This is returned by the `is_file_start` functions in each file format
/// module.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IsFormat {
    /// The file is very likely a file of the specified format.
    ///
    /// This should be returned when there are strong indicators of the
    /// specified format (e.g. the magic number is found). The `is_file_start`
    /// function is not required to already try parsing the file and properly
    /// check for errors. Instead, quick and easy indicators should be used.
    Probably,

    /// The file could be a file of the specified format, but there is no clear
    /// indicator that it is.
    ///
    /// This should be returned as rarely as possible. It's only necessary when
    /// a file format does not have a magic number or something like that.
    Maybe,

    /// The file is definitely not valid in the specified format (e.g. a magic
    /// number is not found).
    No,
}

/// Adds additional convenience methods to all types that implement
/// `MemSource`. *Is reexported in `prelude`*.
pub trait MemSourceExt {
    /// Provides new vertex positions to the source. The returned source uses
    /// the given vertex positions instead of the original.
    ///
    /// This also works if the original source does not offer vertex positions.
    /// Note that you have to specify the cast rigor parameter `R` explicitly.
    fn with_vertex_positions<'a, R, M>(
        &'a self,
        vertex_positions: &'a M,
    ) -> SourceWithVertexPositions<'a, Self, M, R>
    where
        R: CastRigor,
        M: PropMap<VertexHandle>,
        M::Target: Pos3Like,
        <M::Target as Pos3Like>::Scalar: Primitive,
    {
        SourceWithVertexPositions {
            original: self,
            vertex_positions,
            _dummy: PhantomData,
        }
    }

    /// Provides new vertex normals to the source. The returned source uses the
    /// given vertex normals instead of the original.
    ///
    /// This also works if the original source does not offer vertex normals.
    /// Note that you have to specify the cast rigor parameter `R` explicitly.
    fn with_vertex_normals<'a, R, M>(
        &'a self,
        vertex_normals: &'a M,
    ) -> SourceWithVertexNormals<'a, Self, M, R>
    where
        R: CastRigor,
        M: PropMap<VertexHandle>,
        M::Target: Vec3Like,
        <M::Target as Vec3Like>::Scalar: Primitive,
    {
        SourceWithVertexNormals {
            original: self,
            vertex_normals,
            _dummy: PhantomData,
        }
    }

    /// Provides new face normals to the source. The returned source uses the
    /// given face normals instead of the original.
    ///
    /// This also works if the original source does not offer face normals.
    /// Note that you have to specify the cast rigor parameter `R` explicitly.
    fn with_face_normals<'a, R, M>(
        &'a self,
        face_normals: &'a M,
    ) -> SourceWithFaceNormals<'a, Self, M, R>
    where
        R: CastRigor,
        M: PropMap<FaceHandle>,
        M::Target: Vec3Like,
        <M::Target as Vec3Like>::Scalar: Primitive,
    {
        SourceWithFaceNormals {
            original: self,
            face_normals,
            _dummy: PhantomData,
        }
    }
}

impl<T: MemSource> MemSourceExt for T {}


// ===========================================================================
// ===== MemSource adaptors
// ===========================================================================

macro_rules! old_impl_items {
    (core_mesh) => {
        type CoreMesh = S::CoreMesh;
        fn core_mesh(&self) -> &Self::CoreMesh {
            self.original.core_mesh()
        }
    };
    (vertex_position) => {
        fn vertex_position_type(&self) -> Option<PrimitiveType> {
            self.original.vertex_position_type()
        }
        fn vertex_position<T: Primitive>(
            &self,
            v: VertexHandle,
        ) -> Result<Option<Point3<T>>, Error> {
            self.original.vertex_position(v)
        }
    };
    (vertex_normal) => {
        fn vertex_normal_type(&self) -> Option<PrimitiveType> {
            self.original.vertex_normal_type()
        }
        fn vertex_normal<T: Primitive>(
            &self,
            v: VertexHandle,
        ) -> Result<Option<Vector3<T>>, Error> {
            self.original.vertex_normal(v)
        }
    };
    (face_normal) => {
        fn face_normal_type(&self) -> Option<PrimitiveType> {
            self.original.face_normal_type()
        }
        fn face_normal<T: Primitive>(&self, v: FaceHandle) -> Result<Option<Vector3<T>>, Error> {
            self.original.face_normal(v)
        }
    };
}


/// `MemSource` adaptor. See [`MemSourceExt::with_vertex_positions`].
#[derive(Copy, Clone, Debug)]
pub struct SourceWithVertexPositions<'a, S: ?Sized, M, R: CastRigor> {
    original: &'a S,
    vertex_positions: &'a M,
    _dummy: PhantomData<R>,
}

impl<S: ?Sized, M, R: CastRigor> MemSource for SourceWithVertexPositions<'_, S, M, R>
where
    S: MemSource,
    M: PropMap<VertexHandle>,
    M::Target: Pos3Like,
    <M::Target as Pos3Like>::Scalar: Primitive,
{
    old_impl_items!(core_mesh);
    old_impl_items!(vertex_normal);
    old_impl_items!(face_normal);

    fn vertex_position_type(&self) -> Option<PrimitiveType> {
        Some(<M::Target as Pos3Like>::Scalar::TY)
    }

    fn vertex_position<T: Primitive>(&self, f: VertexHandle) -> Result<Option<Point3<T>>, Error> {
        if !is_cast_possible::<R, <M::Target as Pos3Like>::Scalar, T>() {
            return Err(Error::SourceIncompatible {
                prop: PropKind::VertexNormal,
                requested_type: T::TY,
            });
        }

        let out = self.vertex_positions.get(f).map(|p| {
            p.map_scalar(|s| try_cast::<R, _, _>(s).expect("internal bug in `lox::cast` module"))
        });

        Ok(out)
    }
}

/// `MemSource` adaptor. See [`MemSourceExt::with_vertex_normals`].
#[derive(Copy, Clone, Debug)]
pub struct SourceWithVertexNormals<'a, S: ?Sized, M, R: CastRigor> {
    original: &'a S,
    vertex_normals: &'a M,
    _dummy: PhantomData<R>,
}

impl<S: ?Sized, M, R: CastRigor> MemSource for SourceWithVertexNormals<'_, S, M, R>
where
    S: MemSource,
    M: PropMap<VertexHandle>,
    M::Target: Vec3Like,
    <M::Target as Vec3Like>::Scalar: Primitive,
{
    old_impl_items!(core_mesh);
    old_impl_items!(vertex_position);
    old_impl_items!(face_normal);

    fn vertex_normal_type(&self) -> Option<PrimitiveType> {
        Some(<M::Target as Vec3Like>::Scalar::TY)
    }

    fn vertex_normal<T: Primitive>(&self, f: VertexHandle) -> Result<Option<Vector3<T>>, Error> {
        if !is_cast_possible::<R, <M::Target as Vec3Like>::Scalar, T>() {
            return Err(Error::SourceIncompatible {
                prop: PropKind::VertexNormal,
                requested_type: T::TY,
            });
        }

        let out = self.vertex_normals.get(f).map(|p| {
            p.map_scalar(|s| try_cast::<R, _, _>(s).expect("internal bug in `lox::cast` module"))
        });

        Ok(out)
    }
}

/// `MemSource` adaptor. See [`MemSourceExt::with_face_normals`].
#[derive(Copy, Clone, Debug)]
pub struct SourceWithFaceNormals<'a, S: ?Sized, M, R: CastRigor> {
    original: &'a S,
    face_normals: &'a M,
    _dummy: PhantomData<R>,
}

impl<S: ?Sized, M, R: CastRigor> MemSource for SourceWithFaceNormals<'_, S, M, R>
where
    S: MemSource,
    M: PropMap<FaceHandle>,
    M::Target: Vec3Like,
    <M::Target as Vec3Like>::Scalar: Primitive,
{
    old_impl_items!(core_mesh);
    old_impl_items!(vertex_position);
    old_impl_items!(vertex_normal);

    fn face_normal_type(&self) -> Option<PrimitiveType> {
        Some(<M::Target as Vec3Like>::Scalar::TY)
    }

    fn face_normal<T: Primitive>(&self, f: FaceHandle) -> Result<Option<Vector3<T>>, Error> {
        if !is_cast_possible::<R, <M::Target as Vec3Like>::Scalar, T>() {
            return Err(Error::SourceIncompatible {
                prop: PropKind::FaceNormal,
                requested_type: T::TY,
            });
        }

        let out = self.face_normals.get(f).map(|p| {
            p.map_scalar(|s| try_cast::<R, _, _>(s).expect("internal bug in `lox::cast` module"))
        });

        Ok(out)
    }
}

/// Internal helper map to map from indices to handles.
///
/// When reading files, the files usually refer to vertices by index in the
/// file. We also use indices (handles) to refer to vertices. However, we can't
/// guarantee how meshes return handles. In most cases, meshes also return
/// handles by just counting up (0, 1, 2, ...). But sometimes, in particular
/// when the mesh already contains vertices, the handle values do not match the
/// indices in the file. Thus, we need an index -> handle map.
///
/// But we don't want to have a huge overhead for a special case. Therefore,
/// this type is optimized for the common case.
#[derive(Debug)]
pub(super) enum IndexHandleMap<H: Handle> {
    /// All indices were equal to the handles and started at 0.
    Block {
        len: usize,
    },
    /// Something else
    Arbitrary {
        map: StableVec<H>,
    },
    // TODO: think about implementing another special case: handles start from
    // an offset, but then just count up.
}

impl<H: Handle> IndexHandleMap<H> {
    pub(super) fn new() -> Self {
        IndexHandleMap::Block { len: 0 }
    }

    pub(super) fn add(&mut self, index: usize, handle: H) {
        match self {
            IndexHandleMap::Block { len } => {
                // This is the expected case: handles and indices have the same
                // value and are simply counting up.
                if handle.to_usize() == index && *len == index {
                    *len += 1;
                } else {
                    // This is bad: now we have to convert this simple map into
                    // the arbitrary map. First transfer all old values.
                    let mut map = StableVec::with_capacity(*len + 1);
                    for i in 0..*len {
                        map.push(H::from_usize(i));
                    }

                    // Insert new value
                    Self::insert_into_sv(&mut map, index, handle);

                    *self = IndexHandleMap::Arbitrary { map };
                }
            }
            IndexHandleMap::Arbitrary { map } => {
                Self::insert_into_sv(map, index, handle);
            }
        }
    }

    pub(super) fn get(&self, index: usize) -> Option<H> {
        match self {
            IndexHandleMap::Block { len } => {
                if index < *len {
                    Some(H::from_usize(index))
                } else {
                    None
                }
            }
            IndexHandleMap::Arbitrary { map } => {
                map.get(index).cloned()
            }
        }
    }

    fn insert_into_sv(sv: &mut StableVec<H>, idx: usize, elem: H) {
        if sv.has_element_at(idx) {
            panic!("IndexHandleMap::insert called with the same index twice");
        } else {
            // Make sure `idx` is not out of bounds by growing the vector if
            // necessary.
            let next_index = sv.next_index();
            if next_index <= idx {
                sv.grow(1 + idx - next_index);
            }

            // We made sure that there is no element at `idx` and that `idx`
            // is not out of bounds. So we can unwrap here.
            sv.insert_into_hole(idx, elem).ok().unwrap();
        }
    }
}

/// Internal helper map to map from handles to indices.
///
/// This is [`IndexHandleMap`] the other way around. See its documentation for
/// more information.
#[derive(Debug)]
pub(super) enum HandleIndexMap<H: Handle> {
    /// All indices were equal to the handles and started at 0.
    Block {
        len: hsize,
    },
    /// Something else
    Arbitrary {
        map: VecMap<H, hsize>,
    },
}

impl<H: Handle> HandleIndexMap<H> {
    pub(super) fn new() -> Self {
        HandleIndexMap::Block { len: 0 }
    }

    pub(super) fn add(&mut self, handle: H, index: hsize) {
        match self {
            HandleIndexMap::Block { len } => {
                // This is the expected case: handles and indices have the same
                // value and are simply counting up.
                if handle.idx() == index && *len == index {
                    *len += 1;
                } else {
                    // This is bad: now we have to convert this simple map into
                    // the arbitrary map. First transfer all old values.
                    let mut map = VecMap::with_capacity(*len + 1);
                    for i in 0..*len {
                        map.insert(H::new(i), i);
                    }

                    // Insert new value
                    map.insert(handle, index);

                    *self = HandleIndexMap::Arbitrary { map };
                }
            }
            HandleIndexMap::Arbitrary { map } => {
                map.insert(handle, index);
            }
        }
    }

    pub(super) fn get(&self, handle: H) -> Option<hsize> {
        match self {
            HandleIndexMap::Block { len } => {
                if handle.idx() < *len {
                    Some(handle.idx())
                } else {
                    None
                }
            }
            HandleIndexMap::Arbitrary { map } => {
                map.get(handle).map(|boo| *boo)
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn index_handle_map_nice() {
        let fh = FaceHandle::from_usize;

        let mut map = IndexHandleMap::new();
        assert_eq!(map.get(0), None);
        assert_eq!(map.get(1), None);

        map.add(0, fh(0));
        assert_eq!(map.get(0), Some(fh(0)));
        assert_eq!(map.get(1), None);

        map.add(1, fh(1));
        assert_eq!(map.get(0), Some(fh(0)));
        assert_eq!(map.get(1), Some(fh(1)));
    }

    #[test]
    fn index_handle_map_bad() {
        let fh = FaceHandle::from_usize;

        let mut map = IndexHandleMap::new();
        map.add(0, fh(1));
        assert_eq!(map.get(0), Some(fh(1)));
        assert_eq!(map.get(1), None);
        assert_eq!(map.get(2), None);

        let mut map = IndexHandleMap::new();
        map.add(1, fh(0));
        assert_eq!(map.get(0), None);
        assert_eq!(map.get(1), Some(fh(0)));
        assert_eq!(map.get(2), None);

        let mut map = IndexHandleMap::new();
        map.add(0, fh(0));
        map.add(1, fh(2));
        assert_eq!(map.get(0), Some(fh(0)));
        assert_eq!(map.get(1), Some(fh(2)));
        assert_eq!(map.get(2), None);

        let mut map = IndexHandleMap::new();
        map.add(0, fh(0));
        map.add(2, fh(1));
        assert_eq!(map.get(0), Some(fh(0)));
        assert_eq!(map.get(1), None);
        assert_eq!(map.get(2), Some(fh(1)));

        map.add(3, fh(2));
        assert_eq!(map.get(0), Some(fh(0)));
        assert_eq!(map.get(1), None);
        assert_eq!(map.get(2), Some(fh(1)));
        assert_eq!(map.get(3), Some(fh(2)));

        map.add(6, fh(3));
        assert_eq!(map.get(0), Some(fh(0)));
        assert_eq!(map.get(1), None);
        assert_eq!(map.get(2), Some(fh(1)));
        assert_eq!(map.get(3), Some(fh(2)));
        assert_eq!(map.get(4), None);
        assert_eq!(map.get(4), None);
        assert_eq!(map.get(6), Some(fh(3)));
    }

    #[test]
    fn handle_index_map_nice() {
        let fh = FaceHandle::new;

        let mut map = HandleIndexMap::new();
        assert_eq!(map.get(fh(0)), None);
        assert_eq!(map.get(fh(1)), None);

        map.add(fh(0), 0);
        assert_eq!(map.get(fh(0)), Some(0));
        assert_eq!(map.get(fh(1)), None);

        map.add(fh(1), 1);
        assert_eq!(map.get(fh(0)), Some(0));
        assert_eq!(map.get(fh(1)), Some(1));
    }

    #[test]
    fn handle_index_map_bad() {
        let fh = FaceHandle::from_usize;

        let mut map = HandleIndexMap::new();
        map.add(fh(0), 1);
        assert_eq!(map.get(fh(0)), Some(1));
        assert_eq!(map.get(fh(1)), None);
        assert_eq!(map.get(fh(2)), None);

        let mut map = HandleIndexMap::new();
        map.add(fh(1), 0);
        assert_eq!(map.get(fh(0)), None);
        assert_eq!(map.get(fh(1)), Some(0));
        assert_eq!(map.get(fh(2)), None);

        let mut map = HandleIndexMap::new();
        map.add(fh(0), 0);
        map.add(fh(1), 2);
        assert_eq!(map.get(fh(0)), Some(0));
        assert_eq!(map.get(fh(1)), Some(2));
        assert_eq!(map.get(fh(2)), None);

        let mut map = HandleIndexMap::new();
        map.add(fh(0), 0);
        map.add(fh(2), 1);
        assert_eq!(map.get(fh(0)), Some(0));
        assert_eq!(map.get(fh(1)), None);
        assert_eq!(map.get(fh(2)), Some(1));

        map.add(fh(3), 2);
        assert_eq!(map.get(fh(0)), Some(0));
        assert_eq!(map.get(fh(1)), None);
        assert_eq!(map.get(fh(2)), Some(1));
        assert_eq!(map.get(fh(3)), Some(2));

        map.add(fh(6), 3);
        assert_eq!(map.get(fh(0)), Some(0));
        assert_eq!(map.get(fh(1)), None);
        assert_eq!(map.get(fh(2)), Some(1));
        assert_eq!(map.get(fh(3)), Some(2));
        assert_eq!(map.get(fh(4)), None);
        assert_eq!(map.get(fh(4)), None);
        assert_eq!(map.get(fh(6)), Some(3));
    }
}
