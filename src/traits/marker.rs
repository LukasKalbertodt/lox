use crate::{
    sealed::Sealed,
};
use super::{MeshMut};

/// Marker trait: implemented by meshes that support multi fan-blade vertices
/// (technically not 2-manifold).
///
/// TODO: more explanation and image.
pub trait SupportsMultiBlade: MeshMut {}


/// A kind of faces a mesh data structure can store. Either [`TriFaces`] or
/// [`PolyFaces`].
///
/// This is a sealed trait, meaning you cannot implement it for your own types.
/// This library provides exactly two types that implement this trait.
pub trait FaceKind: Sealed {
    const ONLY_TRIANGLES: bool;
}

/// Only triangular faces are supported.
#[allow(missing_debug_implementations)]
pub enum TriFaces {}
impl Sealed for TriFaces {}
impl FaceKind for TriFaces {
    const ONLY_TRIANGLES: bool = true;
}

/// Arbitrary polygons are allowed as faces.
#[allow(missing_debug_implementations)]
pub enum PolyFaces {}
impl Sealed for PolyFaces {}
impl FaceKind for PolyFaces {
    const ONLY_TRIANGLES: bool = false;
}

/// Type level boolean. Only implemented by [`True`] and [`False`].
///
/// Once const generics land, this is not necessary anymore.
pub trait Bool: Sealed {
    const VALUE: bool;
}

/// Type level `true` boolean value.
#[allow(missing_debug_implementations)]
pub enum True {}
impl Sealed for True {}
impl Bool for True {
    const VALUE: bool = true;
}

/// Type level `false` boolean value.
#[allow(missing_debug_implementations)]
pub enum False {}
impl Sealed for False {}
impl Bool for False {
    const VALUE: bool = false;
}
