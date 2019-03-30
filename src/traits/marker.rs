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
pub trait FaceKind: Sealed {}

/// Only triangular faces are supported.
#[allow(missing_debug_implementations)]
pub enum TriFaces {}
impl Sealed for TriFaces {}
impl FaceKind for TriFaces {}

/// Arbitrary polygons are allowed as faces.
#[allow(missing_debug_implementations)]
pub enum PolyFaces {}
impl Sealed for PolyFaces {}
impl FaceKind for PolyFaces {}
