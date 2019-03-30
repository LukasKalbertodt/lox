use super::{MeshMut};

/// Marker trait: implemented by meshes that support multi fan-blade vertices
/// (technically not 2-manifold).
///
/// TODO: more explanation and image.
pub trait SupportsMultiBlade: MeshMut {}
