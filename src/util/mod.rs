use std::any::TypeId;

use cgmath::{
    Point3,
    prelude::*,
};

use crate::{
    cast,
    handle::hsize,
    prop::Pos3Like,
};


pub mod list;

pub use list::{TriList, DiList};


/// Extension trait to add some useful methods to any type implementing
/// `Iterator`.
pub trait IteratorExt: Sized + Iterator {
    fn into_vec(self) -> Vec<Self::Item> {
        self.collect()
    }

    fn centroid(mut self) -> Option<Self::Item>
    where
        Self::Item: Pos3Like,
    {
        self.next().map(|first| {
            let first = first.to_point3().to_vec();
            let (count, total_displacement) = self.fold((1, first), |(count, sum), p| {
                (count + 1, sum + p.to_point3().to_vec())
            });

            Point3::from_vec(total_displacement / cast::lossy(count)).convert()
        })
    }
}

impl<I: Iterator> IteratorExt for I {}


/// Represents potentially partial information about the number of elements in
/// a mesh.
///
/// There are some helper methods to get reasonable estimates from Euler's
/// formula in case some size information is missing. In particular, the
/// derived fact `|F| ≈ 2 * |V|` is of interest.
#[derive(Debug, Clone, Copy)]
pub struct MeshSizeHint {
    pub vertex_count: Option<hsize>,
    pub face_count: Option<hsize>,
}

impl MeshSizeHint {
    /// Returns an estimate of the number of vertices.
    ///
    /// If the number is already given, that number is returned. If not, but
    /// the face count is given, the vertex count is estimated from that.
    /// Otherwise 0 is returned.
    pub fn guess_vertex_count(&self) -> hsize {
        match (self.vertex_count, self.face_count) {
            (Some(v), _) => v,
            (None, Some(f)) => f / 2,
            (None, None)    => 0,
        }
    }

    /// Returns an estimate of the number of faces.
    ///
    /// If the number is already given, that number is returned. If not, but
    /// the vertex count is given, the face count is estimated from that.
    /// Otherwise 0 is returned.
    pub fn guess_face_count(&self) -> hsize {
        match (self.vertex_count, self.face_count) {
            (_, Some(f)) => f,
            (Some(v), None) => 2 * v,
            (None, None) => 0,
        }
    }
}

/// Returns `true` if both given types are the same type, `false` otherwise.
///
/// The types are required to be `'static` because comparing lifetimes for
/// equality is tricky. In particular because this function is implemented
/// using specialization which has known soundness holes regarding lifetimes.
pub fn are_same_type<T: 'static, U: 'static>() -> bool {
    TypeId::of::<T>() == TypeId::of::<U>()
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_are_same_type() {
        assert!(are_same_type::<u32, u32>());
        assert!(are_same_type::<i32, i32>());
        assert!(are_same_type::<String, String>());
        assert!(are_same_type::<bool, bool>());

        assert!(!are_same_type::<u32, i32>());
        assert!(!are_same_type::<u32, String>());
        assert!(!are_same_type::<bool, String>());
    }
}
