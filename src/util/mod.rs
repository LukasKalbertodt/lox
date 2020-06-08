use std::array;

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

pub use list::{DynList, TriList, DiList};

/// Extension trait, adding `map` to arrays of common sizes.
pub trait ArrayMapExt<OutT>: ArrayExt {
    type Out;

    /// Maps each element of the array and returns a new array with the
    /// results.
    fn map<F>(self, mapping: F) -> Self::Out
    where
        F: FnMut(Self::Item) -> OutT;
}

/// An extension traits with useful methods for arrays of common sizes.
///
/// Ideally, those methods would exist for all arrays, but as Rust doesn't
/// offer const generics yet, this is impossible. To avoid duplicate code in
/// several places, it's still useful to have this extension trait for the most
/// common array length in this library.
pub trait ArrayExt {
    type Item;
    type IntoIter;

    fn owned_iter(self) -> Self::IntoIter;
}

impl<T> ArrayExt for [T; 3] {
    type Item = T;
    type IntoIter = array::IntoIter<T, 3>;

    fn owned_iter(self) -> Self::IntoIter {
        Self::IntoIter::new(self)
    }
}
impl<T, OutT> ArrayMapExt<OutT> for [T; 3] {
    type Out = [OutT; 3];

    #[inline(always)]
    fn map<F>(self, mut mapping: F) -> Self::Out
    where
        F: FnMut(Self::Item) -> OutT
    {
        let [a, b, c] = self;
        [mapping(a), mapping(b), mapping(c)]
    }
}

impl<'a, T> ArrayExt for &'a [T; 3] {
    type Item = &'a T;
    type IntoIter = array::IntoIter<&'a T, 3>;

    fn owned_iter(self) -> Self::IntoIter {
        let [a, b, c] = self;
        Self::IntoIter::new([a, b, c])
    }
}
impl<T, OutT> ArrayMapExt<OutT> for &[T; 3] {
    type Out = [OutT; 3];

    #[inline(always)]
    fn map<F>(self, mut mapping: F) -> Self::Out
    where
        F: FnMut(Self::Item) -> OutT
    {
        let [a, b, c] = self;
        [mapping(a), mapping(b), mapping(c)]
    }
}

impl<T> ArrayExt for [T; 4] {
    type Item = T;
    type IntoIter = array::IntoIter<T, 4>;

    fn owned_iter(self) -> Self::IntoIter {
        Self::IntoIter::new(self)
    }
}
impl<T, OutT> ArrayMapExt<OutT> for [T; 4] {
    type Out = [OutT; 4];

    #[inline(always)]
    fn map<F>(self, mut mapping: F) -> Self::Out
    where
        F: FnMut(Self::Item) -> OutT
    {
        let [a, b, c, d] = self;
        [mapping(a), mapping(b), mapping(c), mapping(d)]
    }
}

impl<'a, T: Copy> ArrayExt for &'a [T; 4] {
    type Item = &'a T;
    type IntoIter = array::IntoIter<&'a T, 4>;

    fn owned_iter(self) -> Self::IntoIter {
        let [a, b, c, d] = self;
        Self::IntoIter::new([a, b, c, d])
    }
}
impl<T: Copy, OutT> ArrayMapExt<OutT> for &[T; 4] {
    type Out = [OutT; 4];

    #[inline(always)]
    fn map<F>(self, mut mapping: F) -> Self::Out
    where
        F: FnMut(Self::Item) -> OutT
    {
        let [a, b, c, d] = self;
        [mapping(a), mapping(b), mapping(c), mapping(d)]
    }
}


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
    trait SameType {
        const ANSWER: bool;
    }

    impl<T: 'static, U: 'static> SameType for (T, U) {
        default const ANSWER: bool = false;
    }

    impl<T: 'static> SameType for (T, T) {
        const ANSWER: bool = true;
    }

    <(T, U) as SameType>::ANSWER
}

/// Returns the exact input if both given types `I` and `O` are the same type,
/// `None` otherwise.
///
/// This function makes only sense in generic contexts where you want to get a
/// specific type from a generic one.
///
/// The types are required to be `'static` because comparing lifetimes for
/// equality is tricky. In particular because this function is implemented
/// using specialization which has known soundness holes regarding lifetimes.
pub fn downcast_as<I: 'static, O: 'static>(input: I) -> Option<O> {
    trait DowncastAs<O> {
        fn downcast_as(self) -> Option<O>;
    }

    impl<I: 'static, O: 'static> DowncastAs<O> for I {
        default fn downcast_as(self) -> Option<O> {
            None
        }
    }

    impl<T: 'static> DowncastAs<T> for T {
        default fn downcast_as(self) -> Option<T> {
            Some(self)
        }
    }

    DowncastAs::<O>::downcast_as(input)
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

    #[test]
    fn test_downcast_as() {
        assert_eq!(downcast_as::<_, i32>(0i32), Some(0i32));
        assert_eq!(downcast_as::<_, String>("hi".to_string()), Some("hi".to_string()));
        assert_eq!(downcast_as::<_, bool>(true), Some(true));

        assert_eq!(downcast_as::<_, u32>(0i32), None);
        assert_eq!(downcast_as::<_, Option<u32>>("hi".to_string()), None);
        assert_eq!(downcast_as::<_, u8>(true), None);
    }
}
