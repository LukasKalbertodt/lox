use std::fmt;

use cgmath::{
    Point3,
    prelude::*,
};

use crate::{
    cast,
    handle::hsize,
    math::PrimitiveFloat,
    prop::Pos3Like,
};


pub mod list;

pub use list::{DynList, TriList, DiList};



/// An extension traits with useful methods for arrays of size 3.
///
/// Ideally, those methods would exist for all arrays, but as Rust doesn't
/// offer const generics yet, this is impossible. To avoid duplicate code in
/// several places, it's still useful to have this extension trait for the most
/// common array length in this library.
pub trait TriArrayExt {
    type Item;

    /// Maps each element of the array and returns a new array with the
    /// results.
    fn map<F, OutT>(self, mapping: F) -> [OutT; 3]
    where
        F: FnMut(Self::Item) -> OutT;

    fn owned_iter(self) -> TriArrayIntoIter<Self::Item>
    where
        Self::Item: Copy;
}

impl<T> TriArrayExt for [T; 3] {
    type Item = T;

    #[inline(always)]
    fn map<F, OutT>(self, mut mapping: F) -> [OutT; 3]
    where
        F: FnMut(Self::Item) -> OutT,
    {
        let [a, b, c] = self;
        [mapping(a), mapping(b), mapping(c)]
    }

    fn owned_iter(self) -> TriArrayIntoIter<Self::Item>
    where
        Self::Item: Copy
    {
        TriArrayIntoIter::new(self)
    }
}

impl<'a, T> TriArrayExt for &'a [T; 3] {
    type Item = &'a T;

    fn map<F, OutT>(self, mut mapping: F) -> [OutT; 3]
    where
        F: FnMut(Self::Item) -> OutT,
    {
        let [a, b, c] = self;
        [mapping(a), mapping(b), mapping(c)]
    }

    fn owned_iter(self) -> TriArrayIntoIter<Self::Item>
    where
        Self::Item: Copy
    {
        TriArrayIntoIter::new([&self[0], &self[1], &self[2]])
    }
}

#[derive(Debug)]
pub struct TriArrayIntoIter<T: Copy> {
    arr: [T; 3],
    pos: usize,
}

impl<T: Copy> TriArrayIntoIter<T> {
    fn new(arr: [T; 3]) -> Self {
        Self {
            arr,
            pos: 0,
        }
    }
}

impl<T: Copy> Iterator for TriArrayIntoIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= 3 {
            return None;
        }

        self.pos += 1;
        Some(self.arr[self.pos - 1])
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


/// Format bytes either as slice of hexadecimal numbers or, if all data is
/// valid ASCII, as a string.
pub(crate) fn debug_fmt_bytes(data: &[u8]) -> String {
    if data.is_ascii() {
        format!("{:?}", std::str::from_utf8(data).unwrap())
    } else {
        format!("{:02x?}", data)
    }
}

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

/// An axis aligned bounding box.
pub struct BoundingBox<F: PrimitiveFloat> {
    x_range: [F; 2],
    y_range: [F; 2],
    z_range: [F; 2],
}

impl<F: PrimitiveFloat> BoundingBox<F> {
    /// Creates an invalid bounding box: all lower bounds are ∞, all upper
    /// bounds are -∞. Once you added a single point, the bounding box will be
    /// valid.
    pub fn new() -> Self {
        Self {
            x_range: [F::infinity(), F::neg_infinity()],
            y_range: [F::infinity(), F::neg_infinity()],
            z_range: [F::infinity(), F::neg_infinity()],
        }
    }

    /// Creates a bounding box around all points of the given iterator. If the
    /// iterator is empty, an invalid bounding box is returned (see
    /// [`BoundingBox::new`]).
    pub fn around<I>(iter: I) -> Self
    where
        I: IntoIterator,
        I::Item: Pos3Like<Scalar = F>,
    {
        let mut out = Self::new();
        for pos in iter {
            out.add_point(pos);
        }
        out
    }

    /// Returns the `[lower, upper]` limits for the x coordinate.
    pub fn x(&self) -> [F; 2] {
        self.x_range
    }

    /// Returns the `[lower, upper]` limits for the y coordinate.
    pub fn y(&self) -> [F; 2] {
        self.y_range
    }

    /// Returns the `[lower, upper]` limits for the z coordinate.
    pub fn z(&self) -> [F; 2] {
        self.z_range
    }

    /// Adds a point to the bounding box, enlarging it if the point lies
    /// outside of the box.
    pub fn add_point<P: Pos3Like<Scalar = F>>(&mut self, p: P) {
        fn min<F: PartialOrd>(state: &mut F, new: F) {
            if new < *state {
                *state = new;
            }
        }
        fn max<F: PartialOrd>(state: &mut F, new: F) {
            if new > *state {
                *state = new;
            }
        }


        min(&mut self.x_range[0], p.x());
        max(&mut self.x_range[1], p.x());
        min(&mut self.y_range[0], p.y());
        max(&mut self.y_range[1], p.y());
        min(&mut self.z_range[0], p.z());
        max(&mut self.z_range[1], p.z());
    }

    /// Returns `true` if all bounds are finite.
    pub fn is_valid(&self) -> bool {
        self.x_range[0].is_finite()
            && self.x_range[1].is_finite()
            && self.y_range[0].is_finite()
            && self.y_range[1].is_finite()
            && self.z_range[0].is_finite()
            && self.z_range[1].is_finite()
    }
}

impl<F: PrimitiveFloat> fmt::Debug for BoundingBox<F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("BoundingBox")
            .field("x", &(self.x_range[0]..self.x_range[1]))
            .field("y", &(self.y_range[0]..self.y_range[1]))
            .field("z", &(self.z_range[0]..self.z_range[1]))
            .finish()
    }
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
