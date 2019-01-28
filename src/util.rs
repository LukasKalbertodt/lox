use cgmath::{
    Point3,
    prelude::*,
};
use num_traits::cast;

use crate::{
    math::Pos3Like,
};

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
    #[inline(always)]
    fn map<F, OutT>(self, mapping: F) -> [OutT; 3]
    where
        F: FnMut(Self::Item) -> OutT;

    fn owned_iter(self) -> TriArrayIntoIter<Self::Item>
    where
        Self::Item: Copy;
}

impl<T> TriArrayExt for [T; 3] {
    type Item = T;

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

pub trait PointIteratorExt {
    type Point;
    fn centroid(self) -> Option<Self::Point>;
}

impl<I> PointIteratorExt for I
where
    I: Iterator,
    I::Item: Pos3Like,
{
    type Point = I::Item;
    fn centroid(mut self) -> Option<Self::Point> {
        self.next().map(|first| {
            let first = first.to_point3().to_vec();
            let (count, total_displacement) = self.fold((1, first), |(count, sum), p| {
                (count + 1, sum + p.to_point3().to_vec())
            });

            Point3::from_vec(total_displacement / cast(count).unwrap()).convert()
        })
    }
}

/// A list with a dynamic length (semantically equivalent to `Vec<_>`).
///
/// Several methods of mesh traits need to return a list of handles where the
/// length is not statically known (e.g. faces around a vertex). We don't want
/// to return `Vec<_>` from all those methods, because that would always
/// require an allocation. So we have this more abstract construct. It is
/// basically an iterator with some helper functions.
///
/// This way, you can decide how you want to work with the list of handles you
/// receive. Either use this as an iterator in a `for` loop, or convert it to a
/// `Vec<_>` with `into_vec`, or use the visitor pattern by using
/// [`Iterator::for_each`].
// TODO: change `&mut self` to `self` once GATs land and we don't have to box
// this anymore
pub trait DynList: Iterator {
    /// Appends all elements of this list to the given vector. The vector is
    /// not cleared. After this function is called, the iterator is
    /// exhausted/the list is empty.
    fn append_to_vec(&mut self, v: &mut Vec<Self::Item>) {
        v.extend(self)
    }

    /// Returns a vector with all elements of this list. After this function is
    /// called, the iterator is exhausted/the list is empty.
    fn into_vec(&mut self) -> Vec<Self::Item> {
        self.collect()
    }
}

/// A list of `T` with a maximum length of 3.
///
/// Internally, this is an `[Option<T>; 3]`, so holes are explicitly stored.
/// These holes can convey additional information. The internal array can be
/// obtained via [`TriList::to_array`].
///
/// This type also implements `Iterator` to simply iterate over all values.
#[derive(Debug, Clone, Copy)]
pub struct TriList<T>([Option<T>; 3]);

impl<T> TriList<T> {
    /// Creates a new `TriList` from the given array.
    pub fn new(arr: [Option<T>; 3]) -> Self {
        Self(arr)
    }

    /// Creates an empty `TriList`. Equivalent to `TriList::new([None, None,
    /// None])`.
    pub fn empty() -> Self {
        Self([None, None, None])
    }

    /// Returns a reference to the underlying array.
    pub fn to_array(&self) -> &[Option<T>; 3] {
        &self.0
    }

    /// Converts all elements of this list into a `Vec<T>`. Equivalent to
    /// `self.into_iter().collect()`.
    pub fn into_vec(self) -> Vec<T> {
        self.into_iter().collect()
    }

    /// Returns an iterator over references to this list's elements. See
    /// [`into_iter`](#impl-IntoIterator) for a consuming iterator.
    pub fn iter(&self) -> TriListIter<'_, T> {
        TriListIter(&self.0)
    }

    pub fn contains(&self, needle: &T) -> bool
    where
        T: PartialEq,
    {
        self.0[0].as_ref() == Some(needle)
            || self.0[1].as_ref() == Some(needle)
            || self.0[2].as_ref() == Some(needle)
    }
}

impl<T> IntoIterator for TriList<T> {
    type Item = T;
    type IntoIter = TriListIntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        TriListIntoIter {
            data: self.0,
            pos: 0,
        }
    }
}

/// Consuming iterator for [`TriList`].
#[derive(Debug)]
pub struct TriListIntoIter<T>{
    data: [Option<T>; 3],
    pos: u8,
}

impl<T> Iterator for TriListIntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.pos >= 3 {
                return None;
            }

            if let Some(out) = self.data[self.pos as usize].take() {
                return Some(out);
            } else {
                self.pos += 1;
            }
        }
    }
}

impl<'a, T> IntoIterator for &'a TriList<T> {
    type Item = &'a T;
    type IntoIter = TriListIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// Immutable iterator for [`TriList`].
#[derive(Debug)]
pub struct TriListIter<'a, T>(&'a [Option<T>]);


impl<'a, T> Iterator for TriListIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.0.split_first() {
                None => return None,   // Iterator exhausted/list empty
                Some((head, tail)) => {
                    self.0 = tail;
                    if let Some(out) = &head {
                        return Some(out);
                    }
                }
            }
        }
    }
}

/// Format bytes either as slice of hexadecimal numbers or, if all data is
/// valid ASCII, as a string.
pub(crate) fn debug_fmt_bytes(data: &[u8]) -> String {
    if data.is_ascii() {
        format!("{:?}", std::str::from_utf8(data).unwrap())
    } else {
        format!("{:02x?}", data)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tri_list_empty() {
        let l: TriList<String> = TriList::empty();

        assert_eq!(l.to_array(), &[None, None, None]);
        assert_eq!(l.iter().count(), 0);
        assert_eq!(l.into_iter().count(), 0);
    }

    #[test]
    fn tri_list_full() {
        let l = TriList::new([Some("anna"), Some("bob"), Some("claire")]);

        assert_eq!(l.to_array(), &[Some("anna"), Some("bob"), Some("claire")]);
        assert_eq!(l.iter().collect::<Vec<_>>(), [&"anna", &"bob", &"claire"]);
        assert_eq!(l.into_iter().collect::<Vec<_>>(), ["anna", "bob", "claire"]);
    }

    #[test]
    fn tri_list_holes() {
        let l = TriList::new([None, Some("b"), Some("c")]);
        assert_eq!(l.iter().collect::<Vec<_>>(), [&"b", &"c"]);
        assert_eq!(l.into_iter().collect::<Vec<_>>(), ["b", "c"]);

        let l = TriList::new([Some("a"), None, Some("c")]);
        assert_eq!(l.iter().collect::<Vec<_>>(), [&"a", &"c"]);
        assert_eq!(l.into_iter().collect::<Vec<_>>(), ["a", "c"]);

        let l = TriList::new([Some("a"), Some("b"), None]);
        assert_eq!(l.iter().collect::<Vec<_>>(), [&"a", &"b"]);
        assert_eq!(l.into_iter().collect::<Vec<_>>(), ["a", "b"]);
    }

    #[test]
    fn tri_list_singles() {
        let l = TriList::new([Some("a"), None, None]);
        assert_eq!(l.iter().collect::<Vec<_>>(), [&"a"]);
        assert_eq!(l.into_iter().collect::<Vec<_>>(), ["a"]);

        let l = TriList::new([None, Some("b"), None]);
        assert_eq!(l.iter().collect::<Vec<_>>(), [&"b"]);
        assert_eq!(l.into_iter().collect::<Vec<_>>(), ["b"]);

        let l = TriList::new([None, None, Some("c")]);
        assert_eq!(l.iter().collect::<Vec<_>>(), [&"c"]);
        assert_eq!(l.into_iter().collect::<Vec<_>>(), ["c"]);

    }
}
