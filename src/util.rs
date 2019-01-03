
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

    /// Returns an iterator over references to this list's elements. See
    /// [`into_iter`](#impl-IntoIterator) for a consuming iterator.
    pub fn iter(&self) -> TriListIter<'_, T> {
        TriListIter(&self.0)
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
