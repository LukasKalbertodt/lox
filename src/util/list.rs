// ===============================================================================================
// ===== TriList
// ===============================================================================================

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


// ===============================================================================================
// ===== DiList
// ===============================================================================================

/// A list of `T` with a maximum length of 2. This list has no well-defined
/// order, so it's rather a multi-set.
#[derive(Debug, Clone, Copy)]
// Implementation notes: if the list has exactly one element it's always
// `[Some, None]`! Code relies on this assumption.
pub struct DiList<T>([Option<T>; 2]);

impl<T> DiList<T> {
    /// Creates a `DiList` with two elements.
    pub fn full(a: T, b: T) -> Self {
        Self([Some(a), Some(b)])
    }

    /// Creates a `DiList` with a single element.
    pub fn single(elem: T) -> Self {
        Self([Some(elem), None])
    }

    /// Creates an empty `DiList`.
    pub fn empty() -> Self {
        Self([None, None])
    }

    pub fn from_options(a: Option<T>, b: Option<T>) -> Self {
        match (a, b) {
            (Some(a), Some(b)) => Self::full(a, b),
            (Some(x), None) | (None, Some(x)) => Self::single(x),
            (None, None) => Self::empty(),
        }
    }

    /// Returns the number of elements in this list (0, 1 or 2).
    pub fn len(&self) -> usize {
        match () {
            () if self.0[1].is_some() => 2,
            () if self.0[0].is_some() => 1,
            _ => 0,
        }
    }

    /// Converts all elements of this list into a `Vec<T>`. Equivalent to
    /// `self.into_iter().collect()`.
    pub fn into_vec(self) -> Vec<T> {
        self.into_iter().collect()
    }

    /// Returns an iterator over references to this list's elements. See
    /// [`into_iter`](#impl-IntoIterator) for a consuming iterator.
    pub fn iter(&self) -> DiListIter<'_, T> {

        DiListIter(&self.0[..self.len()])
    }

    pub fn contains(&self, needle: &T) -> bool
    where
        T: PartialEq,
    {
        self.0[0].as_ref() == Some(needle) || self.0[1].as_ref() == Some(needle)
    }
}

impl<T> IntoIterator for DiList<T> {
    type Item = T;
    type IntoIter = DiListIntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        DiListIntoIter(self.0)
    }
}

/// Consuming iterator for [`DiList`].
#[derive(Debug)]
pub struct DiListIntoIter<T>([Option<T>; 2]);

impl<T> Iterator for DiListIntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0[0].is_some() {
            self.0[0].take()
        } else {
            self.0[1].take()
        }
    }

    fn fold<B, F>(mut self, mut init: B, mut f: F) -> B
    where
        F: FnMut(B, Self::Item) -> B,
    {
        if let Some(elem) = self.0[0].take() {
            init = f(init, elem);
        }
        if let Some(elem) = self.0[1].take() {
            init = f(init, elem);
        }

        init
    }
}

impl<'a, T> IntoIterator for &'a DiList<T> {
    type Item = &'a T;
    type IntoIter = DiListIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// Immutable iterator for [`DiList`].
#[derive(Debug)]
// Assumes all elements in the slice are `Some`!
pub struct DiListIter<'a, T>(&'a [Option<T>]);

impl<'a, T> Iterator for DiListIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.0.split_first() {
                None => return None,
                Some((head, tail)) => {
                    self.0 = tail;
                    return head.as_ref();
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


    #[test]
    fn di_list_empty() {
        let l: DiList<String> = DiList::empty();

        assert_eq!(l.len(), 0);
        assert_eq!(l.iter().count(), 0);
        assert_eq!(l.into_iter().count(), 0);
    }

    #[test]
    fn di_list_full() {
        let l = DiList::full("anna", "bob");

        assert_eq!(l.len(), 2);
        assert_eq!(l.iter().collect::<Vec<_>>(), [&"anna", &"bob"]);
        assert_eq!(l.into_iter().collect::<Vec<_>>(), ["anna", "bob"]);
    }

    #[test]
    fn di_list_singles() {
        let l = DiList::single("a");
        assert_eq!(l.len(), 1);
        assert_eq!(l.iter().collect::<Vec<_>>(), [&"a"]);
        assert_eq!(l.into_iter().collect::<Vec<_>>(), ["a"]);
    }

    #[test]
    fn di_list_from_options() {
        let l: DiList<String> = DiList::from_options(None, None);
        assert_eq!(l.len(), 0);
        assert_eq!(l.iter().count(), 0);
        assert_eq!(l.into_iter().count(), 0);

        let l = DiList::from_options(Some("anna"), None);
        assert_eq!(l.len(), 1);
        assert_eq!(l.iter().collect::<Vec<_>>(), [&"anna"]);
        assert_eq!(l.into_iter().collect::<Vec<_>>(), ["anna"]);

        let l = DiList::from_options(None, Some("anna"));
        assert_eq!(l.len(), 1);
        assert_eq!(l.iter().collect::<Vec<_>>(), [&"anna"]);
        assert_eq!(l.into_iter().collect::<Vec<_>>(), ["anna"]);

        let l = DiList::from_options(Some("anna"), Some("bob"));
        assert_eq!(l.len(), 2);
        assert_eq!(l.iter().collect::<Vec<_>>(), [&"anna", &"bob"]);
        assert_eq!(l.into_iter().collect::<Vec<_>>(), ["anna", "bob"]);
    }
}
