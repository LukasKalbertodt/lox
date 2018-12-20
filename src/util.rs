
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
