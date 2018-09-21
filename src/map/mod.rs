//! ...


use std::{
    ops,
};

use crate::{
    handle::Handle,
};


pub mod aliases;
pub mod boo;
mod fn_map;
mod hash_map;
mod special_maps;
mod vec_map;

pub use self::{
    aliases::*,
    fn_map::FnMap,
    hash_map::HashMap,
    special_maps::{ConstMap, EmptyMap},
    vec_map::VecMap,
};



/// A mapping from a handle to some data (property).
///
/// This is a bare minimal trait representing all types that can map a handle
/// to optional data, called property. The returned property can be owned or
/// borrowed from `self`.
///
///
/// # Completeness
///
/// In many contexts, a `PropMap` is required to return `Some(_)` values for
/// a specific set of handles. For example:
///
/// ```ignore
/// # // TODO: use proper mesh traits here and possibly make this compile!
/// fn print_face_props<'s>(mesh: ..., map: impl PropMap<'s, FaceHandle>) {
///     ...
/// }
/// ```
///
/// This function probably requires that `map` contains `Some(_)` data for all
/// face handles of `mesh`. This is stated as: "`map` needs to be complete
/// regarding `mesh`".
///
///
/// # TODO
///
/// - Example how to use `PropMap`s
/// - Example how to implement `PropMap`
/// - Explain strange `boo` thingies
/// - Trait alias
pub trait PropMap<H: Handle> {
    type Target;
    type Marker: boo::Marker;

    /// Returns the property associated with `handle` or `None` if no such
    /// property exists.
    fn get(&self, handle: H) -> Option<boo::Wrap<'_, Self::Target, Self::Marker>>;

    /// Returns `true` if there is a property associated with `handle`, `false`
    /// otherwise.
    fn contains_handle(&self, handle: H) -> bool {
        self.get(handle).is_some()
    }
}


/// A type that stores data associated with handles.
///
/// This type is similar to `PropMap`, but has more restrictions.
/// `PropMap::get` can return owned or borrowed values, whereas
/// `PropStore::get_ref` has to return a borrowed value. It also has
/// `ops::Index` as super trait, which requires the same. Furthermore, a
/// `PropStore` needs to be able to iterate through all of its data.
///
///
/// # Type level relationship between `PropStore` and `PropMap`
///
/// `PropStore` is a subtype of `PropMap`, as in: every `PropStore` is also a
/// `PropMap`. It would be really nice to implement `PropMap` for all types
/// that implement `PropStore` (at least provide a default implementation). But
/// this is currently problematic due to (a) coherence and (b) specialization
/// being unstable. TODO: try this in the future again.
///
///
/// # TODO
///
/// - Example how to use `PropStore`s
/// - Example how to implement `PropStore`
/// - When to use `PropMap` and when to use `PropStore`
/// - Trait alias
pub trait PropStore<H: Handle>: PropMap<H> + ops::Index<H> {
    /// Returns a reference to the property associated with `handle` or `None`
    /// if no such property exists.
    fn get_ref(&self, handle: H) -> Option<&Self::Output>;

    /// Returns the number of properties stored in this map.
    fn num_props(&self) -> usize;

    /// Returns an iterator over all handles that have a value associated with
    /// them.
    ///
    /// The order of the handles is not specified.
    ///
    /// TODO: improve with GATs
    fn handles<'a>(&'a self) -> Box<dyn Iterator<Item = H> + 'a>;

    // Additional maybe useful methods:
    // - Iterator over
    //      - values
    //      - both
}

// TODO: maybe combine this with `PropStore`?
/// ...
pub trait PropStoreMut<H: Handle>: PropStore<H> + ops::IndexMut<H> {
    /// Returns a mutable reference to the property associated with `handle` or
    /// `None` if no such property exists.
    fn get_mut(&mut self, handle: H) -> Option<&mut Self::Output>;

    /// Inserts the given property associated with `handle`. If there was
    /// already a property associated with `handle`, this property is returnd.
    fn insert(&mut self, handle: H, prop: Self::Output) -> Option<Self::Output>
    where
        Self::Output: Sized;

    /// Removes the property associated with `handle` and returns it. If no
    /// property was associated with `handle`, nothing is removed and `None` is
    /// returned.
    fn remove(&mut self, handle: H) -> Option<Self::Output>
    where
        Self::Output: Sized;

    /// Returns an empty instance which doesn't contain any properties yet.
    fn empty() -> Self where Self: Sized;

    /// Removes all properties so that all `contains_handle()` returns `false`
    /// for all handles.
    fn clear(&mut self);

    /// Reserves memory for at least `additional` new properties.
    fn reserve(&mut self, additional: usize);
}
