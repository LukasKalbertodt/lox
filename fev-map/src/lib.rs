//! ...

#![feature(crate_in_paths)]
#![feature(rust_2018_preview)]

extern crate fev_core;
extern crate stable_vec;


use std::{
    ops,
};

use fev_core::{
    handle::Handle,
};


mod foreign_impls;

pub mod aliases;
pub mod fn_map;
pub mod hash_map;
pub mod vec_map;

pub use self::aliases::*;
pub use fn_map::FnMap;
pub use hash_map::HashMap;
pub use vec_map::VecMap;



/// A mapping from a handle to some optional data (property).
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
/// // TODO: use proper mesh traits here and possibly make this compile!
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
/// - Explain parameter `'s`
/// - Trait alias
pub trait PropMap<'s, H: Handle> {
    type Target;

    /// Returns the property associated with `handle` or `None` if no such
    /// property exists.
    fn get(&'s self, handle: H) -> Option<Self::Target>;

    /// Returns `true` if there is a property associated with `handle`, `false`
    /// otherwise.
    fn contains_handle(&'s self, handle: H) -> bool {
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
/// `PropStore` is a subtype of `PropMap`, as in: every `PropStore` is also
/// a `PropMap`, at least semantically. Sadly, this fact is not reflected in
/// the type system yet. This has two reasons:
///
/// - Right now, Rust doesn't offer generic associated types (GATs). To work
///   around that limitation, `PropMap` has a pretty annoying lifetime
///   parameter. To have `PropMap` as super trait of `PropStore`, `PropStore`
///   would also have to get a lifetime parameter. So we'll wait for GATs
///   before we add `PropMap` as super trait.
/// - It would be really nice to implement `PropMap` for all types that
///   implement `PropStore` (at least provide a default implementation). But
///   this is currently not allowed due to (a) orphan rules and (b)
///   specialization being unstable.
///
/// So right now the situation is a bit sub-optimal, but it will hopefully be
/// full resolved in the future. For now, types that implement `PropStore` are
/// just expected to also implement `PropMap`.
///
///
/// # TODO
///
/// - Example how to use `PropStore`s
/// - Example how to implement `PropStore`
/// - When to use `PropMap` and when to use `PropStore`
/// - Trait alias
pub trait PropStore<H: Handle>: ops::Index<H> {
    /// Returns a reference to the property associated with `handle` or `None`
    /// if no such property exists.
    fn get_ref(&self, handle: H) -> Option<&Self::Output>;

    /// Returns `true` if there is a property associated with `handle`, `false`
    /// otherwise.
    fn contains_handle(&self, handle: H) -> bool {
        self.get_ref(handle).is_some()
    }

    // Additional maybe useful methods:
    // - numValues
    // - Iterator over
    //      - handles
    //      - values
    //      - both

    // // Combinators
    // fn map<F, NewOutT>(&'s self, mapping: F) -> MappedPropMap<'s, F, Self>
    // where
    //     Self: Sized,
    //     F: Fn(Self::Out) -> NewOutT,
    // {
    //     MappedPropMap {
    //         original: self,
    //         mapping,
    //     }
    // }
}

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
}



// pub struct MappedPropMap<'a, F, MapT: 'a> {
//     original: &'a MapT,
//     mapping: F,
// }

// impl<'a, F, H, MapT, NewOutT> ops::Index<H> for MappedPropMap<'a, F, MapT>
// where
//     H: 'a + Handle,
//     MapT: 'a + PropMap<H>,
//     F: Fn(&'a Map::Output) -> &'a NewOutT,
//     NewOutT: 'a,
// {
//     type Output = NewOutT;

//     fn index(&self, idx: H) -> &Self::Output {
//         self.get(idx).unwrap() // TODO: expect
//     }
// }

// impl<'s, F, H, MapT, NewOutT> PropMap<'s, H> for MappedPropMap<'s, F, MapT>
// where
//     H: Handle,
//     MapT: PropMap<'s, H>,
//     F: Fn(MapT::Out) -> NewOutT,
// {
//     type Out = NewOutT;
//     fn get(&self, handle: H) -> Option<Self::Out> {
//         self.original.get(handle).map(&self.mapping)
//     }
// }
