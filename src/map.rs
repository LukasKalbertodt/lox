use std::{
    ops
};

use crate::{
    handle::{Handle, FaceHandle, EdgeHandle, VertexHandle},
};


mod vec_map;

pub use self::vec_map::VecMap;

/// A `VecMap` with `FaceHandle` keys.
pub type FaceVecMap<T> = VecMap<FaceHandle, T>;

/// A `VecMap` with `EdgeHandle` keys.
pub type EdgeVecMap<T> = VecMap<EdgeHandle, T>;

/// A `VecMap` with `EdgeHandle` keys.
pub type VertexVecMap<T> = VecMap<VertexHandle, T>;




pub trait PropMap<H: Handle>: ops::Index<H> {
    /// Returns a reference to property's value associated with the given
    /// handle, or `None` if no value is associated with that handle.
    fn get(&self, handle: H) -> Option<&Self::Output>;

    /// Returns `true` if and only if this map contains a property associated
    /// with the given handle.
    fn contains_handle(&self, handle: H) -> bool {
        self.get(handle).is_some()
    }

    // Additional maybe useful methods:
    // - numValues
    // - Iterator over
    //      - handles
    //      - values
    //      - both

    // Combinators
    fn map<'a, F, NewOutput>(&'a self, mapping: F) -> PropMapping<'a, F, Self>
    where
        Self: Sized,
        Self::Output: 'a,
        F: Fn(&'a Self::Output) -> &'a NewOutput,
        NewOutput: 'a,
    {
        PropMapping {
            original: self,
            mapping,
        }
    }
}

pub trait PropMapMut<H: Handle>: PropMap<H> + ops::IndexMut<H> {
    /// Returns a mutable reference to property's value associated with the
    /// given handle, or `None` if no value is associated with that handle.
    fn get_mut(&mut self, handle: H) -> Option<&mut Self::Output>;

    fn insert(&mut self, h: H, elem: Self::Output) -> Option<Self::Output>
        where Self::Output: Sized;

    // Additional maybe useful methods:
    // - clear
    // - insert
    // - erase
}


macro_rules! create_map_trait_alias {
    ($alias_name:ident, $handle_name:ident, $base_trait:ident) => {
        pub trait $alias_name: $base_trait<$handle_name> {}
        impl<T> $alias_name for T
        where
            T: $base_trait<$handle_name>
        {}
    }
}

create_map_trait_alias!(FaceMap, FaceHandle, PropMap);
create_map_trait_alias!(EdgeMap, EdgeHandle, PropMap);
create_map_trait_alias!(VertexMap, VertexHandle, PropMap);

create_map_trait_alias!(FaceMapMut, FaceHandle, PropMapMut);
create_map_trait_alias!(EdgeMapMut, EdgeHandle, PropMapMut);
create_map_trait_alias!(VertexMapMut, VertexHandle, PropMapMut);

pub struct PropMapping<'a, F, Map: 'a> {
    original: &'a Map,
    mapping: F,
}

impl<'a, F, H, Map, NewOutput> ops::Index<H> for PropMapping<'a, F, Map>
where
    H: 'a + Handle,
    Map: 'a + PropMap<H>,
    F: Fn(&'a Map::Output) -> &'a NewOutput,
    NewOutput: 'a,
{
    type Output = NewOutput;

    fn index(&self, idx: H) -> &Self::Output {
        self.get(idx).unwrap() // TODO: expect
    }
}

impl<'a, F, H, Map, NewOutput> PropMap<H> for PropMapping<'a, F, Map>
where
    H: 'a + Handle,
    Map: 'a + PropMap<H>,
    F: Fn(&'a Map::Output) -> &'a NewOutput,
    NewOutput: 'a,
{
    fn get(&self, handle: H) -> Option<&Self::Output> {
        self.original.get(handle).map(|p| (self.mapping)(p))
    }
}
