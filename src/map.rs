use std::{
    ops::Index,
};

use crate::{
    handle::{DefaultIndex, Handle, HandleIndex, FaceHandle, EdgeHandle, VertexHandle},
};


mod vec_map;

pub use self::vec_map::VecMap;

/// A `VecMap` with `FaceHandle` keys.
pub type FaceVecMap<T, Idx = DefaultIndex> = VecMap<FaceHandle<Idx>, T>;

/// A `VecMap` with `EdgeHandle` keys.
pub type EdgeVecMap<T, Idx = DefaultIndex> = VecMap<EdgeHandle<Idx>, T>;

/// A `VecMap` with `EdgeHandle` keys.
pub type VertexVecMap<T, Idx = DefaultIndex> = VecMap<VertexHandle<Idx>, T>;



pub trait AttrMap<H: Handle>: Index<H> {}


macro_rules! create_map_trait_alias {
    ($alias_name:ident, $handle_name:ident) => {
        pub trait $alias_name<Idx: HandleIndex>: AttrMap<$handle_name<Idx>> {}
        impl<T, Idx: HandleIndex> $alias_name<Idx> for T
        where
            T: AttrMap<$handle_name<Idx>>
        {}
    }
}

create_map_trait_alias!(FaceMap, FaceHandle);
create_map_trait_alias!(EdgeMap, EdgeHandle);
create_map_trait_alias!(VertexMap, VertexHandle);
