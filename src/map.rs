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



pub trait AttrMap where Self: Index<<Self as AttrMap>::Handle> {
    // TODO: In the current design, each attribute map is fixed to one handle
    // type. Meaning: a map with `Handle = FaceHandle<u32>` cannot be indexed
    // with `FaceHandle<u16>` nor by `FaceHandle<u64>`. This restriction is
    // not necessary (I think!). However, maps should still be restricted to
    // one handle *kind*, i.e. FaceHandle. I think the only sane way to do that
    // is by using GATs which are still not yet implemented.
    type Handle: Handle;
}


macro_rules! create_map_trait_alias {
    ($alias_name:ident, $handle_name:ident) => {
        pub trait $alias_name<Idx: HandleIndex>: AttrMap<Handle = $handle_name<Idx>> {}
        impl<T, Idx: HandleIndex> $alias_name<Idx> for T
        where
            T: AttrMap<Handle = $handle_name<Idx>>
        {}
    }
}

create_map_trait_alias!(FaceMap, FaceHandle);
create_map_trait_alias!(EdgeMap, EdgeHandle);
create_map_trait_alias!(VertexMap, VertexHandle);
