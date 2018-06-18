use fev_core::{
    handle::{FaceHandle, EdgeHandle, VertexHandle},
};

use crate::{
    PropStore, PropStoreMut, VecMap,
};

macro_rules! create_map_trait_alias {
    ($(#[$attr:meta])* $alias_name:ident = $base_trait:ident<$handle_name:ident>) => {
        pub trait $alias_name: $base_trait<$handle_name> {}
        impl<T> $alias_name for T
        where
            T: $base_trait<$handle_name>
        {}
    }
}

create_map_trait_alias!(FacePropStore = PropStore<FaceHandle>);
create_map_trait_alias!(EdgePropStore = PropStore<EdgeHandle>);
create_map_trait_alias!(VertexPropStore = PropStore<VertexHandle>);

create_map_trait_alias!(FacePropStoreMut = PropStoreMut<FaceHandle>);
create_map_trait_alias!(EdgePropStoreMut = PropStoreMut<EdgeHandle>);
create_map_trait_alias!(VertexPropStoreMut = PropStoreMut<VertexHandle>);


/// A `VecMap` with `FaceHandle` keys.
pub type FaceVecMap<T> = VecMap<FaceHandle, T>;

/// A `VecMap` with `EdgeHandle` keys.
pub type EdgeVecMap<T> = VecMap<EdgeHandle, T>;

/// A `VecMap` with `EdgeHandle` keys.
pub type VertexVecMap<T> = VecMap<VertexHandle, T>;
