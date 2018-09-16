//! Trait aliases for property maps and stores with the three important
//! elements as handle (edge, face, vertex).

use crate::{
    handle::{FaceHandle, EdgeHandle, VertexHandle},
};
use super::{
    PropMap, PropStore, PropStoreMut,
};

macro_rules! create_map_trait_alias {
    ($(#[$attr:meta])* $alias_name:ident = $base_trait:ident<$handle_name:ident>) => {
        $(#[$attr])*
        pub trait $alias_name: $base_trait<$handle_name> {}
        impl<T> $alias_name for T
        where
            T: $base_trait<$handle_name>
        {}
    }
}

create_map_trait_alias!(
    /// A mapping from a face handle to some data.
    FacePropMap = PropMap<FaceHandle>
);
create_map_trait_alias!(
    /// A mapping from an edge handle to some data.
    EdgePropMap = PropMap<EdgeHandle>
);
create_map_trait_alias!(
    /// A mapping from a vertex handle to some data.
    VertexPropMap = PropMap<VertexHandle>
);

create_map_trait_alias!(FacePropStore = PropStore<FaceHandle>);
create_map_trait_alias!(EdgePropStore = PropStore<EdgeHandle>);
create_map_trait_alias!(VertexPropStore = PropStore<VertexHandle>);

create_map_trait_alias!(FacePropStoreMut = PropStoreMut<FaceHandle>);
create_map_trait_alias!(EdgePropStoreMut = PropStoreMut<EdgeHandle>);
create_map_trait_alias!(VertexPropStoreMut = PropStoreMut<VertexHandle>);
