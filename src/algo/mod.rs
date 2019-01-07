use crate::{
    prelude::*,
    map::{VecMap, VertexPropMap},
    math::Pos3Like,
    refs::VertexRef,
};


pub fn smooth_simple<MeshT, MapT>(
    mesh: &MeshT,
    vertex_positions: &MapT,
) -> VecMap<VertexHandle, MapT::Target>
where
    MeshT: Mesh + VerticesAroundVertex,
    MapT: VertexPropMap,
    MapT::Target: Pos3Like,
{
    // TODO: things to improve
    // - Implement `FromIterator` for `VecMap` and then `collect`
    let mut out = VecMap::with_capacity(mesh.num_vertices());
    let pos_of = |v: VertexRef<'_, MeshT>| {
        *vertex_positions.get(v.handle()).expect("missing vertex position")
    };

    for v in mesh.vertices() {
        let new_pos = v.ring1_neighbors()
            .map(|n| pos_of(n))
            .centroid()
            .unwrap_or(pos_of(v));

        out.insert(v.handle(), new_pos.convert());
    }

    out
}
