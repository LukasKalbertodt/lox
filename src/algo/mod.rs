use crate::{
    prelude::*,
    map::{VecMap, VertexPropMap},
    prop::Pos3Like,
    refs::VertexRef,
};

pub mod subdivision;


#[inline(never)]
pub fn smooth_simple<MeshT, MapT>(
    mesh: &MeshT,
    vertex_positions: &MapT,
) -> VecMap<VertexHandle, MapT::Target>
where
    MeshT: Mesh + VerticesAroundVertex,
    MapT: VertexPropMap,
    MapT::Target: Pos3Like,
{
    // Helper function to get the position of a vertex.
    let pos_of = |v: VertexRef<'_, MeshT>| {
        *vertex_positions.get(v.handle()).expect("missing vertex position")
    };

    mesh.vertices().map(|v| {
        // We use the centroid of all neighbors' position as new positon. If
        // the vertex does not have any neighbor vertices, its position stays
        // the same.
        let new_pos = v.ring1_neighbors()
            .map(|n| pos_of(n))
            .centroid()
            .unwrap_or(pos_of(v));

        (v.handle(), new_pos.convert())
    }).collect()
}
