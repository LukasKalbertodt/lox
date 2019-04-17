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
    MeshT: FullAdj,
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
pub fn is_closed<MeshT>(mesh: &MeshT) -> bool
where
    MeshT: FullAdj,
{
    // TODO: We can check this property in two ways:
    // - (a) each edge has two adjacent faces
    // - (b) each face has the same number of adjacent faces as number of
    //   adjacent vertices/edges
    //
    // If all edges have either 1 or 2 adjacent faces (i.e. no isolated edges
    // and no fucked-up edges), the two are equivalent, because:
    // - if (b) => each face has as many edges as vertices. On each edge of the
    //   face, there can only be one other face. Since there are as many
    //   adjacent faces as adjacent vertices/eges, each edge has two adjacent
    //   faces.
    // - if (a) => if all edges of a face have two adjacent faces, the face has
    //   as many adjacent faces as edges. Which is also the same number as the
    //   number of adjacent vertices.
    //
    // Problem: So we can perform this check for:
    // - (a): MeshT: EdgeMesh + FacesOfEdge
    // - (b): MeshT: Mesh + FacesAroundFace + VerticesAroundFace
    //      - Note: this is bad already too: if we know it's a triangle mesh,
    //        we don't need `VerticesAroundFace`
    //
    // But we can't have an "or" part in trait bounds. This is meh.


    mesh.faces().all(|f| f.adjacent_faces().count() == f.adjacent_vertices().count())
}
