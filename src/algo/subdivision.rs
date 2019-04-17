use crate::{
    prelude::*,
    cast,
    map::VecMap,
    math::PrimitiveFloat,
    prop::Pos3Like,
};


/// The sqrt(3) subdivision algorithm.
///
/// TODO: explain & link to paper
#[inline(never)]
pub fn sqrt3<MeshT, MapT, ScalarT>(
    mesh: &mut MeshT,
    vertex_positions: &mut MapT,
)
where
    MeshT: TriEdgeMeshMut + EdgeAdj, // TODO: doesn't need to be tri in the beginning
    MapT: PropStoreMut<VertexHandle> + Clone,
    MapT::Target: Pos3Like<Scalar = ScalarT>,
    ScalarT: PrimitiveFloat,
{
    // Remember the original edges of the mesh.
    // TODO: replace with proper prop set
    let old_edges: VecMap<EdgeHandle, ()> = mesh.edges()
        .filter(|e| !e.is_boundary())
        .map(|e| (e.handle(), ()))
        .collect();

    // ----- (1) Calculate new positions for old vertices ----------------------------------------
    // We have to calculate a new position for all already existing vertices.
    // To do that we need their old positions, so we have no choice but making
    // a copy. We write the new positions into this copy and only write them
    // back into `vertex_positions` at the very end, since the calculation of
    // new vertex points also relies on the old positions.
    let mut new_positions = vertex_positions.clone();

    for vh in mesh.vertex_handles() {
        let v = mesh.get_ref(vh);
        let old_pos = vertex_positions[vh];

        // Count the number of neighbors and calculate the centroid of all
        // neighbors.
        let mut valence = 0;
        let maybe_centroid = v.ring1_neighbors()
            .inspect(|_| valence += 1)
            .map(|v| vertex_positions[v.handle()])
            .centroid();

        // Set the new vertex position
        new_positions[vh] = maybe_centroid.map(|centroid| {
            // Helper macro to create literal values of type `ScalarT`
            macro_rules! lit {
                ($x:literal) => (cast::lossless::<f32, ScalarT>($x));
            }

            // We know that there is at least one neighbor vertex, so `valence`
            // is not 0. We simply use the formula from the paper.
            let alpha = (
                lit!(4.0) - lit!(2.0) * (
                    lit!(2.0) * ScalarT::PI() / cast::rounding::<_, ScalarT>(valence)
                ).cos()
            ) / lit!(9.0);

            // Lerp between `old_pos? and `centroid` by the amount `alpha`
            MapT::Target::from_coords(
                (lit!(1.0) - alpha) * old_pos.x() + alpha * centroid.x(),
                (lit!(1.0) - alpha) * old_pos.y() + alpha * centroid.y(),
                (lit!(1.0) - alpha) * old_pos.z() + alpha * centroid.z(),
            )
        }).unwrap_or(old_pos);
    }


    // ----- (2) Split faces and calc new vertex positions
    //       --------------------------------------- We create a new vertex per
    //       face by splitting each face into three new ones. First we can
    //       reserve a bunch of memory, because we know exactly how many
    //       elements we will end up with.
    vertex_positions.reserve(mesh.num_faces());
    mesh.reserve_for_vertices(mesh.num_faces());
    mesh.reserve_for_faces(mesh.num_faces() * 3);

    let mut it = mesh.face_handles_mut();
    while let Some(fh) = it.next() {
        // The position of the new vertex is just the centroid of the face's
        // vertices. We can unwrap because the face always has three vertices.
        let point_pos = it.mesh().get_ref(fh)
            .adjacent_vertices()
            .map(|v| vertex_positions[v.handle()])
            .centroid()
            .unwrap();

        // Split face and set position of the midpoint.
        let vh = it.mesh().split_face(fh);
        vertex_positions.insert(vh, point_pos);
    }

    // ----- Flip all old edges and set relaxed vertex positions ---------------------------------
    for eh in old_edges.handles() {
        mesh.flip_edge(eh);
    }

    for vh in new_positions.handles() {
        vertex_positions[vh] = new_positions[vh];
    }
}
