use cgmath::{
    Point3,
    prelude::*
};

use crate::{
    prelude::*,
    cast,
    map::VecMap,
    math::PrimitiveFloat,
    prop::Pos3Like,
};

pub fn sqrt3<MeshT, MapT, ScalarT>(
    mesh: &mut MeshT,
    vertex_positions: &mut MapT,
    num_iterations: u32,
)
where
    MeshT: TriEdgeMeshMut + EdgeAdj, // TODO: doesn't need to be tri in the beginning
    MapT: PropStoreMut<VertexHandle> + Clone,
    MapT::Target: Pos3Like<Scalar = ScalarT>,
    ScalarT: PrimitiveFloat,
{
    for i in 0..num_iterations {
        sqrt3_impl(mesh, vertex_positions, i % 2 == 1);
    }
}

/// The sqrt(3) subdivision algorithm.
///
/// TODO: explain & link to paper
///
/// TODO: mention that `split_boundary = true` has the condition that all
/// boundary faces must only have one boundary edge!
///
/// https://www.graphics.rwth-aachen.de/media/papers/sqrt31.pdf
#[inline(never)]
fn sqrt3_impl<MeshT, MapT, ScalarT>(
    mesh: &mut MeshT,
    vertex_positions: &mut MapT,
    split_boundary: bool,
)
where
    MeshT: TriEdgeMeshMut + EdgeAdj, // TODO: doesn't need to be tri in the beginning
    MapT: PropStoreMut<VertexHandle> + Clone,
    MapT::Target: Pos3Like<Scalar = ScalarT>,
    ScalarT: PrimitiveFloat,
{
    // Helper macro to create literal values of type `ScalarT`
    macro_rules! lit {
        ($x:literal) => (cast::lossless::<f32, ScalarT>($x));
    }

    // Remember the original edges of the mesh.
    // TODO: replace with proper prop set
    let old_edges: VecMap<EdgeHandle, ()> = mesh.edges()
        .filter(|e| !e.is_boundary())
        .map(|e| (e.handle(), ()))
        .collect();

    // ----- (1) Calculate new positions for old vertices ----------------------------------------

    // We have to calculate a new position for all already existing vertices
    // (except boundary vertices!). To do that we need their old positions, so
    // we have no choice but making a copy. We write the new positions into
    // this copy and only write them back into `vertex_positions` at the very
    // end, since the calculation of new vertex points also relies on the old
    // positions.
    let mut new_positions = vertex_positions.clone();

    for v in mesh.vertices() {
        let vh = v.handle();
        let old_pos = vertex_positions[vh];

        let new_pos = if v.is_boundary() {
            if !split_boundary || v.is_isolated() {
                old_pos
            } else {
                // Instead of taking the centroid of all adjacent vertices,
                // only the two adjacent boundary vertices are used. We checked
                // that this vertex is boundary and not isolated, so it has at
                // least two adjacent boundary vertices. If it has more, it's a
                // multi-blade vertex which we don't allow (right now).
                let mut neighbors = v.adjacent_edges()
                    .filter(|e| e.is_boundary())
                    .map(|e| e.opposite_endpoint_of(vh));
                let neighbor_a = neighbors.next().unwrap();
                let neighbor_b = neighbors.next().unwrap();
                if neighbors.next().is_some() {
                    // TODO: add that to doc comment
                    panic!(
                        "encountered multi-blade vertex {:?} in sqrt3 (these are not allowed)",
                        vh,
                    );
                }

                let pos_a = vertex_positions[neighbor_a.handle()];
                let pos_b = vertex_positions[neighbor_b.handle()];

                // The neighbor vertices are weighted with 4/27 and the old
                // vertex with 19/27. This is from formula (9) in chapter 5.
                MapT::Target::from_coords(
                    (lit!(4.0) * (pos_a.x() + pos_b.x()) + lit!(19.0) * old_pos.x()) / lit!(27.0),
                    (lit!(4.0) * (pos_a.y() + pos_b.y()) + lit!(19.0) * old_pos.y()) / lit!(27.0),
                    (lit!(4.0) * (pos_a.z() + pos_b.z()) + lit!(19.0) * old_pos.z()) / lit!(27.0),
                )
            }
        } else {
            // Count the number of neighbors and calculate the centroid of all
            // neighbors.
            let mut valence = 0;
            let centroid = v.adjacent_vertices()
                .inspect(|_| valence += 1)
                .map(|v| vertex_positions[v.handle()])
                .centroid()
                .unwrap(); // we checked the vertex is not a boundary vertex


            // We know that there is at least one neighbor vertex, so `valence`
            // is not 0. We simply use the formula from the paper.
            let alpha = (
                lit!(4.0) - lit!(2.0) * (
                    lit!(2.0) * ScalarT::PI() / cast::rounding::<_, ScalarT>(valence)
                ).cos()
            ) / lit!(9.0);

            // Lerp between `old_pos` and `centroid` by the amount `alpha`
            MapT::Target::from_coords(
                (lit!(1.0) - alpha) * old_pos.x() + alpha * centroid.x(),
                (lit!(1.0) - alpha) * old_pos.y() + alpha * centroid.y(),
                (lit!(1.0) - alpha) * old_pos.z() + alpha * centroid.z(),
            )
        };

        // Set the new vertex position
        new_positions[vh] = new_pos;
    }


    // ----- (2) Split faces and calc new vertex positions ---------------------------------------

    // We create a new vertex per face by splitting each face into three new
    // ones. First we can reserve a bunch of memory, because we know exactly
    // how many elements we will end up with.
    vertex_positions.reserve(mesh.num_faces());
    mesh.reserve_for_vertices(mesh.num_faces());
    mesh.reserve_for_faces(mesh.num_faces() * 3);

    let mut it = mesh.face_handles_mut();
    while let Some(fh) = it.next() {
        let f = it.mesh().get_ref(fh);

        // Check if the face has a boundary edge. We know that if it has, there
        // is only one such edge (function contract).
        let boundary_edge = f.adjacent_edges().find(|e| e.is_boundary());

        if split_boundary && boundary_edge.is_some() {
            //
            //      ll         x         rr       ll         x         rr
            //       \       /   \       /         \       /| |\       /
            //        \     /     \     /           \     / | | \     /
            //         \   /   F   \   /             \   / /   \ \   /
            //          \ /         \ /               \ /  |   |  \ /
            //           l --------- r                 l - a - b - r
            //
            //           boundary_edge
            //
            let boundary_edge = boundary_edge.unwrap();
            let [l, r] = boundary_edge.endpoints();
            let rh = r.handle();

            let ll = l.adjacent_vertices()
                .find(|vn| vn.is_boundary() && vn.handle() != r.handle())
                .unwrap();
            let rr = r.adjacent_vertices()
                .find(|vn| vn.is_boundary() && vn.handle() != l.handle())
                .unwrap();

            let pos_l = vertex_positions[l.handle()].to_point3().to_vec();
            let pos_r = vertex_positions[r.handle()].to_point3().to_vec();
            let pos_ll = vertex_positions[ll.handle()].to_point3().to_vec();
            let pos_rr = vertex_positions[rr.handle()].to_point3().to_vec();

            let pos_a: MapT::Target = (
                Point3::origin() + (pos_r * lit!(10.0) + pos_l * lit!(16.0) + pos_ll) / lit!(27.0)
            ).convert();
            let pos_b: MapT::Target = (
                Point3::origin() + (pos_l * lit!(10.0) + pos_r * lit!(16.0) + pos_rr) / lit!(27.0)
            ).convert();

            let eh = boundary_edge.handle();
            let res = it.mesh().split_edge_with_faces(eh);
            let va = res.vertex;

            // Decide what edge we need to split next.
            let other_edge = res.replacement_edges
                .iter()
                .cloned()
                .find(|&new_edge| it.mesh().endpoints_of_edge(new_edge).contains(&rh))
                .unwrap();

            let res = it.mesh().split_edge_with_faces(other_edge);
            let vb = res.vertex;

            vertex_positions.insert(va, pos_a);
            vertex_positions.insert(vb, pos_b);
        } else {
            // The position of the new vertex is just the centroid of the
            // face's vertices. We can unwrap because the face always has three
            // vertices.
            let point_pos = it.mesh().get_ref(fh)
                .adjacent_vertices()
                .map(|v| vertex_positions[v.handle()])
                .centroid()
                .unwrap();

            // Split face and set position of the midpoint.
            let vh = it.mesh().split_face(fh);
            vertex_positions.insert(vh, point_pos);
        }
    }

    // ----- Flip all old edges and set relaxed vertex positions ---------------------------------
    for eh in old_edges.handles() {
        mesh.flip_edge(eh);
    }

    for vh in new_positions.handles() {
        vertex_positions[vh] = new_positions[vh];
    }
}
