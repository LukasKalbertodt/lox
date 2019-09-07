use std::{
    collections::HashMap,
};

use cgmath::{
    Point3,
    prelude::*
};

use crate::{
    prelude::*,
    cast,
    handle::hsize,
    map::{DenseMap, SparseMap},
    math::PrimitiveFloat,
    prop::Pos3Like,
};

pub fn sqrt3<MeshT, MapT, ScalarT>(
    mesh: &mut MeshT,
    vertex_positions: &mut MapT,
    num_iterations: u32,
)
where
    MeshT: TriMesh + EdgeMesh + MeshMut + EdgeAdj, // TODO: doesn't need to be tri in the beginning
    MapT: PropStoreMut<VertexHandle>,
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
    MeshT: TriMesh + EdgeMesh + MeshMut + EdgeAdj, // TODO: doesn't need to be tri in the beginning
    MapT: PropStoreMut<VertexHandle>,
    MapT::Target: Pos3Like<Scalar = ScalarT>,
    ScalarT: PrimitiveFloat,
{
    // Helper macro to create literal values of type `ScalarT`
    macro_rules! lit {
        ($x:literal) => (cast::lossless::<f32, ScalarT>($x));
    }


    /// A simple memoization helper to avoid calculating `alpha` all the time
    /// as it's only dependent on the valence of a vertex. The number of
    /// different valences is usually pretty low.
    struct AlphaCache<ScalarT: PrimitiveFloat> {
        low: [ScalarT; 10],
        // TODO: use a faster hash function
        rest: HashMap<hsize, ScalarT>,
    }

    impl<ScalarT: PrimitiveFloat> AlphaCache<ScalarT> {
        fn alpha_for(valence: hsize) -> ScalarT {
            (
                lit!(4.0) - lit!(2.0) * (
                    lit!(2.0) * ScalarT::PI() / cast::rounding::<_, ScalarT>(valence)
                ).cos()
            ) / lit!(9.0)
        }

        fn new() -> Self {
            Self {
                low: [
                    lit!(0.0),
                    Self::alpha_for(1),
                    Self::alpha_for(2),
                    Self::alpha_for(3),
                    Self::alpha_for(4),
                    Self::alpha_for(5),
                    Self::alpha_for(6),
                    Self::alpha_for(7),
                    Self::alpha_for(8),
                    Self::alpha_for(9),
                ],
                rest: HashMap::new(),
            }
        }

        fn get(&mut self, valence: hsize) -> ScalarT {
            if valence < 10 {
                self.low[valence as usize]
            } else {
                self.get_from_hashmap(valence)
            }
        }

        #[cold]
        #[inline(never)]
        fn get_from_hashmap(&mut self, valence: hsize) -> ScalarT {
            *self.rest.entry(valence).or_insert_with(|| Self::alpha_for(valence))
        }
    }





    // ----- (1) Calculate positions for new boundary vertices -----------------------------------

    // Remember the original edges of the mesh and calculate boundary vertex
    // positions if we will split the boundaries. We have to do this now
    // because later we will already change the topology and can't properly
    // calculate the positions anymore.
    //
    //TODO: replace with proper prop set
    let mut old_edges = DenseMap::with_capacity(mesh.num_edges());
    let mut new_boundary_points = SparseMap::new();
    for e in mesh.edges() {
        if e.is_boundary() {
            if split_boundary {
                // Here we prepare the positions for the two new vertices that
                // will be created by splitting the boundary face later on.
                //
                //      ll         x         rr       ll         x         rr
                //       \       /   \       /         \       /| |\       /
                //        \     /     \     /           \     / | | \     /
                //         \   /   F   \   /             \   / /   \ \   /
                //          \ /         \ /               \ /  |   |  \ /
                //           l --------- r                 l - a - b - r
                //                 e
                //
                let [l, r] = e.endpoints();

                let ll = l.adjacent_edges()
                    .find(|en| en.is_boundary() && en.handle() != e.handle())
                    .map(|en| en.opposite_endpoint_of(l.handle()))
                    .unwrap();
                let rr = r.adjacent_edges()
                    .find(|en| en.is_boundary() && en.handle() != e.handle())
                    .map(|en| en.opposite_endpoint_of(r.handle()))
                    .unwrap();

                let pos_l = vertex_positions[l.handle()].to_point3().to_vec();
                let pos_r = vertex_positions[r.handle()].to_point3().to_vec();
                let pos_ll = vertex_positions[ll.handle()].to_point3().to_vec();
                let pos_rr = vertex_positions[rr.handle()].to_point3().to_vec();

                let pos_a = Point3::origin()
                    + (pos_r * lit!(10.0) + pos_l * lit!(16.0) + pos_ll) / lit!(27.0);
                let pos_b = Point3::origin()
                    + (pos_l * lit!(10.0) + pos_r * lit!(16.0) + pos_rr) / lit!(27.0);

                new_boundary_points.insert(e.handle(), [pos_a, pos_b]);
            }
        } else {
            old_edges.insert(e.handle(), ());
        }
    }

    // ----- (2) Calculate new positions for old vertices ----------------------------------------

    // We have to calculate a new position for all already existing vertices
    // (except boundary vertices!). To do that we need their old positions, so
    // we have no choice but making a copy. We write the new positions into
    // this copy and only write them back into `vertex_positions` at the very
    // end, since the calculation of new vertex points also relies on the old
    // positions.

    let mut alphas = AlphaCache::new();

    let new_positions = mesh.vertices().map(|v| {
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

            let alpha = alphas.get(valence);

            // Lerp between `old_pos` and `centroid` by the amount `alpha`
            MapT::Target::from_coords(
                (lit!(1.0) - alpha) * old_pos.x() + alpha * centroid.x(),
                (lit!(1.0) - alpha) * old_pos.y() + alpha * centroid.y(),
                (lit!(1.0) - alpha) * old_pos.z() + alpha * centroid.z(),
            )
        };

        (vh, new_pos)
    }).collect::<DenseMap<_, _>>();


    // ----- (3) Split faces and calc new vertex positions ---------------------------------------

    // We create a new vertex per face by splitting each face into three new
    // ones. First we can reserve a bunch of memory, because we know exactly
    // how many elements we will end up with.
    vertex_positions.reserve(mesh.num_faces());
    mesh.reserve_for_vertices(mesh.num_faces());
    mesh.reserve_for_faces(mesh.num_faces() * 3);

    let mut it = mesh.face_handles_mut();
    while let Some(fh) = it.next() {
        let f = it.mesh().get_ref(fh);

        if split_boundary {
            // Check if the face has a boundary edge. We know that if it has,
            // there is only one such edge (function contract).
            let boundary_edge = f.adjacent_edges().find(|e| e.is_boundary());
            if let Some(boundary_edge) = boundary_edge {
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
                let [_, r] = boundary_edge.endpoints();
                let rh = r.handle();
                let eh = boundary_edge.handle();

                // Retrieve the new points we calculated before
                let [pos_a, pos_b] = new_boundary_points[eh];

                // Split the boundary edge/face once
                let res = it.mesh().split_edge_with_faces(eh);
                let va = res.vertex;

                // Decide what edge we need to split next.
                let other_edge = res.replacement_edges
                    .iter()
                    .copied()
                    .find(|&new_edge| it.mesh().endpoints_of_edge(new_edge).contains(&rh))
                    .unwrap();

                // Split another edge
                let res = it.mesh().split_edge_with_faces(other_edge);
                let vb = res.vertex;

                vertex_positions.insert(va, pos_a.convert());
                vertex_positions.insert(vb, pos_b.convert());

                continue;
            }
        }


        // The position of the new vertex is just the centroid of the
        // face's vertices. We can unwrap because the face always has three
        // vertices.
        let point_pos = f
            .adjacent_vertices()
            .map(|v| vertex_positions[v.handle()])
            .centroid()
            .unwrap();

        // Split face and set position of the midpoint.
        let vh = it.mesh().split_face(fh);
        vertex_positions.insert(vh, point_pos);
    }

    // ----- (4) Flip all old edges and set relaxed vertex positions -----------------------------
    for eh in old_edges.handles() {
        mesh.flip_edge(eh);
    }

    for vh in new_positions.handles() {
        vertex_positions[vh] = new_positions[vh];
    }
}
