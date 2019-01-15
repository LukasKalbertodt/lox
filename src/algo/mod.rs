use cgmath::{
    prelude::*,
    Point3,
};
use num_traits::ToPrimitive;

use crate::{
    prelude::*,
    map::{VecMap, VertexPropMap},
    math::Pos3Like,
    refs::VertexRef,
};


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


/// The sqrt(3) subdivision algorithm.
///
/// TODO: explain & link to paper
#[inline(never)]
pub fn sqrt3_subdivision<MeshT, MapT>(
    mesh: &mut MeshT,
    vertex_positions: &mut MapT,
)
where
    MeshT: TriMeshMut
        + TriVerticesOfFace
        + FacesAroundVertex
        + TriFacesAroundFace
        + VerticesAroundVertex,
    MapT: PropStoreMut<VertexHandle>,
    MapT::Target: Pos3Like,
{
    struct WeightHelper {
        cache: Vec<(f64, f64)>,
    }

    impl WeightHelper {
        fn new() -> Self {
            Self {
                cache: (1..10).map(|v| Self::calc_values(v)).collect(),
            }
        }

        fn get(&mut self, valence: u32) -> (f64, f64) {
            // We don't want to blow our cache. This is a simple fall back for
            // super strange meshes.
            if valence > 1000 {
                return Self::calc_values(valence);
            }

            // Fill cache with remaining values, if the requested value is not in the cache yet
            for v in self.cache.len() + 1..valence as usize + 1 {
                self.cache.push(Self::calc_values(v as u32))
            }

            self.cache[valence as usize - 1]
        }

        fn calc_values(valence: u32) -> (f64, f64) {
            let alpha = (4.0 - 2.0 * (2.0 * std::f64::consts::PI / valence as f64).cos()) / 9.0;
            (1.0 - alpha, alpha / (valence as f64))
        }
    }

    // Helper function to get the position of a vertex.
    let pos_of = |vertex_positions: &mut MapT, v: VertexRef<'_, MeshT>| {
        *vertex_positions.get(v.handle()).expect("missing vertex position")
    };

    let mut weights = WeightHelper::new();

    // We create a new vertex per face, so we will create a map from face
    // handle to vertex handle. This is done in two steps since we run in
    // borrowing problems.
    //
    // TODO: collecting face handles first should not be necessary
    let face_handles = mesh.faces().map(|f| f.handle()).collect::<Vec<_>>();
    let old_vertices = mesh.vertices().map(|v| v.handle()).collect::<Vec<_>>();

    let relaxed_positions = old_vertices.iter().map(|&vh| {
        let v = mesh.get_ref(vh);
        let old_pos = pos_of(vertex_positions, v);

        // Count the number of neighbors and calculate the centroid of all
        // neighbors.
        let mut valence = 0;
        let vec_sum = v.ring1_neighbors()
            .inspect(|_| valence += 1)
            .map(|v| pos_of(vertex_positions, v).to_point3().to_vec())
            .sum::<cgmath::Vector3<_>>();
            // .centroid();

        let relaxed_pos = if valence == 0 {
            old_pos
        } else {
            let (weight_old, weight_new) = weights.get(valence);
            let old_pos = old_pos.to_point3().to_vec().map(|s| s.to_f64().unwrap());
            let vec_sum = vec_sum.map(|s| s.to_f64().unwrap());

            (Point3::origin() + (weight_old * old_pos + weight_new * vec_sum))
                .cast()
                .unwrap()
                .convert()
        };

        // // Se the new vertex position
        // let relaxed_pos = maybe_centroid.map(|centroid| {
        //     // We know that there is at least one neighbor vertex, so `valence`
        //     // is not 0. We simply use the formula from the paper.
        //     let alpha = (4.0 - 2.0 * (2.0 * std::f64::consts::PI / valence as f64).cos()) / 9.0;
        //     let old_pos = old_pos.to_point3().to_vec().map(|s| s.to_f64().unwrap());
        //     let centroid = centroid.to_point3().to_vec().map(|s| s.to_f64().unwrap());

        //     (Point3::origin() + old_pos.lerp(centroid, alpha))
        //         .cast()
        //         .unwrap()
        //         .convert()
        // }).unwrap_or(old_pos);

        (vh, relaxed_pos)
    }).collect::<VecMap<_, _>>();

    let new_vertices = face_handles.into_iter().map(|fh| {
        // The position of the new vertex is just the centroid of the face's
        // vertices. We can unwrap because the face always has three vertices.
        let point_pos = mesh.get_ref(fh)
            .adjacent_vertices()
            .map(|v| pos_of(vertex_positions, v))
            .centroid()
            .unwrap();

        // Add a vertex with the calculate position
        let vh = mesh.add_vertex();
        vertex_positions.insert(vh, point_pos);

        // We want build a face -> vertex map
        (fh, vh)
    }).collect::<VecMap<_, _>>();

    for vh in relaxed_positions.handles() {
        vertex_positions[vh] = relaxed_positions[vh];
    }


    // Create a list of new faces we want to add
    let mut new_faces = Vec::new();
    let mut faces_cache = Vec::new();
    for v in mesh.vertices() {
        faces_cache.clear();
        faces_cache.extend(v.adjacent_faces());

        for i in 0..faces_cache.len() {
            let curr = faces_cache[i];
            let next = faces_cache[(i + 1) % faces_cache.len()];
            if curr.is_adjacent_to_face(next.handle()) {
                new_faces.push([
                    new_vertices[next.handle()],
                    new_vertices[curr.handle()],
                    v.handle(),
                ]);
            }
        }
    }

    // All old faces will be replaced
    mesh.remove_all_faces();

    // Now add all the faces to the mesh
    for new_face in new_faces {
        mesh.add_face(new_face);
    }

    // for vh in old_vertices {
    //     let v = mesh.get_ref(vh);
    //     let old_pos = pos_of(vertex_positions, v);

    //     // Count the number of neighbors and calculate the centroid of all
    //     // neighbors.
    //     let mut valence = 0;
    //     let maybe_centroid = v.ring1_neighbors()
    //         .inspect(|_| valence += 1)
    //         .map(|v| pos_of(vertex_positions, v))
    //         .centroid();

    //     // Se the new vertex position
    //     vertex_positions[vh] = maybe_centroid.map(|centroid| {
    //         // We know that there is at least one neighbor vertex, so `valence`
    //         // is not 0. We simply use the formula from the paper.
    //         let alpha = (4.0 - 2.0 * (2.0 * std::f64::consts::PI / valence as f64).cos()) / 9.0;
    //         let old_pos = old_pos.to_point3().to_vec().map(|s| s.to_f64().unwrap());
    //         let centroid = centroid.to_point3().to_vec().map(|s| s.to_f64().unwrap());

    //         (Point3::origin() + old_pos.lerp(centroid, alpha))
    //             .cast()
    //             .unwrap()
    //             .convert()
    //     }).unwrap_or(old_pos);
    // }
}
