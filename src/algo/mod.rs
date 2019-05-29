use cgmath::prelude::*;

use crate::{
    prelude::*,
    map::{VecMap, VertexPropMap},
    math::PrimitiveFloat,
    prop::Pos3Like,
    refs::VertexRef,
};

pub mod bounding;
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
        // If the vertex is a boundary vertex, its position doesn't change. If
        // not, we use the centroid of all neighbors' position as new positon.
        let new_pos = if v.is_boundary() {
            pos_of(v)
        } else {
            v.adjacent_vertices()
                .map(|n| pos_of(n))
                .centroid()
                .unwrap()  // is not boundary
                .convert()
        };

        (v.handle(), new_pos)
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


/// Data that the Dijkstra algorithm returns per vertex.
#[derive(Debug, Clone, Copy)]
pub struct DijsktraVertexData<F> {
    /// Distance of the shortest path from start vertex. This is infinity if
    /// there is no path from the start vertex.
    pub distance: F,

    /// The previous vertex in the path from the start vertex. If this vertex
    /// is not reachable from the start vertex, this is the handle of the
    /// vertex itself (and `distance` is infinity).
    pub prev: VertexHandle,
}


/// TODO
///
/// - think about having a parameter `target vertex` that allows the algo to
///   break early when it's found
/// - Provide distance as edge map -> but then we need EdgeAdj
pub fn dijkstra<MeshT, MapT, ScalarT>(
    mesh: &MeshT,
    vertex_positions: &MapT,
    start_vertex: VertexHandle,
) -> VecMap<VertexHandle, DijsktraVertexData<ScalarT>>
where
    MeshT: FullAdj,
    MapT: VertexPropMap,
    MapT::Target: Pos3Like<Scalar = ScalarT>,
    ScalarT: PrimitiveFloat,
{
    use std::{
        cmp::Ordering,
        collections::BinaryHeap,
    };

    /// Stuff we store in the heap
    struct HeapElem<ScalarT> {
        /// The currently best distance to this vertex.
        distance: ScalarT,

        /// Handle of the vertex
        handle: VertexHandle,
    }

    // Implementing ordering traits
    impl<ScalarT: PrimitiveFloat> Ord for HeapElem<ScalarT> {
        fn cmp(&self, other: &Self) -> Ordering {
            self.partial_cmp(other).expect("NaN distance in Dijkstra")
        }
    }
    impl<ScalarT: PrimitiveFloat> PartialOrd for HeapElem<ScalarT> {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            // We reverse the order because the std binary heap is a max heap
            self.distance.partial_cmp(&other.distance)
                .map(|ord| ord.reverse())
        }
    }
    impl<ScalarT: PrimitiveFloat> Eq for HeapElem<ScalarT> {}
    impl<ScalarT: PrimitiveFloat> PartialEq for HeapElem<ScalarT> {
        fn eq(&self, other: &Self) -> bool {
            self.distance == other.distance
        }
    }


    // Create the main data structures and preallocate. For the heap, since we
    // don't use `decrease_key` but instead insert elements multiple times, we
    // expect that more than `num_vertices()` elements are stored in the heap.
    // A few experiments showed that for most "normal" meshes, the peak element
    // count in the heap is somewhere around 1.3 times the number of vertices.
    // Allocating 1.5 times as much shouldn't be wasting a lot of space and we
    // are still on the save side.
    let mut vertex_data = VecMap::with_capacity(mesh.num_vertices());
    let mut visited = VecMap::with_capacity(mesh.num_vertices()); // TODO: real set
    let mut heap = BinaryHeap::with_capacity((mesh.num_vertices() as f64 * 1.5) as usize);

    // Initialization: set all distances to infinity and the `prev` field to
    // the vertex itself. For the start vertex, set the distance to 0. Add all
    // vertices into the heap.
    for vh in mesh.vertex_handles() {
        let distance = if vh == start_vertex {
            ScalarT::zero()
        } else {
            ScalarT::infinity()
        };

        vertex_data.insert(vh, DijsktraVertexData { distance, prev: vh });
        heap.push(HeapElem { distance, handle: vh });
    }

    // The actual search: pop the element with the smallest distance from the
    // heap, visit all its neighbors and update their distances.
    while let Some(current) = heap.pop() {
        // Since we insert elements into the heap multiple times, we have to
        // check if we already popped it from the heap and skip it in that
        // case.
        if visited.contains_handle(current.handle) {
            continue;
        }

        // Mark vertex as visited (its distance is now finalized)
        visited.insert(current.handle, ());

        // Visit all neighbors
        for nh in mesh.vertices_around_vertex(current.handle) {
            // We can skip neighbors we already visited: their distance is
            // already finalized and won't be improved.
            if visited.contains_handle(nh) {
                continue;
            }

            let pos_of = |vh: VertexHandle| {
                vertex_positions.get(vh)
                    .unwrap_or_else(|| panic!("vertex position for {:?} missing in Dijkstra", vh))
                    .to_point3()
            };

            let distance_to_neighbor = pos_of(current.handle).distance(pos_of(nh));
            let new_distance = current.distance + distance_to_neighbor;

            if new_distance < vertex_data[nh].distance {
                vertex_data[nh].distance = new_distance;
                vertex_data[nh].prev = current.handle;

                // Add vertex to heap again, but with a smaller distance. In
                // the classical algorithm, there would be a
                // `heap.decrease_key` call here. However, supporting this
                // method makes the heap more complex. It has been found that
                // for many graphs, in particular all sparse graphs, adding
                // nodes multiple times instead of using `decrease_key` is
                // actually faster. Meshes are sparse graphs almost all of the
                // time, since they are a number of planar graphs.
                //
                // See this paper for more information:
                // Chen, Mo, et al. Priority queues and dijkstra's algorithm.
                // Computer Science Department, University of Texas at Austin,
                // 2007.
                heap.push(HeapElem {
                    distance: new_distance,
                    handle: nh,
                });
            }
        }

        // This allows us to quit early. Since we add every vertex potentially
        // multiple times to the heap, the heap still contains a bunch of
        // garbage values after we visited all vertices. With this check we can
        // avoid popping all elements individually.
        if visited.num_elements() == mesh.num_vertices() {
            break;
        }
    }

    vertex_data
}
