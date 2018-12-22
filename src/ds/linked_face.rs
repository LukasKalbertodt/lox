//! Everything related to the `LinkedFaceMesh`.

#[allow(unused_imports)] // TODO
use crate::{
    handle::{DefaultInt, FaceHandle, VertexHandle, Opt},
    map::{VecMap, PropMap},
    traits::{Empty, TriVerticesOfFace, Mesh, TriMesh, TriMeshMut, MeshMut, FacesAroundVertex},
    refs::{FaceRef, VertexRef},
    util::DynList,
};



#[derive(Debug, Clone)]
pub struct LinkedFaceMesh {
    vertices: VecMap<VertexHandle, Vertex>,
    faces: VecMap<FaceHandle, Face>,
}

#[derive(Debug, Clone, Copy)]
struct Vertex {
    /// An arbitrary adjacent face, or `None` if this vertex is not connected
    /// to any face.
    face: Opt<FaceHandle>,

    // TODO: think about storing the index of this vertex in the `vertices`
    // list of `face`.
}

#[derive(Debug, Clone, Copy)]
struct Face {
    /// The adjacent vertices in face-front CCW order.
    vertex_data: [VertexDataInFace; 3],
}

/// Each face stores some data for its three vertices. This struct defines
/// which data is stored.
#[derive(Debug, Clone, Copy)]
struct VertexDataInFace {
    /// The handle of this vertex.
    handle: VertexHandle,

    /// The next face in clockwise order around the vertex.
    ///
    /// If the vertex is only connected to one (this) face, the `next_face`
    /// holds the face handle of this face (i.e. points to itself).
    ///
    /// `next_face` is usually also a neighbor face of the current face, but
    /// this is not the case when the vertex is a non-2-manifold vertex,
    /// meaning that there are two faces only connected by one vertex (▷◁).
    next_face: FaceHandle,
}

impl Face {
    fn is_adjacent_to(&self, vh: VertexHandle) -> bool {
        self.vertex_data[0].handle == vh
            || self.vertex_data[1].handle == vh
            || self.vertex_data[2].handle == vh
    }

    /// Searches for `vh` in the `vertex_data` array and returns its index in
    /// that array (0, 1 or 2).
    ///
    /// `vh` has to be in the array! Otherwise the behavior is unspecified.
    fn idx_of(&self, vh: VertexHandle) -> usize {
        match () {
            () if self.vertex_data[0].handle == vh => 0,
            () if self.vertex_data[1].handle == vh => 1,
            _ => {
                debug_assert!(self.vertex_data[2].handle == vh, "fucky wucky");
                2
            }
        }
    }

    /// Returns a reference to the data of `vh` stored in the `vertex_data`
    /// array.
    ///
    /// `vh` has to be in the array! Otherwise the behavior is unspecified.
    fn data_of(&self, vh: VertexHandle) -> &VertexDataInFace {
        &self.vertex_data[self.idx_of(vh)]
    }

    /// Returns a mutable reference to the data of `vh` stored in the
    /// `vertex_data` array.
    ///
    /// `vh` has to be in the array! Otherwise the behavior is unspecified.
    fn data_mut_of(&mut self, vh: VertexHandle) -> &mut VertexDataInFace {
        &mut self.vertex_data[self.idx_of(vh)]
    }
}

impl LinkedFaceMesh {
    pub fn new() -> Self {
        Self {
            vertices: VecMap::new(),
            faces: VecMap::new(),
        }
    }

    fn circulate_around(&self, center: VertexHandle, start_face: FaceHandle) -> Circulator<'_> {
        Circulator {
            mesh: self,
            center,
            start_face: Opt::some(start_face),
            current_face: Opt::some(start_face),
        }
    }
}

impl Empty for LinkedFaceMesh {
    fn empty() -> Self {
        Self::new()
    }
}

impl Mesh for LinkedFaceMesh {
    fn num_vertices(&self) -> DefaultInt {
        self.vertices.num_elements()
    }

    fn vertices<'s>(&'s self) -> Box<Iterator<Item = VertexRef<Self>> + 's> {
        Box::new(self.vertices.handles().map(move |handle| {
            VertexRef::new(self, handle)
        }))
    }

    fn contains_vertex(&self, vertex: VertexHandle) -> bool {
        self.vertices.contains_handle(vertex)
    }

    fn num_faces(&self) -> DefaultInt {
        self.faces.num_elements()
    }

    fn faces<'s>(&'s self) -> Box<Iterator<Item = FaceRef<Self>> + 's> {
        Box::new(self.faces.handles().map(move |handle| {
            FaceRef::new(self, handle)
        }))
    }

    fn contains_face(&self, face: FaceHandle) -> bool {
        self.faces.contains_handle(face)
    }
}

impl MeshMut for LinkedFaceMesh {
    fn add_vertex(&mut self) -> VertexHandle {
        self.vertices.push(Vertex {
            face: Opt::none(),
        })
    }
}


impl TriMesh for LinkedFaceMesh {}

impl TriMeshMut for LinkedFaceMesh {
    fn add_face(&mut self, vertex_handles: [VertexHandle; 3]) -> FaceHandle {
        println!("> add_face({:?})", vertex_handles);

        let new_fh = self.faces.next_push_handle();

        // Create the array of vertex data stored in the face. The `next_face`
        // is only preliminary and might be changed below.
        let mut vertex_data = [
            VertexDataInFace { handle: vertex_handles[0], next_face: new_fh },
            VertexDataInFace { handle: vertex_handles[1], next_face: new_fh },
            VertexDataInFace { handle: vertex_handles[2], next_face: new_fh },
        ];

        for i in 0..3 {
            let vh = vertex_handles[i];
            let v = &mut self.vertices[vh];
            let v_next_face = &mut vertex_data[i].next_face;

            println!("   > Handling vertex {} ({:?})", i, vh);

            if let Some(face_of_vertex) = v.face.to_option() {
                // The vertex is already connected to a face. We keep its face
                // handle as it is, but we now need to insert this new face
                // into the face-cycle around this vertex. This requires
                // changing/setting the `next_face` link of a couple of faces.
                //
                // Things get more complicated when the vertex is or will be a
                // non-manifold vertex. That means that the order of faces
                // around the vertex is partially ambiguous. In the following,
                // connected faces around the vertex are called `fan blades`.
                // Faces from different fan blades are never adjacent. If the
                // vertex has none or one fan blade, it is a manifold vertex,
                // otherwise it's not.
                //
                // Here is an example: there are three fan-blades around the
                // central vertex X. One fan blade consists of two faces, the
                // other two of only one face.
                //
                //
                //               o---------o
                //                \       /
                //                 \  A  /
                //                  \   /
                //                   \ /
                //          o---------X---------o
                //          |       ╱ | ╲       |
                //          | D   ╱   |   ╲  B  |
                //          |   ╱     |     ╲   |
                //          | ╱       |   C   ╲ |
                //          o         o---------o
                //
                //
                // While the clockwise order seems to be obvious from the
                // ascii-art (A -> B -> C -> D), note that this is a particular
                // embedding of the mesh into a 2D plane. The order of the
                // blades cannot be known. It is entirely possible that a face
                // is added later that is adjacent to C and A. Therefore, the
                // order (A -> D -> B -> C) is equally valid. We only know for
                // sure that `C` comes after `B`.
                //
                // So what do we have to do?
                //
                // For one, we need to change the `next_face` of the face prior
                // to us in the clockwise cycle. There are two possibilties:
                // either the new face is inserted at the end (CW speaking) of
                // an already existing fan-blade, meaning that the last face of
                // a fan-blade is adjacent to the new face. Or this is not the
                // case, which means that there is no real prior face.
                //
                // This is similar to the face after (CW speaking) the new
                // face. Either there is such a face (adjacent to the new face)
                // or there is not.
                //
                // In a first, we circulate around the vertex and find the
                // `prior` and `next` faces, if they exist.

                // The vertices shared with out potential neighbor faces (in
                // addition to `v` of course).
                let vertex_shared_with_prior = vertex_handles[(i + 2) % 3];
                let vertex_shared_with_next = vertex_handles[(i + 1) % 3];

                let mut prior = None;
                let mut next = None;

                // We also search for the face that comes before the first hole
                // in the cycle. A hole is between two fan-blades. So this face
                // is the last face of the first blade (the on with
                // `face_of_vertex` in it).
                let mut before_first_hole = None;

                self.circulate_around(vh, face_of_vertex).for_each(|item| {
                    let face_data = &self.faces[item.face];

                    // Check if the current face face is adjacent to the new
                    // one.
                    //
                    // If `prior`/`next` is already `Some`, we know that with
                    // the new face, there would be three faces adjacent to one
                    // edge. That's not allowed. That's what the
                    // `debug_assert`s are for.
                    if face_data.is_adjacent_to(vertex_shared_with_prior) {
                        debug_assert!(prior.is_none(), "new face would add a non-manifold edge");
                        prior = Some(item.face);
                    }

                    if face_data.is_adjacent_to(vertex_shared_with_next) {
                        debug_assert!(next.is_none(), "new face would add a non-manifold edge");
                        next = Some(item.face);
                    }

                    // Check if this is the last face before the first hole.
                    if before_first_hole.is_none() {
                        // Get the vertex that would be shared with the next
                        // face if that face is adjacent to the current one.
                        let idx_next = (item.vertex_idx as usize + 1) % 3;
                        let shared = face_data.vertex_data[idx_next].handle;
                        let next_face = item.vertex_data.next_face;
                        let shares_vertex = self.faces[next_face].is_adjacent_to(shared);

                        // If the next face does not share a vertex with us (or
                        // the special cae when the next face is the current
                        // face), we have found our hole.
                        if !shares_vertex || item.face == next_face {
                            before_first_hole = Some(item.face);
                        }
                    }
                });

                // let mut current_face = face_of_vertex;
                // loop {
                //     let curr = &self.faces[current_face];
                //     // Get next face in the cycle.
                //     let (idx, next_face) = curr.data_of(vh);

                //     current_face = next_face;

                //     // We will stop when we reached the start.
                //     if current_face == face_of_vertex {
                //         break;
                //     }
                // }

                // There needs to be a hole in the face cycle. Otherwise the
                // new face would introduce a non-manifold edge.
                let before_first_hole = before_first_hole
                    .expect("new face would add a non-manifold edge (no hole found in cycle)");

                println!("    ... before_first_hole: {:?}", before_first_hole);
                println!("    ... prior: {:?}", prior);
                println!("    ... next:  {:?}", next);

                match (prior, next) {
                    // We haven't found any adjacent faces in the cycle, so the
                    // new face will create a new fan-blade. We can pick any
                    // other fan as our prior fan and insert the face after it.
                    // So we just use the `before_first_hole` face from above.
                    (None, None) => {
                        let before_hole_next = &mut self.faces[before_first_hole]
                            .data_mut_of(vh)
                            .next_face;
                        *v_next_face = *before_hole_next;
                        *before_hole_next = new_fh;
                    }

                    // We are adding a face to the end of a fan-blade. This is
                    // simple, we just need to change two handles.
                    (Some(prior), None) => {
                        let prior_next = &mut self.faces[prior].data_mut_of(vh).next_face;
                        *v_next_face = *prior_next;
                        *prior_next = new_fh;
                    }

                    // We are adding a face to the beginning of a fan-blade.
                    // Now we need to find the face that points to
                    // `after_new_face` since that now needs to point to the
                    // new face now. We know such a face exists, otherwise the
                    // cycle is broken and we would never have left the loop
                    // above.
                    (None, Some(after_new_face)) => {
                        let before_next = self.circulate_around(vh, after_new_face)
                            .find(|item| item.vertex_data.next_face == after_new_face)
                            .unwrap()
                            .face;

                        *v_next_face = after_new_face;
                        self.faces[before_next].data_mut_of(vh).next_face = new_fh;
                    }

                    // The new face is between to fan-blades and will connect
                    // them.
                    //
                    // This is more complicated because the fan-blade after the
                    // new face ("after blade") might not be in the cycle after
                    // the fan-blade before the new face ("before blade").
                    // Instead there might be a "wrong blade". This is just
                    // because, as described above, the order of fan-blades is
                    // inherently unspecified. So we might need to repair that
                    // order first.
                    (Some(prior), Some(after_new_face)) => {
                        // Check if we need to repair anything
                        let face_after_prior = self.faces[prior].data_of(vh).next_face;
                        if face_after_prior != after_new_face {
                            struct VertexDataRef {
                                face: FaceHandle,
                                idx: u8,
                            }

                            impl VertexDataRef {
                                fn get<'a>(
                                    &self,
                                    mesh: &'a LinkedFaceMesh,
                                ) -> &'a VertexDataInFace {
                                    &mesh.faces[self.face].vertex_data[self.idx as usize]
                                }

                                fn get_mut<'a>(
                                    &self,
                                    mesh: &'a mut LinkedFaceMesh,
                                ) -> &'a mut VertexDataInFace {
                                    &mut mesh.faces[self.face].vertex_data[self.idx as usize]
                                }
                            }

                            // Find the last face of the blade before "after blade".
                            let before_after_blade = self.circulate_around(vh, after_new_face)
                                .find(|item| item.vertex_data.next_face == after_new_face)
                                .map(|item| VertexDataRef {
                                    face: item.face,
                                    idx: item.vertex_idx,
                                })
                                .unwrap();

                            // `after_new_face` is the start faceof the "after blade". We
                            //  need to find the end face of that blade.
                            let after_blade_end = self.circulate_around(vh, after_new_face)
                                .find(|item| {
                                    let this_face = &self.faces[item.face];
                                    let shared_idx = (item.vertex_idx as usize + 1) % 3;
                                    let shared_vertex = this_face.vertex_data[shared_idx].handle;

                                    let next_face = &self.faces[item.vertex_data.next_face];
                                    let shared_idx = (next_face.idx_of(vh) + 2) % 3;
                                    next_face.vertex_data[shared_idx].handle != shared_vertex
                                })
                                .map(|item| VertexDataRef {
                                    face: item.face,
                                    idx: item.vertex_idx,
                                })
                                .unwrap();

                            // Now cut "after blade" from its current position in the linked list.
                            before_after_blade.get_mut(self).next_face
                                = after_blade_end.get(self).next_face;
                            after_blade_end.get_mut(self).next_face
                                = self.faces[prior].data_of(vh).next_face;
                        }

                        // Insert the new face in between the two blades.
                        *v_next_face = after_new_face;
                        self.faces[prior].data_mut_of(vh).next_face = new_fh;
                    }
                }

            } else {
                // The vertex was not connected to any face. We now set its
                // face handle to the new face. His `next_face` entry is
                // correct already (it also points to the new face).
                v.face = Opt::some(new_fh);
            }
        }

        self.faces.push(Face { vertex_data })
    }
}

// impl TriVerticesOfFace for LinkedFaceMesh {
//     fn vertices_of_face(&self, face: FaceHandle) -> [VertexHandle; 3] {
//         self.faces[face].vertices
//     }
// }

impl FacesAroundVertex for LinkedFaceMesh {
    fn faces_around_vertex(
        &self,
        vh: VertexHandle,
    ) -> Box<dyn DynList<Item = FaceHandle> + '_> {
        // TODO: make nicer and use `circulate_around`
        Box::new(FaceCirculator {
            it: Circulator {
                mesh: self,
                center: vh,
                start_face: self.vertices[vh].face,
                current_face: self.vertices[vh].face,
            },
        })
    }
}

struct FaceCirculator<'a> {
    it: Circulator<'a>,
}

impl DynList for FaceCirculator<'_> {}
impl Iterator for FaceCirculator<'_> {
    type Item = FaceHandle;

    fn next(&mut self) -> Option<Self::Item> {
        self.it.next().map(|item| item.face)
    }
}

struct Circulator<'a> {
    mesh: &'a LinkedFaceMesh,
    center: VertexHandle,
    start_face: Opt<FaceHandle>,
    current_face: Opt<FaceHandle>,
}

/// The data the `Circulator` iterator emits.
#[derive(Debug)]
struct CirculatorItem<'a> {
    face: FaceHandle,
    vertex_data: &'a VertexDataInFace,
    vertex_idx: u8,
}

impl<'a> Iterator for Circulator<'a> {
    type Item = CirculatorItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let prev_face = match self.current_face.to_option() {
            Some(fh) => fh,
            None => return None,
        };

        // Find next face in CW order.
        let f = &self.mesh.faces[prev_face];
        let idx = f.idx_of(self.center);
        let vertex_data = &f.vertex_data[idx];
        self.current_face = Opt::some(vertex_data.next_face);

        // If we reached the beginning again, set `face` to `None` so we won't
        // iterate forever.
        if self.current_face == self.start_face {
            self.current_face = Opt::none();
        }

        Some(CirculatorItem {
            face: prev_face,
            vertex_data,
            vertex_idx: idx as u8,
        })
    }
}

// impl VerticesAroundVertex for LinkedFaceMesh {
//     fn vertices_around_vertex(
//         &self,
//         vertex: VertexHandle,
//     ) -> Box<dyn DynList<Item = VertexHandle> + '_>;
// }


#[cfg(test)]
mod test {
    use super::*;

    gen_tri_mesh_tests!(LinkedFaceMesh: [FacesAroundVertex]);
    // gen_tri_mesh_tests!(LinkedFaceMesh: [TriVerticesOfFace, FacesAroundVertex]);
}
