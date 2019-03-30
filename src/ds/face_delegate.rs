//! Everything related to the `FaceDelegateMesh`.

use crate::{
    self as lox, // for proc macros
    prelude::*,
    handle::{hsize, FaceHandle, VertexHandle, Opt},
    map::{VecMap, PropMap, PropStoreMut},
    traits::marker::TriFaces,
    util::{DynList, TriList, TriArrayExt},
};



#[derive(Debug, Clone, Empty)]
pub struct FaceDelegateMesh {
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

    // Ideas about what else to put here:
    // - Indices for each `next_face` reference. Meaning: for a specific
    //   vertex, where does the `next_face` for that vertex store the vertex
    //   information in the array? These are just 0, 1 or 2 so we only need 2
    //   bits. All three of these indices can be tightly packed.
    // - Store whether the `next_face` is adjacent to the current face. Three
    //   bools, three bits.
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
        // Sadly, the `.iter().any()` version doesn't get optimized very well:
        // https://rust.godbolt.org/z/gxohHY
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
                debug_assert!(self.vertex_data[2].handle == vh, "internal `FaceDelegateMesh` bug");
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

impl FaceDelegateMesh {
    /// Creates a circulator iterator around `center` starting at `start_face`.
    fn circulate_around(
        &self,
        center: VertexHandle,
        start_face: impl Into<Opt<FaceHandle>>,
    ) -> Circulator<'_> {
        let start_face = start_face.into();

        Circulator {
            mesh: self,
            center,
            start_face,
            current_face: start_face,
        }
    }

    /// Returns the vertex data referenced by `r`.
    fn vertex_data(&self, r: VertexDataRef) -> &VertexDataInFace {
        &self.faces[r.face].vertex_data[r.idx as usize]
    }

    /// Returns a mutable reference to the vertex data referenced by `r`.
    fn vertex_data_mut(&mut self, r: VertexDataRef) -> &mut VertexDataInFace {
        &mut self.faces[r.face].vertex_data[r.idx as usize]
    }

    /// Inserts the new face `new_fh` into the existing face cycle around the
    /// vertex `vertex_handles[idx]`.
    ///
    /// Since this operation is quite complex, it lives in its own function.
    /// For more details on how this works and why it's so complex, see the
    /// inline comments in the function.
    ///
    /// - `vertex_handles` contains all vertices of the new face in CCW order.
    /// - `idx` is the index of the vertex in question inside of
    ///   `vertex_handles` (i.e. `vertex_handles[idx]` is the vertex we are
    ///   talking about).
    /// - `face_of_vertex` is the face `vertex_handles[idx]` points to.
    /// - `new_fh`: the handle of the face to be inserted
    ///
    /// This function returns the `next_face` handle for the vertex data of
    /// `vertex_handles[idx]` stored inside the new face.
    fn insert_into_cycle(
        &mut self,
        vertex_handles: &[VertexHandle; 3],
        idx: usize,
        face_of_vertex: FaceHandle,
        new_fh: FaceHandle,
    ) -> FaceHandle {
        // We know the vertex is already connected to a face. We now need to
        // insert the new face into the face-cycle around this vertex. This
        // requires changing/setting the `next_face` link of a couple of faces
        // (for this vertex).
        //
        // Things get more complicated when the vertex is or will be a
        // non-manifold vertex. That means that the order of faces around the
        // vertex is partially ambiguous. In the following, connected faces
        // around the vertex are called `fan blades`. Faces from different fan
        // blades are never adjacent. If the vertex has none or one fan blade,
        // it is a manifold vertex, otherwise it's not.
        //
        // Here is an example: there are three fan-blades around the central
        // vertex X. One fan blade consists of two faces, the other two of only
        // one face.
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
        // In these cases, we store the fan blades in an arbitrary order. This
        // is fine, but we need to take some care when we insert a face in such
        // a cycle.
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
        // So first, we circulate around the vertex and find the `prior` and
        // `next` faces (meaning: faces that are adjacent to the new face), if
        // they exist.

        // The vertices shared with out potential neighbor faces (in addition
        // to `v` of course).
        let vh = vertex_handles[idx];
        let vertex_shared_with_prior = vertex_handles[(idx + 2) % 3];
        let vertex_shared_with_next = vertex_handles[(idx + 1) % 3];

        let mut prior = None;
        let mut next = None;

        // We also search for the face that comes before the first hole in the
        // cycle. A hole is between two fan-blades. So this face is the last
        // face of the first blade (the one with `face_of_vertex` in it).
        //
        // TODO: this variable is only used in one case later. Think about
        // doing this search in that case later.
        let mut before_first_hole = None;

        self.circulate_around(vh, face_of_vertex).for_each(|item| {
            let face_data = &self.faces[item.face];

            // Check if the current face is adjacent to the new one.
            //
            // If `prior`/`next` is already `Some`, we know that with the new
            // face, there would be three faces adjacent to one edge. That's
            // not allowed. That's what the `debug_assert`s are for.
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
                // Get the vertex that would be shared with the next face if
                // that face is adjacent to the current one.
                let idx_next = (item.vertex_idx as usize + 1) % 3;
                let shared = face_data.vertex_data[idx_next].handle;
                let next_face = item.vertex_data.next_face;

                // If the next face does not share a vertex with us (or the
                // special case when the next face is the current face), we
                // have found our hole.
                if !self.faces[next_face].is_adjacent_to(shared) || item.face == next_face {
                    before_first_hole = Some(item.face);
                }
            }
        });

        // There needs to be a hole in the face cycle. Otherwise the new face
        // would introduce a non-manifold vertex.
        let before_first_hole = before_first_hole
            .expect("new face would add a non-manifold vertex (no hole found in cycle)");


        // Now we have to decide what to do depending on whether we found a
        // face prior to/after the new one.
        match (prior, next) {
            // We haven't found any adjacent faces in the cycle, so the new
            // face will create a new fan-blade. We can pick any other fan as
            // our prior fan and insert the face after it. So we just use the
            // `before_first_hole` face from above.
            (None, None) => {
                let before_hole_data = self.faces[before_first_hole].data_mut_of(vh);
                let out = before_hole_data.next_face;
                before_hole_data.next_face = new_fh;

                out
            }

            // We are adding a face to the end of a fan-blade. This is simple,
            // we just need to change two handles.
            (Some(prior), None) => {
                let prior_data = self.faces[prior].data_mut_of(vh);
                let out = prior_data.next_face;
                prior_data.next_face = new_fh;

                out
            }

            // We are adding a face to the beginning of a fan-blade. Now we
            // need to find the face that points to `next` since that now needs
            // to point to the new face. We know such a face exists, otherwise
            // the cycle is broken and we would never have left the loop above.
            // The new face needs to point to `next`.
            (None, Some(next)) => {
                let before_next = self.circulate_around(vh, next)
                    .find(|item| item.vertex_data.next_face == next)
                    .unwrap()
                    .face;

                self.faces[before_next].data_mut_of(vh).next_face = new_fh;

                next
            }

            // The new face is between to fan-blades and will connect them.
            //
            // This is the complicated case because the fan-blade after the new
            // face ("after blade") might not be in the cycle after the
            // fan-blade before the new face ("before blade"). Instead there
            // might be a "wrong blade". This is because, as described above,
            // the order of fan-blades is inherently unspecified. So we might
            // need to repair that order first.
            (Some(prior), Some(next)) => {
                // Check if we need to repair anything: if the "before blade"
                // doesn't point to the "after blade".
                let face_after_prior = self.faces[prior].data_of(vh).next_face;
                if face_after_prior != next {
                    // This is the ugly part.
                    //
                    // So we are dealing with a linked cycle of faces around
                    // `vh`. The main idea is to first completely remove "after
                    // blade" from that linked cycle. Then we reinsert it after
                    // "before blade", but without setting one `next` handle,
                    // leaving the linked cycle in a slightly inconsistent
                    // state. However, we can do that because that state is
                    // always repaired by the code after this `if` block.
                    //
                    //
                    // # Visualization
                    //
                    // Names:
                    // - [1]: `face_after_prior`
                    // - [2]: `before_after_blade`
                    // - [3]: `after_blade_end`
                    //
                    //
                    // Current state:
                    //
                    //       ┌──────────────┐     ┌───────────────┐     ┌────────────────┐
                    //       │ before blade │     │  wrong blade  │     │  after blade   │
                    //       ├──────┬───────┤     ├─────┬───┬─────┤     ├──────┬───┬─────┤
                    // X --> │  …   │ prior │ --> │ [1] │ … │ [2] │ --> │ next │ … │ [3] │ --> Y
                    //       └──────┴───────┘     └─────┴───┴─────┘     └──────┴───┴─────┘
                    //
                    //
                    // Remove "after blade" (`[2].next = [3].next`):
                    //
                    //       ┌──────────────┐     ┌───────────────┐
                    //       │ before blade │     │  wrong blade  │
                    //       ├──────┬───────┤     ├─────┬───┬─────┤
                    // X --> │  …   │ prior │ --> │ [1] │ … │ [2] │ --> Y
                    //       └──────┴───────┘     └─────┴───┴─────┘
                    //
                    //
                    // Partially reinsert "after blade" before "wrong blade"
                    // (`[3].next = [1]`):
                    //
                    //       ┌──────────────┐     ┌───────────────┐
                    //       │ before blade │     │  wrong blade  │
                    //       ├──────┬───────┤     ├─────┬───┬─────┤
                    // X --> │  …   │ prior │ --> │ [1] │ … │ [2] │ --> Y
                    //       └──────┴───────┘     └─────┴───┴─────┘
                    //     ┌────────────────┐        ^
                    //     │  after blade   │        |
                    //     ├──────┬───┬─────┤        |
                    //     │ next │ … │ [3] │ -------+
                    //     └──────┴───┴─────┘
                    //
                    // Why is this inconsistent state OK? Because after this
                    // `if` block, `prior.next` is set to the new face. And the
                    // `next` handle of the new face is set to `next`. This
                    // happens regardless of whether this repair is necessary.
                    // So we can leave the linked cycle in this state.
                    //
                    // The main part is now to find [2] (`before_after_blade`)
                    // and [3] (`after_blade_end`).

                    // Find the last face of the blade before "after blade".
                    let before_after_blade = self.circulate_around(vh, next)
                        .find(|item| item.vertex_data.next_face == next)
                        .map(VertexDataRef::from)
                        .unwrap();

                    // `next` is the start face of the "after blade". We need
                    //  to find the end face of that blade.
                    let after_blade_end = self.circulate_around(vh, next)
                        .find(|item| {
                            let this_face = &self.faces[item.face];
                            let shared_idx = (item.vertex_idx as usize + 1) % 3;
                            let shared_vertex = this_face.vertex_data[shared_idx].handle;

                            let next_face = &self.faces[item.vertex_data.next_face];
                            let shared_idx = (next_face.idx_of(vh) + 2) % 3;
                            next_face.vertex_data[shared_idx].handle != shared_vertex
                        })
                        .map(VertexDataRef::from)
                        .unwrap();

                    // Now cut "after blade" from its current position in the
                    // linked cycle.
                    self.vertex_data_mut(before_after_blade).next_face
                        = self.vertex_data(after_blade_end).next_face;

                    // Partially reinsert it into the linked cycle.
                    self.vertex_data_mut(after_blade_end).next_face
                        = self.faces[prior].data_of(vh).next_face;
                }

                // Insert the new face in between the two blades (`prior` now
                // needs to point to the new face and the new face to `next`).
                self.faces[prior].data_mut_of(vh).next_face = new_fh;

                next
            }
        }
    }
}

impl Mesh for FaceDelegateMesh {
    type FaceKind = TriFaces;

    fn num_vertices(&self) -> hsize {
        self.vertices.num_elements()
    }

    fn vertex_handles(&self) -> Box<Iterator<Item = VertexHandle> + '_> {
        Box::new(self.vertices.handles())
    }

    fn contains_vertex(&self, vertex: VertexHandle) -> bool {
        self.vertices.contains_handle(vertex)
    }

    fn num_faces(&self) -> hsize {
        self.faces.num_elements()
    }

    fn face_handles(&self) -> Box<Iterator<Item = FaceHandle> + '_> {
        Box::new(self.faces.handles())
    }

    fn contains_face(&self, face: FaceHandle) -> bool {
        self.faces.contains_handle(face)
    }
}

impl MeshMut for FaceDelegateMesh {
    fn add_vertex(&mut self) -> VertexHandle {
        self.vertices.push(Vertex {
            face: Opt::none(),
        })
    }

    fn remove_all_vertices(&mut self) {
        assert!(
            self.num_faces() == 0,
            "call to `remove_all_vertices`, but there are faces in the mesh!",
        );

        self.vertices.clear();
    }

    fn remove_all_faces(&mut self) {
        self.faces.clear();
        for v in self.vertices.values_mut() {
            v.face = Opt::none();
        }
    }
}


impl TriMesh for FaceDelegateMesh {}

impl TriMeshMut for FaceDelegateMesh {
    fn add_face(&mut self, vertex_handles: [VertexHandle; 3]) -> FaceHandle {
        assert_ne!(vertex_handles[0], vertex_handles[1], "vertices of new face are not unique");
        assert_ne!(vertex_handles[0], vertex_handles[2], "vertices of new face are not unique");

        let new_fh = self.faces.next_push_handle();

        // Create the array of vertex data stored in the face. The `next_face`
        // is only preliminary and might be changed below.
        let mut vertex_data = vertex_handles.map(|handle| VertexDataInFace {
            handle,
            next_face: new_fh,
        });

        for i in 0..3 {
            let vh = vertex_handles[i];
            let v = &mut self.vertices[vh];

            if let Some(face_of_vertex) = v.face.to_option() {
                // The vertex is already connected to a face. We keep its face
                // handle as it is, but we now need to insert this new face
                // into the face-cycle around this vertex.
                vertex_data[i].next_face
                    = self.insert_into_cycle(&vertex_handles, i, face_of_vertex, new_fh);
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

impl TriVerticesOfFace for FaceDelegateMesh {
    fn vertices_of_face(&self, face: FaceHandle) -> [VertexHandle; 3] {
        self.faces[face].vertex_data.map(|d| d.handle)
    }
}

impl TriFacesAroundFace for FaceDelegateMesh {
    fn faces_around_face(&self, face: FaceHandle) -> TriList<FaceHandle> {
        let data = &self.faces[face].vertex_data;

        TriList::new([0, 1, 2].map(|i| {
            let v = &data[i];

            // Check if the `next_face` is adjacent to `face`. If it's not, we
            // don't emit it.
            let shared = data[(i + 1) % 3].handle;
            if v.next_face != face && self.faces[v.next_face].is_adjacent_to(shared) {
                Some(v.next_face)
            } else {
                None
            }
        }))
    }
}

impl FacesAroundVertex for FaceDelegateMesh {
    fn faces_around_vertex(
        &self,
        vh: VertexHandle,
    ) -> Box<dyn DynList<Item = FaceHandle> + '_> {
        Box::new(FaceCirculator {
            it: self.circulate_around(vh, self.vertices[vh].face),
        })
    }
}

/// Iterator over all faces of a vertex. Is returned by `faces_around_vertex`.
///
/// This is basically just a `Circulator {...}.map(|item| item.face)`.
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

impl SupportsMultiBlade for FaceDelegateMesh {}

impl VerticesAroundVertex for FaceDelegateMesh {
    fn vertices_around_vertex(
        &self,
        vh: VertexHandle,
    ) -> Box<dyn DynList<Item = VertexHandle> + '_> {
        Box::new(VertexCirculator {
            it: self.circulate_around(vh, self.vertices[vh].face),
            queue: None,
        })
    }
}

/// Iterator over all neighbor vertices of a vertex. Is returned by
/// `vertices_around_vertex`.
struct VertexCirculator<'a> {
    it: Circulator<'a>,
    queue: Option<VertexHandle>,
}

impl DynList for VertexCirculator<'_> {}
impl Iterator for VertexCirculator<'_> {
    type Item = VertexHandle;

    fn next(&mut self) -> Option<Self::Item> {
        // This is a bit more complicated than `FaceCirculator`: the number of
        // vertices around a vertex might be different from the number of
        // faces.

        // If a vertex is stored in `queue`, we emit it before doing anything
        // else.
        if let Some(vh) = self.queue.take() {
            return Some(vh);
        }

        // Else, we pull a new face from the underlying circulator.
        let item = match self.it.next() {
            None => return None,
            Some(item) => item,
        };

        let face_data = &self.it.mesh.faces[item.face];

        // We will first emit the vertex of the current face that is not
        // shared with the next face.
        let out = face_data.vertex_data[(item.vertex_idx as usize + 2) % 3].handle;

        // If the face is the last face in its blade, we store the other vertex
        // of that face in `queue` to emit it in the next iteration.
        let next_vertex = face_data.vertex_data[(item.vertex_idx as usize + 1) % 3].handle;
        let next_face = item.vertex_data.next_face;

        // If the next face does not share that vertex with us (or the special
        // case when the next face is the current face), we have found a hole
        // and need to queue the `next_vertex`.
        if !self.it.mesh.faces[next_face].is_adjacent_to(next_vertex) || item.face == next_face {
            self.queue = Some(next_vertex);
        }

        Some(out)
    }
}


/// References the data of a specific vertex stored in a face. Only stores face
/// handle and index, meaning: the struct has no lifetime.
#[derive(Clone, Copy)]
struct VertexDataRef {
    face: FaceHandle,
    idx: u8,
}

impl<'a> From<CirculatorItem<'a>> for VertexDataRef {
    fn from(src: CirculatorItem<'a>) -> Self {
        Self {
            face: src.face,
            idx: src.vertex_idx,
        }
    }
}


struct Circulator<'a> {
    mesh: &'a FaceDelegateMesh,
    center: VertexHandle,
    start_face: Opt<FaceHandle>,
    current_face: Opt<FaceHandle>,
}

/// The data the `Circulator` iterator emits.
#[derive(Debug)]
struct CirculatorItem<'a> {
    face: FaceHandle,
    vertex_data: &'a VertexDataInFace,

    /// The index of the center vertex in the `vertex_data` array inside
    /// `face`. Meaning: ´mesh.faces[self.face].vertex_data[self.vertex_idx]`
    /// is the same as `self.vertex_data`.
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



#[cfg(test)]
mod test {
    use super::*;

    gen_tri_mesh_tests!(FaceDelegateMesh: [
        FacesAroundVertex,
        VerticesAroundVertex,
        TriVerticesOfFace,
        TriFacesAroundFace,
        Manifold,
        SupportsMultiBlade
    ]);
}
