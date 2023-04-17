#![recursion_limit="256"]

extern crate proc_macro;

use proc_macro::TokenStream;


#[macro_use]
mod util;

mod mesh;


/// Convenience macro to quickly create a small mesh.
///
/// # Examples
///
/// Here we create two triangles:
///
/// ```
/// use lox::{
///     mesh,
///     prelude::*,
///     core::SharedVertexMesh,
/// };
///
///
/// let (mesh, positions, distances, face_colors) = mesh! {
///     type: SharedVertexMesh,
///     vertices: [
///         v0: ([0.0, 0.0, 0.0], 0.0),
///         v1: ([0.0, 1.0, 0.0], 1.0),
///         v2: ([1.0, 0.0, 0.0], 1.0),
///         v3: ([1.0, 1.0, 0.0], 1.414),
///     ],
///     faces: [
///         [v0, v2, v1]: ("red"),
///         [v3, v1, v2]: ("green"),
///     ],
/// };
///
/// assert_eq!(mesh.num_vertices(), 4);
/// assert_eq!(mesh.num_faces(), 2);
/// ```
///
/// In the code above, we associate a position and a scalar value with each
/// vertex and a color (or rather, a color name) with each face. Properties of
/// vertices and faces are specified after a colon (`:`) in parenthesis (like a
/// tuple).
///
/// For each property you add in those parenthesis, the macro returns an
/// additional property map. The full return value is:
///
/// ```text
/// (mesh, /* vertex property maps */, /* face property maps*/)
/// ```
///
/// ## Without properties
///
/// We don't need to specify any properties. We can either write empty
/// parenthesis (`()`) or just omit the colon and the parenthesis:
///
/// ```
/// use lox::{
///     mesh,
///     core::SharedVertexMesh,
/// };
///
///
/// let mesh = mesh! {
///     type: SharedVertexMesh,
///     vertices: [
///         v0: (),  // <-- this is equivalent to:
///         v1,      // <-- this
///         v2,
///         v3,
///     ],
///     faces: [
///         [v0, v2, v1],
///         [v3, v1, v2],
///     ],
/// };
/// ```
///
/// Of course, you can also add properties to the vertices, but not the faces,
/// or the other way around. However, you always have to specify the same
/// number of properties for all vertices and the same number of properties for
/// all faces.
///
/// ## An empty mesh
///
/// This is not particularly useful in itself, but it works. You can use this
/// syntax when you haven't yet decided how your mesh should look like.
///
/// ```
/// use lox::{
///     mesh,
///     core::SharedVertexMesh,
/// };
///
///
/// let empty_mesh = mesh! {
///     type: SharedVertexMesh,
///     vertices: [],
///     faces: [],
/// };
/// ```
#[proc_macro]
pub fn mesh(input: TokenStream) -> TokenStream {
    use crate::mesh::MeshInput;

    match syn::parse::<MeshInput>(input) {
        Ok(x) => x.output().into(),
        Err(e) => e.to_compile_error().into(),
    }
}

