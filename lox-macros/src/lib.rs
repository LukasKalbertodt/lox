#![recursion_limit="256"]
#![feature(try_blocks)]

extern crate proc_macro;

use proc_macro::TokenStream;
use syn::DeriveInput;

use crate::derives::input::Input;


#[macro_use]
mod util;

mod derives;
mod mesh;


#[proc_macro]
pub fn mesh(input: TokenStream) -> TokenStream {
    use crate::mesh::MeshInput;

    match syn::parse::<MeshInput>(input) {
        Ok(x) => x.output().into(),
        Err(e) => e.to_compile_error().into(),
    }
}


/// Custom derive for the `Empty` trait.
///
/// Only works on structs. All fields must implement `Empty` in order for this
/// to work.
#[proc_macro_derive(Empty)]
pub fn derive_empty(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as DeriveInput);
    derives::derive_empty(&input)
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}


/// The `derive(MemSink)` macro.
///
/// To derive the trait `MemSink`, you have to attach `#[derive(MemSink)]` to
/// your struct definition (note: currently, the trait can only be derived for
/// structs with named fields). You also have to annotate your fields with
/// `#[lox(...)]` attributes to tell the derive macro what a field should be
/// used for. Example:
///
/// ```ignore
/// #[derive(MemSink)]
/// struct MyMesh {
///     #[lox(core_mesh)]
///     mesh: HalfEdgeMesh,
///
///     #[lox(vertex_position)]
///     positions: VecMap<VertexHandle, Point3<f32>>,
/// }
/// ```
///
/// There is one required field: the core mesh field. That field's type has to
/// implement several mesh traits, in particular `MeshMut` and `TriMeshMut`.
/// You have to annotate that mesh with `#[lox(core_mesh)]`.
///
/// Additionally, you can have fields for each mesh property, like vertex
/// position or face colors. The type of those fields has to implement
/// `PropStoreMut` with a compatible element type. You have to annotate these
/// property fields with the corresponding attribute. The available properties
/// are:
///
/// - `vertex_position`
/// - `vertex_normal`
/// - `vertex_color`
/// - `face_normal`
/// - `face_color`
///
/// Furthermore, there are some configurations (like the cast mode) that can be
/// configured via `lox(...)` attributes as well. See below for more
/// information.
///
///
/// # Exact traits required for each field
///
/// TODO
///
/// # Cast modes
///
/// TODO
#[proc_macro_derive(MemSink, attributes(lox))]
pub fn derive_mem_sink(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as DeriveInput);
    Input::from_syn(&input, "MemSink")
        .map(|i| derives::mem_sink::gen_impl(&i))
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

#[proc_macro_derive(MemSource, attributes(lox))]
pub fn derive_mem_source(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as DeriveInput);
    Input::from_syn(&input, "MemSource")
        .map(|i| derives::mem_source::gen_impl(&i))
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}
