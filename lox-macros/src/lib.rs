#![recursion_limit="256"]

extern crate proc_macro;

use proc_macro::TokenStream;


#[macro_use]
mod util;

mod mesh;


// See [`lox::mesh`][../lox/macro.mesh.html] for documentation.
#[proc_macro]
pub fn mesh(input: TokenStream) -> TokenStream {
    use crate::mesh::MeshInput;

    match syn::parse::<MeshInput>(input) {
        Ok(x) => x.output().into(),
        Err(e) => e.to_compile_error().into(),
    }
}

