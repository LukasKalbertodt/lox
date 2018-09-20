#![feature(try_blocks)]

extern crate proc_macro;
#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

mod util;
mod mesh;


#[proc_macro]
pub fn mesh(input: TokenStream) -> TokenStream {
    use crate::mesh::MeshInput;

    match syn::parse::<MeshInput>(input) {
        Ok(x) => x.output().into(),
        Err(e) => e.to_compile_error().into(),
    }
}
