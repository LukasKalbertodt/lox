#![recursion_limit="256"]
#![feature(try_blocks)]

extern crate proc_macro;

use proc_macro::TokenStream;
use syn::DeriveInput;


mod derives;
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


#[proc_macro_derive(MemSink, attributes(lox))]
pub fn derive_mem_sink(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as DeriveInput);
    derives::derive_mem_sink(&input)
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}
