#![feature(try_blocks)]

extern crate proc_macro;
#[macro_use]
extern crate quote;

use self::proc_macro::TokenStream;

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
    use syn::{
        DeriveInput, Error, Data, Fields,
        spanned::Spanned,
    };

    let input = syn::parse_macro_input!(input as DeriveInput);

    // Make sure this is a struct and pull out the fields.
    let fields = match &input.data {
        Data::Struct(s) => &s.fields,
        _ => {
            let msg = "only structs can derive `Empty`";
            return Error::new(input.span(), msg).to_compile_error().into();
        }
    };

    // TODO: allow this derive to be used internally (via `crate`)
    let crate_prefix = quote! { ::lox };

    // Prepare stuff for impl header.
    // TODO: maybe add `Empty` bounds to generics?
    let name = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    // Generate the method body
    let body = match fields {
        Fields::Named(fields) => {
            let field_initializer = fields.named.iter().map(|f| {
                let name = &f.ident;
                let ty = &f.ty;
                quote_spanned! {f.span()=>
                    #name: <#ty as #crate_prefix::Empty>::empty()
                }
            });

            quote! {
                Self {
                    #(#field_initializer ,)*
                }
            }
        }
        Fields::Unnamed(fields) => {
            let field_initializer = fields.unnamed.iter().map(|f| {
                let ty = &f.ty;
                quote_spanned! {f.span()=>
                    <#ty as #crate_prefix::Empty>::empty()
                }
            });

            quote! {
                #name(#(#field_initializer ,)*)
            }

        }
        Fields::Unit => quote! { #name },
    };

    // Combine everything.
    let out = quote! {
        impl #impl_generics #crate_prefix::Empty for #name #ty_generics #where_clause {
            fn empty() -> Self {
                #body
            }
        }
    };

    out.into()
}
