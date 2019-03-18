//! Contains all code for custom derives.

use quote::{quote, quote_spanned};
use proc_macro2::TokenStream as TokenStream2;
use syn::{
    DeriveInput, Error, Fields,
    spanned::Spanned,
};

use crate::util::struct_fields;


pub(crate) mod input;
pub(crate) mod mem_sink;
pub(crate) mod mem_source;


/// Specifies the default casting mode when the casting mode is not explicitly
/// specifid.
const DEFAULT_CAST_MODE: input::CastMode = input::CastMode::Lossless;

/// Specifies whether casting colors (in "rounding" mode) is allowed when a
/// casting mode is not explicitly specified.
const DEFAULT_COLOR_CAST_ALLOWED: bool = true;


/// Generates the `impl Empty` code for the given input type definition.
pub(crate) fn derive_empty(input: &DeriveInput) -> Result<TokenStream2, Error> {
    // Make sure this is a struct and pull out the fields.
    let fields = struct_fields(&input, "only structs can derive `Empty`")?;

    // Prepare stuff for impl header.
    // TODO: maybe add `Empty` bounds to generics?
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    // Generate the method body
    let body = match fields {
        Fields::Named(fields) => {
            let field_initializer = fields.named.iter().map(|f| {
                let name = &f.ident;
                let ty = &f.ty;
                quote_spanned! {f.span()=>
                    #name: <#ty as lox::traits::Empty>::empty()
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
                    <#ty as lox::traits::Empty>::empty()
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
        impl #impl_generics lox::traits::Empty for #name #ty_generics #where_clause {
            fn empty() -> Self {
                #body
            }
        }
    };

    Ok(out)
}
