use std::collections::HashMap;

use quote::{quote, quote_spanned};
use proc_macro2::TokenStream as TokenStream2;
use syn::{
    DeriveInput, Error, Fields,
    spanned::Spanned,
};

use crate::util::struct_fields;



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
                    #name: <#ty as lox::Empty>::empty()
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
                    <#ty as lox::Empty>::empty()
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
        impl #impl_generics lox::Empty for #name #ty_generics #where_clause {
            fn empty() -> Self {
                #body
            }
        }
    };

    Ok(out)
}


pub(crate) fn derive_mem_sink(input: &DeriveInput) -> Result<TokenStream2, Error> {
    // Make sure this is a struct and pull out the fields.
    let fields = struct_fields(&input, "only structs can derive `Empty`")?;

    // We can only handle named fields right now
    if !are_named(fields) {
        return Err(Error::new(
            input.span(),
            "`MemSink` can only be derived for structs with named fields",
        ));
    }

    // ===== The core mesh =====
    let e =  Error::new(
        input.span(),
        "no field named `mesh` or with attribute `#[lox(mesh)]` found"
    );
    let mesh_field = find_field(fields, "mesh")?.ok_or(e)?;

    // ===== Vertex positions =====
    let vertex_position_field = find_field(fields, "vertex_positions")?;
    let vertex_position_code = if let Some(field) = vertex_position_field {
        quote! {
            fn prepare_vertex_positions<N: lox::math::PrimitiveNum>(
                &mut self,
                count: lox::handle::DefaultInt,
            ) -> Result<(), lox::io::Error> {
                lox::map::PropStoreMut::reserve(
                    &mut self.#field,
                    count,
                );
                Ok(())
            }

            fn set_vertex_position<N: lox::math::PrimitiveNum>(
                &mut self,
                v: lox::VertexHandle,
                position: lox::cgmath::Point3<N>,
            ) {
                lox::map::PropStoreMut::insert(
                    &mut self.#field,
                    v,
                    unimplemented!(), // TODO
                );
            }
        }
    } else {
        quote! {}
    };


    // Prepare stuff for impl header.
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();


    // Combine everything.
    let out = quote! {
        impl #impl_generics lox::io::MemSink for #name #ty_generics #where_clause {
            fn add_vertex(&mut self) -> lox::VertexHandle {
                lox::MeshMut::add_vertex(&mut self.#mesh_field)
            }

            fn add_face(&mut self, vertices: [lox::VertexHandle; 3]) -> lox::FaceHandle {
                lox::TriMeshMut::add_face(&mut self.#mesh_field, vertices)
            }

            #vertex_position_code
        }
    };

    Ok(out)
}


// ===========================================================================
// ===== Helper stuff
// ===========================================================================

/// Make sure the given `meta` is a `MetaList` and return it. Otherwise, error.
fn get_meta_list(meta: &syn::Meta) -> Result<&syn::MetaList, Error> {
    match meta {
        syn::Meta::Word(w) => {
            Err(Error::new(
                w.span(),
                "empty `lox` attribute (only `#[lox(...)]` is allowed)",
            ))
        }
        syn::Meta::NameValue(v) => {
            Err(Error::new(
                v.span(),
                "value-like `lox` attribute (only `#[lox(...)]` is allowed)",
            ))
        }
        syn::Meta::List(l) => Ok(l),
    }
}

type LoxAttrs = HashMap<String, LoxAttr>;

struct LoxAttr {
    name: syn::Ident,
    values: Vec<LoxAttrValue>,
}

struct LoxAttrValue {
    name: Option<syn::Ident>,
    value: syn::Lit,
}

/// Parses an attribute as `lox` attributes.
///
/// The given attribute has to have the path `lox` and be a meta list (e.g.
/// `lox(...)`). Inside that list we allow simple idents and nested lists (only
/// one level deep, though). Each list item's name must only appear once.
///
/// The items are returned as a hash map.
fn parse_lox_attrs(attr: &syn::Attribute) -> Result<LoxAttrs, Error> {
    // Make sure it's a valid meta list.
    let meta = attr.parse_meta()?;
    let l = get_meta_list(&meta)?;

    // Iterate through all items in the list and collect them in this hash map.
    let mut out = HashMap::new();
    for nested in &l.nested {
        // Make sure the item has the correct type and extract the ident and
        // potential values from it.
        let (ident, values) = match nested {
            syn::NestedMeta::Meta(syn::Meta::Word(w)) => (w, vec![]),
            syn::NestedMeta::Meta(syn::Meta::List(l)) => {
                (&l.ident, vec![]) // TODO
            }
            syn::NestedMeta::Meta(syn::Meta::NameValue(v)) => {
                return Err(Error::new(
                    v.span(),
                    "expected ident, found named value",
                ));
            }
            syn::NestedMeta::Literal(lit) => {
                return Err(Error::new(
                    lit.span(),
                    "expected ident, found literal",
                ));
            }
        };

        // Insert the new attribute into the hashmap
        let v = LoxAttr {
            name: ident.clone(),
            values,
        };
        let prev_entry = out.insert(ident.to_string(), v);

        // Make sure that there was no other attribute with the same name.
        if let Some(prev_attr) = prev_entry {
            return Err(Error::new(
                prev_attr.name.span(),
                "more than one attribute with that name",
            ));
        }
    }

    Ok(out)
}

/// Searches for fields either named `needle` or that do have a lox attribute
/// `needle`. Attributes win over names, so if both exist, the one with the
/// attribute is returned.
fn find_field<'a>(
    fields: &'a syn::Fields,
    needle: &str,
) -> Result<Option<&'a syn::Ident>, Error> {
    // Find the field with ident `needle`.
    let field_with_name = fields.iter()
        .map(|f| f.ident.as_ref().unwrap())
        .find(|&i| i == needle);

    // Find all fields with an attribute `#[lox(#attr)]` where `#attr` is
    // `attr`. We also do some error checking to detect invalid or strange
    // attributes.
    let mut fields_with_attr = Vec::new();
    for f in fields {
        // Find all attributes that start with `lox`
        let lox_attrs = f.attrs.iter()
            .filter(|attr| attr.path.is_ident("lox"))
            .collect::<Vec<_>>();

        // Make sure there is only one such attribute
        if lox_attrs.len() > 1 {
            return Err(Error::new(
                lox_attrs[1].span(),
                "more than one `lox` attribute on field",
            ));
        }

        // If there is a `lox` attribute, let's check it for validity and check
        // if it contains `needle`.
        if lox_attrs.len() == 1 {
            let lox_attrs = parse_lox_attrs(&lox_attrs[0])?;
            if let Some(_) = lox_attrs.get(needle) {
                fields_with_attr.push(f.ident.as_ref().unwrap());
            }
        }
    }

    // Make sure the attribute is only attached to one field
    if fields_with_attr.len() > 1 {
        return Err(Error::new(
            fields_with_attr[1].span(),
            format!("more than one field with '{}' attribute", needle),
        ));
    }

    // If there is a field that is explicitly marked with the attribute `attr`,
    // we return that. Otherwise we return the field with the fitting name, if
    // that exists.
    Ok(fields_with_attr.get(0).cloned().or(field_with_name))
}

/// Returns `true` **iff** the given fields are named fields.
fn are_named(fields: &syn::Fields) -> bool {
    match fields {
        Fields::Named(_) => true,
        _ => false,
    }
}
