//! Everything related to the `mesh!` macro.

use quote::quote;
use proc_macro2::{Ident, Span, TokenStream};
use syn::{
    bracketed, parenthesized, Token, token,
    parse::{Error, Parse, ParseStream, ParseBuffer, Result},
    punctuated::Punctuated,
};

use crate::{
    util::ParseBufferExt,
};


#[derive(Debug)]
pub(crate) struct MeshInput {
    mesh_type: syn::Path,
    vertices: Vec<(Ident, Vec<syn::Expr>)>,
    faces: Vec<(Vec<Ident>, Vec<syn::Expr>)>,
}

impl MeshInput {
    pub(crate) fn output(self) -> TokenStream {
        let Self { mesh_type, vertices, faces } = self;

        /// Helper function to create idents for `len` many property maps.
        fn create_map_idents(prefix: &str, len: Option<usize>) -> Vec<Ident> {
            (0..len.unwrap_or(0))
                .map(|i| {
                    Ident::new(
                        &format!("{}_{}", prefix, i),
                        Span::call_site(), // TODO: sure this is ok?
                    )
                })
                .collect()
        }

        // Create the idents for the vertex and face maps
        let vertex_maps = create_map_idents("vertex_map", vertices.get(0).map(|(_, v)| v.len()));
        let face_maps = create_map_idents("face_map", faces.get(0).map(|(_, v)| v.len()));

        // Create code that adds vertices to the mesh and the corresponding
        // properties to the property maps.
        let vertex_count = vertices.len() as u32;
        let mut add_vertices = vertex_maps.iter()
            .map(|map| {
                quote! {
                    let mut #map = DenseMap::new();
                    PropStoreMut::reserve(&mut #map, Into::into(#vertex_count));
                }
            })
            .collect::<TokenStream>();
        for (name, values) in vertices {
            add_vertices.extend(quote! {
                let #name = MeshMut::add_vertex(&mut mesh);
            });

            for (value, map_ident) in values.into_iter().zip(&vertex_maps) {
                add_vertices.extend(quote! {
                    PropStoreMut::insert(&mut #map_ident, #name, #value);
                });
            }
        }

        // Create code that adds faces to the mesh and the corresponding
        // properties to the property maps.
        let face_count = faces.len() as u32;
        let mut add_faces = face_maps.iter()
            .map(|map| {
                quote! {
                    let mut #map = DenseMap::new();
                    PropStoreMut::reserve(&mut #map, Into::into(#face_count));
                }
            })
            .collect::<TokenStream>();
        for (vertices, values) in faces {
            let call = if vertices.len() == 3 {
                quote! { MeshMut::add_triangle(&mut mesh, [ #(#vertices ,)* ]) }
            } else {
                quote! { MeshMut::add_face(&mut mesh, &[ #(#vertices ,)* ]) }
            };

            add_faces.extend(quote! { let face = #call; });

            for (value, map_ident) in values.into_iter().zip(&face_maps) {
                add_faces.extend(quote! {
                    PropStoreMut::insert(&mut #map_ident, face, #value);
                });
            }
        }

        // Combine everything
        quote! {{
            // So: imports are hard. We cannot be sure that the crate `lox` is
            // used by the crate using this macro. We just have to hope the
            // user crate doesn't rename the `lox` crate. Additionally, we hope
            // that the user does not have a local submodule called `lox` since
            // with uniform paths, that would lead to a ambiguity error.
            use std::convert::Into;
            use lox::{
                traits::{MeshMut, Empty},
                map::{PropStoreMut, DenseMap},
            };

            let mut mesh = <#mesh_type as Empty>::empty();
            MeshMut::reserve_for_vertices(&mut mesh, Into::into(#vertex_count));
            MeshMut::reserve_for_faces(&mut mesh, Into::into(#face_count));

            #add_vertices
            #add_faces

            (
                mesh
                #(, #vertex_maps )*
                #(, #face_maps )*
            )
        }}
    }
}

impl Parse for MeshInput {
    fn parse(input: ParseStream) -> Result<Self> {
        /// Parses optional values for vertices or faces:
        ///
        /// ```text
        ///     <nothing>
        ///     : expr
        ///     : (expr)
        ///     : (expr_a, expr_b)
        /// ```
        ///
        /// Returns the (potentially empty) list of expressions. Also checks
        /// the number of parsed expressions against `expected_len`, if
        /// provided.
        fn parse_prop_values(
            buf: &ParseBuffer,
            expected_len: Option<usize>,
        ) -> Result<Vec<syn::Expr>> {
            if buf.peek(Token![:]) {
                buf.eat_punct(b":")?;

                // Fork for span information
                let values_span = buf.cursor().span();

                // Check if multiple values are given (in parenthesis, with
                // tuple syntax) or if it's only a single value.
                let values = if buf.peek(token::Paren) {
                    // Parse values
                    let values_inner;
                    parenthesized!(values_inner in buf);
                    let exprs = values_inner.parse_terminated::<_, Token![,]>(syn::Expr::parse)?;
                    exprs.into_iter().collect()
                } else {
                    vec![buf.parse::<syn::Expr>()?]
                };

                // Check that the number of values equals the previous
                // vertices.
                if let Some(expected_len) = expected_len {
                    if values.len() != expected_len {
                        let msg = format!(
                            "all vertices must have the same number of properties (the first \
                                vertex has {}, this vertex has {})",
                            expected_len,
                            values.len(),
                        );
                        return Err(Error::new(values_span, msg));
                    }
                }


                Ok(values)
            } else {
                Ok(vec![])
            }
        }


        // ----- Parse type of mesh -------------------
        input.eat_ident("type")?;
        input.eat_punct(b":")?;
        let mesh_type = input.parse::<syn::Path>()?;
        input.eat_punct(b",")?;


        // ----- Parse vertex data --------------------
        input.eat_ident("vertices")?;
        input.eat_punct(b":")?;

        let inner;
        bracketed!(inner in input);
        let mut vertices: Vec<(_, Vec<_>)> = Vec::new();
        // let mut count = None;
        while !inner.is_empty() {
            // Parse name and values
            let name = inner.eat_ident(None)?;
            let values = parse_prop_values(&inner, vertices.get(0).map(|v| v.1.len()))?;

            // Eat optional comma
            if !inner.is_empty() {
                inner.eat_punct(b",")?;
            }

            vertices.push((name, values));
        }

        input.eat_punct(b",")?;


        // ----- Parse face data ---------------------
        input.eat_ident("faces")?;
        input.eat_punct(b":")?;

        let inner;
        bracketed!(inner in input);
        let mut faces: Vec<(_, Vec<_>)> = Vec::new();
        while !inner.is_empty() {
            // Parse vertices of face: `[<ident>, <ident>, <ident>]`
            let vertex_list_span = inner.cursor().span();
            let vertices_inner;
            bracketed!(vertices_inner in inner);
            let vertices_of_face: Punctuated<_, Token![,]>
                = vertices_inner.parse_terminated(syn::Ident::parse)?;

            // Make sure we have enough vertices
            if vertices_of_face.len() < 3 {
                return Err(Error::new(
                    vertex_list_span,
                    "faces must have at least three vertices",
                ));
            }

            let vertices_of_face = vertices_of_face.into_iter().collect::<Vec<_>>();

            // Parse face properties.
            let values = parse_prop_values(&inner, faces.get(0).map(|v| v.1.len()))?;


            // Eat comma
            if !inner.is_empty() {
                inner.eat_punct(b",")?;
            }

            faces.push((vertices_of_face, values));
        }

        // Optionally eat trailing comma
        if !input.is_empty() {
            input.eat_punct(b",")?;
        }


        Ok(Self {
            mesh_type,
            vertices,
            faces,
        })
    }
}
