//! Everything related to the `mesh!` macro.

use proc_macro2::{Ident, Span, TokenStream};
use syn::{
    bracketed, parenthesized, Token,
    parse::{Error, Parse, ParseStream, Result},
    punctuated::Punctuated,
};

use crate::{
    util::ParseBufferExt,
};


#[derive(Debug)]
pub(crate) struct MeshInput {
    is_internal_call: bool,
    mesh_type: syn::Path,
    vertices: Vec<(Ident, Vec<syn::Expr>)>,
    faces: Vec<([Ident; 3], Vec<syn::Expr>)>,
}

impl MeshInput {
    pub(crate) fn output(self) -> TokenStream {
        let Self { is_internal_call, mesh_type, vertices, faces } = self;

        // TODO: reserve memory for the mesh

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
        let vertex_count = vertices.len();
        let mut add_vertices = vertex_maps.iter()
            .map(|map| {
                quote! {
                    let mut #map = VecMap::new();
                    PropStoreMut::reserve(&mut #map, #vertex_count);
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
        let face_count = faces.len();
        let mut add_faces = face_maps.iter()
            .map(|map| {
                quote! {
                    let mut #map = VecMap::new();
                    PropStoreMut::reserve(&mut #map, #face_count);
                }
            })
            .collect::<TokenStream>();
        for ([va, vb, vc], values) in faces {
            add_faces.extend(quote! {
                let face = TriMeshMut::add_face(&mut mesh, [#va, #vb, #vc]);
            });

            for (value, map_ident) in values.into_iter().zip(&face_maps) {
                add_faces.extend(quote! {
                    PropStoreMut::insert(&mut #map_ident, face, #value);
                });
            }
        }

        let crate_ident = if is_internal_call {
            quote! { crate }
        } else {
            quote! { ::lox }
        };

        // Combine everything
        quote! {{
            use #crate_ident::{
                MeshMut, TriMeshMut, Empty,
                map::{PropStoreMut, VecMap},
            };

            let mut mesh = <#mesh_type as Empty>::empty();

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
        // ----- Check for special internal marker ----
        let is_internal_call = input.eat_punct(b"*").is_ok();

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
            // Parse name and colon
            let name = inner.eat_ident(None)?;

            // There might or might not be values specified after a colon.
            let (values, error_span) = if inner.peek(Token![:]) {
                inner.eat_punct(b":")?;

                // Fork for span information
                let values_span = inner.cursor().span();

                // Parse values
                let values_inner;
                parenthesized!(values_inner in inner);
                let exprs: Punctuated<_, Token![,]>
                    = values_inner.parse_terminated(syn::Expr::parse)?;

                (exprs.into_iter().collect(), values_span)
            } else {
                (vec![], name.span())
            };

            // Check that the number of properties of this vertex matches
            // the number of the first one.
            if !vertices.is_empty() {
                if values.len() != vertices[0].1.len() {
                    let msg = format!(
                        "all vertices must have the same number of properties (the first \
                            vertex has {}, this vertex has {})",
                        vertices[0].1.len(),
                        values.len(),
                    );
                    return Err(Error::new(error_span, msg));
                }
            }

            // Eat comma
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

            // Make sure we have exactly three vertices
            let vertices_of_face = if vertices_of_face.len() != 3 {
                return Err(Error::new(
                    vertex_list_span,
                    "right now, only triangular faces are supported",
                ));
            } else {
                let mut it = vertices_of_face.into_iter();
                [it.next().unwrap(), it.next().unwrap(), it.next().unwrap()]
            };

            // There might or might not be values specified after a colon.
            let (values, error_span) = if inner.peek(Token![:]) {
                inner.eat_punct(b":")?;

                let values_span = inner.cursor().span();

                // Parse values
                let values_inner;
                parenthesized!(values_inner in inner);
                let exprs: Punctuated<_, Token![,]>
                    = values_inner.parse_terminated(syn::Expr::parse)?;

                (exprs.into_iter().collect(), values_span)
            } else {
                (vec![], vertex_list_span)
            };

            // Check that the number of properties of this faces matches the
            // number of the first one.
            if !faces.is_empty() {
                if values.len() != faces[0].1.len() {
                    let msg = format!(
                        "all faces must have the same number of properties (the first \
                            face has {}, this face has {})",
                        faces[0].1.len(),
                        values.len(),
                    );
                    return Err(Error::new(error_span, msg));
                }
            }

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
            is_internal_call,
            mesh_type,
            vertices,
            faces,
        })
    }
}
