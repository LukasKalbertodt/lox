//! Everything related to the `mesh!` macro.

use proc_macro2::{Ident, TokenStream};
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
    mesh_type: syn::Path,
    vertices: Vec<(Ident, Vec<syn::Expr>)>,
    faces: Vec<([Ident; 3], Vec<syn::Expr>)>,
}

impl MeshInput {
    pub(crate) fn output(self) -> TokenStream {
        let Self { mesh_type, .. } = self;

        quote! {{
            use lox::{
                Mesh, ExplicitFace, ExplicitVertex,
                map::{PropStoreMut, VecMap},
            };

            <#mesh_type as Mesh>::empty()
        }}
    }
}

impl Parse for MeshInput {
    fn parse(input: ParseStream) -> Result<Self> {
        // ----- Parse type of mesh ------------------
        input.eat_ident("type")?;
        input.eat_punct(b":")?;
        let mesh_type = input.parse::<syn::Path>()?;
        input.eat_punct(b",")?;


        // ----- Parse vertex data -------------------
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
            mesh_type,
            vertices,
            faces,
        })
    }
}
