//! Everything related to the `mesh!` macro.

use proc_macro2::{Delimiter, Ident, TokenStream};
use syn::{
    bracketed, parenthesized, Token,
    parse::{Parse, ParseStream, Result},
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
        let Self { mesh_type, vertices, faces } = self;

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
        let mut vertices = Vec::new();
        while !inner.is_empty() {
            // Parse name and colon
            let name = inner.eat_ident(None)?;
            inner.eat_punct(b":")?;

            // Parse values
            let values_inner;
            parenthesized!(values_inner in inner);
            let exprs: Punctuated<_, Token![,]>
                = values_inner.parse_terminated(syn::Expr::parse)?;

            // Eat comma
            if !inner.is_empty() {
                inner.eat_punct(b",")?;
            }

            vertices.push((name, exprs.into_iter().collect()));
        }
        input.eat_punct(b",")?;


        // ----- Parse face data ---------------------
        input.eat_ident("faces")?;
        input.eat_punct(b":")?;

        let inner;
        bracketed!(inner in input);
        let mut faces = Vec::new();
        while !inner.is_empty() {
            // We fork the token stream to get a span to the group we are about
            // to parse. We need this for nice errors.
            let error_fork = inner.fork();

            // Parse vertices of face: `[<ident>, <ident>, <ident>]`
            let vertices_inner;
            bracketed!(vertices_inner in inner);
            let vertices_of_face: Punctuated<_, Token![,]>
                = vertices_inner.parse_terminated(syn::Ident::parse)?;

            // Make sure we have exactly three vertices
            let vertices_of_face = if vertices_of_face.len() != 3 {
                return Err(error_fork.error("right now, only triangular faces are supported"));
            } else {
                let mut it = vertices_of_face.into_iter();
                [it.next().unwrap(), it.next().unwrap(), it.next().unwrap()]
            };

            // Eat colon
            inner.eat_punct(b":")?;

            // Parse values
            let values_inner;
            parenthesized!(values_inner in inner);
            let exprs: Punctuated<_, Token![,]>
                = values_inner.parse_terminated(syn::Expr::parse)?;

            // Eat comma
            if !inner.is_empty() {
                inner.eat_punct(b",")?;
            }

            faces.push((vertices_of_face, exprs.into_iter().collect()));
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
