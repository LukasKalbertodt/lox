//! Everything related to the `mesh!` macro.

use proc_macro2::{Delimiter, TokenStream};
use syn::{
    parse::{Parse, ParseStream, Result},
};

use crate::{
    util::ParseStreamExt,
};


#[derive(Debug)]
pub(crate) struct MeshInput {
    mesh_type: syn::Path,
}

impl MeshInput {
    pub(crate) fn output(self) -> TokenStream {
        let Self { mesh_type } = self;

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
        // Parse type of mesh
        input.eat_ident("type")?;
        input.eat_punct(b":")?;
        let mesh_type = input.parse::<syn::Path>()?;
        input.eat_punct(b",")?;

        // Parse vertex data
        input.eat_ident("vertices")?;
        input.eat_punct(b":")?;
        input.step(|cursor| {
            let (group, _, rest) = match cursor.group(Delimiter::Bracket) {
                Some(x) => x,
                None => return Err(cursor.error(
                    "expected '[ … ]' (group delimited by brackets)"
                )),
            };
            // TODO

            Ok(((), rest))
        })?;
        input.eat_punct(b",")?;

        // Parse face data
        input.eat_ident("faces")?;
        input.eat_punct(b":")?;
        input.step(|cursor| {
            let (group, _, rest) = match cursor.group(Delimiter::Bracket) {
                Some(x) => x,
                None => return Err(cursor.error(
                    "expected '[ … ]' (group delimited by brackets)"
                )),
            };
            // TODO

            Ok(((), rest))
        })?;
        input.eat_punct(b",")?;


        Ok(Self {
            mesh_type,
        })
    }
}
