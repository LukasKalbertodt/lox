#[allow(unused_imports)] // TODO
use proc_macro2::{
    TokenStream, Span,
};
use quote::{quote, quote_spanned};
use syn::{
    Error, Ident,
    spanned::Spanned,
};
use super::{
    DEFAULT_COLOR_CAST_ALLOWED,
    input::{CastMode, ColorPropField, CoreMeshField, Input, PropField},
};


pub(in crate::derives) fn gen_impl(input: &Input) -> Result<TokenStream, Error> {
    macro_rules! maybe_field {
        ($field:ident, |$f:ident| $body:expr) => {
            input.$field
                .as_ref()
                .map(|$f| $body)
                .unwrap_or(TokenStream::new())
        }
    }

    let mesh_code = gen_mesh_code(&input.core_mesh);

    let global_cast_mode = input.cast_mode.as_ref().map(|m| m.mode);
    let vertex_color_code = maybe_field!(vertex_color, |f| {
        gen_color_prop_code(f, "Vertex", global_cast_mode)
    });
    let face_color_code = maybe_field!(face_color, |f| {
        gen_color_prop_code(f, "Face", global_cast_mode)
    });

    // Prepare stuff for impl header.
    let name = &input.name;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    // Combine everything.
    let out = quote! {
        impl #impl_generics lox::io::MemSink for #name #ty_generics #where_clause {
            #mesh_code
            // #finish_code
            // #vertex_position_code
            // #vertex_normal_code
            #vertex_color_code
            // #face_normal_code
            #face_color_code
        }
    };

    Ok(out)
}


/// Generates the code for `add_vertex`, `add_face` and `size_hint` (everything
/// that only involves the `core_mesh` field).
fn gen_mesh_code(field: &CoreMeshField) -> TokenStream {
    let field_name = &field.name;

    // The bodies of the functions are just explicit function calls. We use the
    // type's span for them to make error reporting better.
    let add_vertex = quote_spanned!{field.ty.span()=>
        lox::traits::MeshMut::add_vertex(&mut self.#field_name)
    };
    let add_face = quote_spanned!{field.ty.span()=>
        lox::traits::TriMeshMut::add_face(&mut self.#field_name, vertices)
    };
    let size_hint = quote_spanned!{field.ty.span()=>
        lox::traits::MeshMut::reserve_for_vertices(
            &mut self.#field_name,
            lox::util::MeshSizeHint::guess_vertex_count(&hint),
        );
        lox::traits::MeshMut::reserve_for_faces(
            &mut self.#field_name,
            lox::util::MeshSizeHint::guess_face_count(&hint),
        );
    };

    quote! {
        fn add_vertex(&mut self) -> lox::VertexHandle {
            #add_vertex
        }
        fn add_face(&mut self, vertices: [lox::VertexHandle; 3]) -> lox::FaceHandle {
            #add_face
        }
        fn size_hint(&mut self, hint: lox::util::MeshSizeHint) {
            #size_hint
        }
    }
}

/// Generates the code for `prepare_*_colors` and `set_*_color` for a given
/// field.
///
/// The `elem` string has to be either `"Face"` or `"Vertex"` -- the first
/// character has to be uppercase.
fn gen_color_prop_code(
    field: &ColorPropField,
    elem: &str,
    global_cast_mode: Option<CastMode>,
) -> TokenStream {
    // Create idents
    let elem_color = Ident::new(&format!("{}Color", elem), Span::call_site());
    let elem_handle = Ident::new(&format!("{}Handle", elem), Span::call_site());
    let elem = elem.to_lowercase();
    let prep_fn_name = Ident::new(&format!("prepare_{}_colors", elem), Span::call_site());
    let set_fn_name = Ident::new(&format!("set_{}_color", elem), Span::call_site());

    // The code to check whether the supplied color channel type can be cast
    // into the target type, respecting the specified cast modes. Plus the code
    // to get the new item to be inserted into the map.
    let rounding_allowed = field.allow_cast
        .or_else(|| global_cast_mode.map(|m| m.is_rounding_allowed()))
        .unwrap_or(DEFAULT_COLOR_CAST_ALLOWED);
    let (check_type, new_value) = if rounding_allowed {
        (
            quote! {},
            quote! { lox::prop::ColorLike::cast(&color) },
        )
    } else {
        let check = quote! {
            if !lox::util::are_same_type::<C, <T::Output as lox::prop::ColorLike>::Channel>() {
                return Err(lox::io::Error::SinkIncompatible {
                    prop: lox::io::PropKind::#elem_color,
                    source_type: <C as lox::io::Primitive>::TY,
                });
            }
        };

        let err_msg = format!(
            "`set_{}_color` called with unexpected color channel type '{{:?}}' \
                that cannot be cast to the target color channel type of the sink \
                (this is most likely a bug in the source implementation as the type \
                was not passed to `prepare_{}_colors` before)",
            elem,
            elem,
        );
        let value = quote! {
            lox::prop::ColorLike::map_channel(&color, |c| {
                c.downcast_as().unwrap_or_else(|| panic!(#err_msg, C::TY))
            })
        };

        (check, value)
    };

    // Prepare the code calling the inner functions. These have different spans
    // to improve error messages if the field's type does not satisfy the trait
    // bounds.
    let field_name = &field.name;
    let ty = &field.ty;
    let prep_inner_call = quote_spanned!{ty.span()=>
        _impl::<_, C>(&mut self.#field_name, count, alpha)
    };
    let set_inner_call = quote_spanned!{ty.span()=>
        _impl::<_, C>(&mut self.#field_name, v, color)
    };

    // Combine everything
    quote! {
        fn #prep_fn_name<C: lox::prop::PrimitiveColorChannel + lox::io::Primitive>(
            &mut self,
            count: lox::handle::hsize,
            alpha: bool,
        ) -> Result<(), lox::io::Error> {
            fn _impl<T, C: lox::prop::PrimitiveColorChannel + lox::io::Primitive>(
                map: &mut T,
                count: lox::handle::hsize,
                _alpha: bool,
            ) -> Result<(), lox::io::Error>
            where
                T: lox::map::PropStoreMut<lox::handle::#elem_handle>,
                T::Output: lox::prop::ColorLike,

            {
                // TODO: check alpha channel and return maybe error if mismatch
                #check_type
                map.reserve(count);
                Ok(())
            }

            #prep_inner_call
        }

        fn #set_fn_name<C: lox::prop::PrimitiveColorChannel + lox::io::Primitive>(
            &mut self,
            v: #elem_handle,
            color: lox::io::Color<C>,
        ) {
            fn _impl<T, C: lox::prop::PrimitiveColorChannel + lox::io::Primitive>(
                map: &mut T,
                v: lox::#elem_handle,
                color: lox::io::Color<C>,
            )
            where
                T: lox::map::PropStoreMut<lox::handle::#elem_handle>,
                T::Target: lox::prop::ColorLike,
                <T::Target as lox::prop::ColorLike>::Channel: lox::io::Primitive,
            {
                map.insert(v, #new_value);
            }

            #set_inner_call
        }
    }
}
