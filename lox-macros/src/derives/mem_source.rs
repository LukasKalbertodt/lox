//! Contains function to generate the `impl MemSource` for a type. The main
//! function is `gen_impl`.

#![allow(unused_imports)] // TODO
use proc_macro2::{TokenStream, Span};
use quote::{quote, quote_spanned};
use syn::{
    Ident,
    spanned::Spanned,
};
use super::{
    DEFAULT_COLOR_CAST_ALLOWED, DEFAULT_CAST_MODE,
    input::{CastMode, ColorPropField, CoreMeshField, Input, PropField},
};


/// Generates an `impl MemSource` block for the type defined by `input`. This
/// is the main function for `derive(MemSource)`.
pub(crate) fn gen_impl(input: &Input) -> TokenStream {
    let global_cast_mode = input.cast_mode.as_ref().map(|m| m.mode);

    // Core mesh
    let mesh_code = gen_mesh_code(&input.core_mesh);

    // Vertex properties
    let vertex_position_code = input.vertex_position.as_ref()
        .map(|f| gen_prop_code(f, "Vertex", "Position", "Point3", "Pos3Like", global_cast_mode));
    let vertex_normal_code = input.vertex_normal.as_ref()
        .map(|f| gen_prop_code(f, "Vertex", "Normal", "Vector3", "Vec3Like", global_cast_mode));
    let vertex_color_code = input.vertex_color.as_ref()
        .map(|f| gen_color_prop_code(f, "Vertex", global_cast_mode));

    // Face properties
    let face_normal_code = input.face_normal.as_ref()
        .map(|f| gen_prop_code(f, "Face", "Normal", "Vector3", "Vec3Like", global_cast_mode));
    let face_color_code = input.face_color.as_ref()
        .map(|f| gen_color_prop_code(f, "Face", global_cast_mode));

    // Prepare stuff for impl header.
    let name = &input.name;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    // Combine everything.
    quote! {
        impl #impl_generics lox::io::MemSource for #name #ty_generics #where_clause {
            #mesh_code

            #vertex_position_code
            #vertex_normal_code
            #vertex_color_code
            #face_normal_code
            #face_color_code
        }
    }
}

/// Generates code for the `CoreMesh` type and the `core_mesh` method.
fn gen_mesh_code(field: &CoreMeshField) -> TokenStream {
    let field_name = &field.name;
    let ty = &field.ty;

    // The associated type assignment gets the span of the type to improve the
    // error message when the bound is not satisfied.
    //
    // Currently, the error message is bad either way thanks to #58707.
    let type_def = quote_spanned!(ty.span()=>
        type CoreMesh = #ty;
    );

    quote! {
        #type_def
        fn core_mesh(&self) -> &Self::CoreMesh {
            &self.#field_name
        }
    }
}


/// Generates the code for `{prop}_type` and `{prop}` methods for a given
/// position or normal field.
///
/// The string parameters work as follows (the case is important!):
/// - `elem`: either `"Face"` or `"Vertex"`
/// - `prop`: either `"Position"` or `"Normal"`
/// - `type_name`: `"Point3"` for positions, `"Vector3"` for normals
/// - `trait_name`: `"Pos3Like"` for positions, `"Vec3Like"` for normals
fn gen_prop_code(
    field: &PropField,
    elem: &str,
    prop: &str,
    type_name: &str,
    trait_name: &str,
    global_cast_mode: Option<CastMode>,
) -> TokenStream {
    // Create idents
    let elem_prop = ident!("{}{}", elem, prop);
    let elem_handle = ident!("{}Handle", elem);
    let prop_type_fn = ident!("{}_{}_type", elem.to_lowercase(), prop.to_lowercase());
    let prop_fn = ident!("{}_{}", elem.to_lowercase(), prop.to_lowercase());
    let prop_type = ident!("{}", type_name);
    let trait_name = ident!("{}", trait_name);


    // Generate the code to check whether the source scalar type can be cast
    // into the requested type (respecting the specified cast modes) and to
    // perform that cast if possible.
    let cast_mode = field.cast_mode
        .map(|m| m.mode)
        .or(global_cast_mode)
        .unwrap_or(DEFAULT_CAST_MODE);
    let inner_body = if let Some(cast_rigor) = cast_mode.rigor_tokens() {
        quote! {
            let cast_possible = lox::cast::is_cast_possible::<
                #cast_rigor,
                <M::Target as lox::prop::#trait_name>::Scalar,
                T,
            >();

            if !cast_possible {
                return Err(lox::io::Error::new(|| lox::io::ErrorKind::SourceIncompatibleProp {
                    prop: lox::io::PropKind::#elem_prop,
                    requested_type: T::TY,
                }));
            }

            Ok(
                map.get(v).map(|p| {
                    lox::prop::#trait_name::map_scalar(&*p, |s| {
                        // A few lines above we checked that the cast is indeed
                        // possible. If this fails, there is a bug in lox.
                        lox::cast::try_cast::<#cast_rigor, _, _>(s)
                            .expect("internal bug in `lox::cast` module")
                    })
                })
            )
        }

    } else {
        // "None" cast mode
        quote! {
            if !lox::util::are_same_type::<T, <M::Target as lox::prop::#trait_name>::Scalar>() {
                return Err(lox::io::Error::new(|| lox::io::ErrorKind::SourceIncompatibleProp {
                    prop: lox::io::PropKind::#elem_prop,
                    requested_type: T::TY,
                }));
            }

            Ok(
                map.get(v).map(|p| {
                    lox::prop::#trait_name::map_scalar(&*p, |s| {
                        // A few lines above we checked that the cast is indeed
                        // possible. If this fails, there is a bug in lox.
                        lox::util::downcast_as(s).expect("internal bug in `lox::cast` module")
                    })
                })
            )
        }
    };


    // Prepare the code calling the inner functions. These have different spans
    // to improve error messages if the field's type does not satisfy the trait
    // bounds.
    let field_name = &field.name;
    let field_ty = &field.ty;
    let inner_call = quote_spanned!{field_ty.span()=>
        _impl::<_, T>(&self.#field_name, v)
    };
    let type_fn_body = quote_spanned!{field_ty.span()=>
        <
            <
                <#field_ty as lox::map::PropMap<#elem_handle>>::Target
                    as lox::prop::#trait_name
            >::Scalar
                as lox::io::Primitive
        >::TY
    };

    // Combine everything
    quote! {
        fn #prop_type_fn(&self) -> std::option::Option<lox::io::PrimitiveType> {
            std::option::Option::Some(#type_fn_body)
        }

        fn #prop_fn<T: lox::io::Primitive>(
            &self,
            v: #elem_handle,
        ) -> Result<Option<#prop_type<T>>, lox::io::Error> {
            fn _impl<M, T: lox::io::Primitive>(
                map: &M,
                v: #elem_handle,
            ) -> Result<Option<#prop_type<T>>, lox::io::Error>
            where
                M: lox::map::PropMap<#elem_handle>,
                M::Target: lox::prop::#trait_name,
                <M::Target as lox::prop::#trait_name>::Scalar: lox::io::Primitive,
            {
                #inner_body
            }

            #inner_call
        }
    }
}

/// Generates the code for `{elem}_color_type` and `{elem}_color` methods for a
/// given color field.
///
/// The parameter `elem` as to be one of `"Face"` or `"Vertex"`.
fn gen_color_prop_code(
    field: &ColorPropField,
    elem: &str,
    global_cast_mode: Option<CastMode>,
) -> TokenStream {
    // Create idents
    let elem_prop = ident!("{}Color", elem);
    let elem_handle = ident!("{}Handle", elem);
    let prop_color_fn = ident!("{}_color_type", elem.to_lowercase());
    let prop_fn = ident!("{}_color", elem.to_lowercase());


    // Generate the code to check whether the source color type can be cast
    // into the requested type (respecting the specified cast modes) and to
    // perform that cast if possible.
    let rounding_allowed = field.allow_cast
        .or_else(|| global_cast_mode.map(|m| m.is_rounding_allowed()))
        .unwrap_or(DEFAULT_COLOR_CAST_ALLOWED);
    let inner_body = if rounding_allowed {
        // Just cast it via `ColorLike::cast`
        quote! {
            Ok(map.get(handle).map(|c| lox::prop::ColorLike::cast(&*c)))
        }
    } else {
        quote! {
            let is_same_type = lox::util::are_same_type::<
                <C as lox::prop::ColorLike>::Channel,
                <M::Target as lox::prop::ColorLike>::Channel,
            >();

            if !is_same_type {
                return Err(lox::io::Error::new(|| lox::io::ErrorKind::SourceIncompatibleProp {
                    prop: lox::io::PropKind::#elem_prop,
                    requested_type: <
                        <C as lox::prop::ColorLike>::Channel
                            as lox::io::Primitive
                    >::TY,
                }));
            }

            Ok(
                map.get(handle).map(|color| {
                    lox::prop::ColorLike::map_channel(&*color, |c| {
                        // A few lines above we checked that the cast is indeed
                        // possible. If this fails, there is a bug in lox.
                        lox::util::downcast_as(c).expect("internal bug in `lox::cast` module")
                    })
                })
            )
        }
    };


    // Prepare the code calling the inner functions. These have different spans
    // to improve error messages if the field's type does not satisfy the trait
    // bounds.
    let field_name = &field.name;
    let field_ty = &field.ty;
    let inner_call = quote_spanned!{field_ty.span()=>
        _impl::<_, C>(&self.#field_name, handle)
    };
    let type_fn_body = quote_spanned!{field_ty.span()=>
        lox::io::ColorType::from_color_like::<
            <#field_ty as lox::map::PropMap<#elem_handle>>::Target
        >()
    };

    // Combine everything
    quote! {
        fn #prop_color_fn(&self) -> Option<lox::io::ColorType> {
            std::option::Option::Some(#type_fn_body)
        }

        fn #prop_fn<C>(&self, handle: #elem_handle) -> Result<Option<C>, lox::io::Error>
        where
            C: lox::prop::ColorLike,
            C::Channel: lox::io::Primitive,
        {
            fn _impl<M, C>(
                map: &M,
                handle: #elem_handle,
            ) -> Result<Option<C>, lox::io::Error>
            where
                M: lox::map::PropMap<#elem_handle>,
                M::Target: lox::prop::ColorLike,
                <M::Target as lox::prop::ColorLike>::Channel: lox::io::Primitive,
                C: lox::prop::ColorLike,
                C::Channel: lox::io::Primitive,
            {
                #inner_body
            }

            #inner_call
        }
    }
}
