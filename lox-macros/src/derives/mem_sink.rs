//! Contains function to generate the `impl MemSink` for a type. Main function
//! is `gen_impl`.

use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{
    Ident,
    spanned::Spanned,
};
use super::{
    DEFAULT_COLOR_CAST_ALLOWED, DEFAULT_CAST_MODE,
    input::{CastMode, ColorPropField, CoreMeshField, Input, PropField},
};


/// Generates an `impl MemSink` block for the type defined by `input`. This is
/// the main function for `derive(MemSink)`.
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
    let face_color_code = input.face_color.as_ref()
        .map(|f| gen_color_prop_code(f, "Face", global_cast_mode));
    let face_normal_code = input.face_normal.as_ref()
        .map(|f| gen_prop_code(f, "Face", "Normal", "Vector3", "Vec3Like", global_cast_mode));

    // Edge properties
    let edge_color_code = input.edge_color.as_ref()
        .map(|f| gen_color_prop_code(f, "Edge", global_cast_mode));

    // The `finish()` method
    let finish_code = gen_finish_code(input);

    // Prepare stuff for impl header.
    let name = &input.name;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    // Combine everything.
    quote! {
        impl #impl_generics lox::io::MemSink for #name #ty_generics #where_clause {
            #mesh_code
            #finish_code

            #vertex_position_code
            #vertex_normal_code
            #vertex_color_code
            #face_normal_code
            #face_color_code
            #edge_color_code
        }
    }
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
        lox::io::util::try_add_face(&mut self.#field_name, vertices)
    };
    let get_edge_between = quote_spanned!{field.ty.span()=>
        lox::io::util::try_get_edge_between(&self.#field_name, endpoints)
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
        fn add_face(
            &mut self,
            vertices: &[lox::VertexHandle],
        ) -> Result<lox::FaceHandle, lox::io::Error> {
            #add_face
        }
        fn get_edge_between(
            &self,
            endpoints: [lox::VertexHandle; 2],
        ) -> Result<Option<lox::EdgeHandle>, lox::io::Error> {
            #get_edge_between
        }
        fn size_hint(&mut self, hint: lox::util::MeshSizeHint) {
            #size_hint
        }
    }
}

/// Generates the code for `finish()`.
fn gen_finish_code(input: &Input) -> TokenStream {
    fn gen_check(field_name: &Ident, prop: &str, expected: &Ident) -> TokenStream {
        let prop_kind = format_ident!("{}", prop);
        let err_msg = "{} provided, {} expected";
        quote! {
            // We only have to check for the case where the number of
            // properties is less than the number of elements in the mesh. It
            // can't be more because the source can only add properties for
            // each handle returned by the mesh.
            if lox::map::PropStore::num_props(&self.#field_name) < #expected {
                let msg = format!(
                    #err_msg,
                    lox::map::PropStore::num_props(&self.#field_name),
                    #expected,
                );
                return Err(lox::io::Error::new(|| lox::io::ErrorKind::DataIncomplete {
                    prop: lox::io::PropKind::#prop_kind,
                    msg,
                }));
            }
        }
    }


    // Generate on check for each field that is present
    let num_vertices = format_ident!("_num_vertices");
    let num_faces = format_ident!("_num_faces");

    let vertex_position = input.vertex_position.as_ref()
        .map(|f| gen_check(&f.name, "VertexPosition", &num_vertices));
    let vertex_normal = input.vertex_normal.as_ref()
        .map(|f| gen_check(&f.name, "VertexNormal", &num_vertices));
    let vertex_color = input.vertex_color.as_ref()
        .map(|f| gen_check(&f.name, "VertexColor", &num_vertices));
    let face_normal = input.face_normal.as_ref()
        .map(|f| gen_check(&f.name, "FaceNormal", &num_faces));
    let face_color = input.face_color.as_ref()
        .map(|f| gen_check(&f.name, "FaceColor", &num_faces));

    // Combine everything
    let mesh_field_name = &input.core_mesh.name;
    quote! {
        fn finish(&mut self) -> Result<(), lox::io::Error> {
            // We start the names with underscores in case the sink doesn't
            // have any properties (for one elem) which would mean no checking
            // code is generated which would cause a "unused variable" warning.
            let _num_vertices = lox::traits::Mesh::num_vertices(&self.#mesh_field_name);
            let _num_faces = lox::traits::Mesh::num_faces(&self.#mesh_field_name);

            #vertex_position
            #vertex_normal
            #vertex_color
            #face_normal
            #face_color

            Ok(())
        }
    }
}

/// Generates the code for `prepare_*` and `set_*` for a given position or
/// normal field.
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
    let elem_prop = format_ident!("{}{}", elem, prop);
    let elem_handle = format_ident!("{}Handle", elem);
    let prep_fn_name = format_ident!("prepare_{}_{}s", elem.to_lowercase(), prop.to_lowercase());
    let set_fn_name = format_ident!("set_{}_{}", elem.to_lowercase(), prop.to_lowercase());
    let prop_type = format_ident!("{}", type_name);
    let trait_name = format_ident!("{}", trait_name);


    // Generate the code to check whether the supplied scalar type can be cast
    // into the target type, respecting the specified cast modes. Plus the code
    // to get the new item to be inserted into the map.
    let cast_mode = field.cast_mode
        .map(|m| m.mode)
        .or(global_cast_mode)
        .unwrap_or(DEFAULT_CAST_MODE);
    let (check_code, new_elem_code) = if let Some(cast_rigor) = cast_mode.rigor_tokens() {
        let check = quote! {
            let cast_possible = lox::cast::is_cast_possible::<
                #cast_rigor,
                N,
                <T::Output as lox::prop::#trait_name>::Scalar,
            >();

            if !cast_possible {
                return Err(lox::io::Error::new(|| lox::io::ErrorKind::SinkIncompatibleProp {
                    prop: lox::io::PropKind::#elem_prop,
                    source_type: N::TY,
                }));
            }
        };

        let err_msg = format!(
            "`{}` called with unexpected primitive type '{{:?}}' \
                that cannot be cast (in cast mode \"{}\") to the target type of the sink \
                (this is most likely a bug in the source implementation as the type \
                was not passed to `{}` before)",
            set_fn_name,
            cast_mode.to_str(),
            prep_fn_name,
        );
        let elem = quote! {
            lox::prop::#trait_name::convert(
                &position.map(|s| {
                    lox::cast::try_cast::<#cast_rigor, _, _>(s)
                        .unwrap_or_else(|| panic!(#err_msg, N::TY))
                })
            )
        };

        (check, elem)
    } else {
        // "None" cast mode
        let check = quote! {
            if !lox::util::are_same_type::<N, <T::Output as lox::prop::#trait_name>::Scalar>() {
                return Err(lox::io::Error::new(|| lox::io::ErrorKind::SinkIncompatibleProp {
                    prop: lox::io::PropKind::#elem_prop,
                    source_type: <N as lox::io::Primitive>::TY,
                }));
            }
        };

        let err_msg = format!(
            "`set_vertex_position` called with unexpected type '{{:?}}' \
                that cannot be cast to the target type of the sink \
                (this is most likely a bug in the source implementation as the type \
                was not passed to `prepare_vertex_positions` before)",
        );
        let elem = quote! {
            lox::prop::#trait_name::map_scalar(&position, |s| {
                *<dyn std::any::Any>::downcast_ref(&s).unwrap_or_else(|| panic!(#err_msg, N::TY))
            })
        };

        (check, elem)
    };


    // Prepare the code calling the inner functions. These have different spans
    // to improve error messages if the field's type does not satisfy the trait
    // bounds.
    let field_name = &field.name;
    let ty = &field.ty;
    let prep_inner_call = quote_spanned!{ty.span()=>
        _impl::<_, N>(&mut self.#field_name, count)
    };
    let set_inner_call = quote_spanned!{ty.span()=>
        _impl::<_, N>(&mut self.#field_name, v, position)
    };

    // Combine everything
    quote! {
        fn #prep_fn_name<N: lox::io::Primitive>(
            &mut self,
            count: lox::handle::hsize,
        ) -> Result<(), lox::io::Error> {
            fn _impl<T, N: lox::io::Primitive>(
                map: &mut T,
                count: lox::handle::hsize,
            ) -> Result<(), lox::io::Error>
            where
                T: lox::map::PropStoreMut<lox::handle::#elem_handle>,
                T::Target: lox::prop::#trait_name,
            {
                #check_code
                map.reserve(count);
                Ok(())
            }

            #prep_inner_call
        }

        fn #set_fn_name<N: lox::io::Primitive>(
            &mut self,
            v: lox::#elem_handle,
            position: lox::cgmath::#prop_type<N>,
        ) {
            fn _impl<T, N: lox::io::Primitive>(
                map: &mut T,
                v: lox::#elem_handle,
                position: lox::cgmath::#prop_type<N>,
            )
            where
                T: lox::map::PropStoreMut<lox::handle::#elem_handle>,
                T::Target: lox::prop::#trait_name,
            {
                map.insert(v, #new_elem_code);
            }

            #set_inner_call
        }
    }
}

/// Generates the code for `prepare_*_colors` and `set_*_color` for a given
/// field.
///
/// The `elem` string has to be either `"Face"`, `"Vertex"` or `"Edge"` -- the
/// first character has to be uppercase.
fn gen_color_prop_code(
    field: &ColorPropField,
    elem: &str,
    global_cast_mode: Option<CastMode>,
) -> TokenStream {
    // Create idents
    let elem_color = format_ident!("{}Color", elem);
    let elem_handle = format_ident!("{}Handle", elem);
    let elem = elem.to_lowercase();
    let prep_fn_name = format_ident!("prepare_{}_colors", elem);
    let set_fn_name = format_ident!("set_{}_color", elem);

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
            let is_same_type = lox::util::are_same_type::<
                C::Channel,
                <T::Output as lox::prop::ColorLike>::Channel,
            >();

            if !is_same_type {
                return Err(lox::io::Error::new(|| lox::io::ErrorKind::SinkIncompatibleProp {
                    prop: lox::io::PropKind::#elem_color,
                    source_type: <C::Channel as lox::io::Primitive>::TY,
                }));
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
                *<dyn std::any::Any>::downcast_ref(&c).unwrap_or_else(|| panic!(#err_msg, C::TY))
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
        _impl::<_, C>(&mut self.#field_name, count)
    };
    let set_inner_call = quote_spanned!{ty.span()=>
        _impl::<_, C>(&mut self.#field_name, v, color)
    };

    // Combine everything
    quote! {
        fn #prep_fn_name<C>(
            &mut self,
            count: lox::handle::hsize,
        ) -> Result<(), lox::io::Error>
        where
            C: lox::prop::ColorLike,
            C::Channel: lox::io::Primitive,
        {
            fn _impl<T, C>(
                map: &mut T,
                count: lox::handle::hsize,
            ) -> Result<(), lox::io::Error>
            where
                T: lox::map::PropStoreMut<lox::handle::#elem_handle>,
                T::Output: lox::prop::ColorLike,
                C: lox::prop::ColorLike,
                C::Channel: lox::io::Primitive,
            {
                // TODO: check alpha channel and return maybe error if mismatch
                #check_type
                map.reserve(count);
                Ok(())
            }

            #prep_inner_call
        }

        fn #set_fn_name<C>(
            &mut self,
            v: #elem_handle,
            color: C,
        )
        where
            C: lox::prop::ColorLike,
            C::Channel: lox::io::Primitive,
        {
            fn _impl<T, C>(
                map: &mut T,
                v: lox::#elem_handle,
                color: C,
            )
            where
                T: lox::map::PropStoreMut<lox::handle::#elem_handle>,
                T::Target: lox::prop::ColorLike,
                <T::Target as lox::prop::ColorLike>::Channel: lox::io::Primitive,
                C: lox::prop::ColorLike,
                C::Channel: lox::io::Primitive,
            {
                map.insert(v, #new_value);
            }

            #set_inner_call
        }
    }
}
