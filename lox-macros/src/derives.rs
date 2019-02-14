use std::{
    collections::HashMap,
    fmt,
};

use quote::{quote, quote_spanned};
use proc_macro2::TokenStream as TokenStream2;
use syn::{
    DeriveInput, Error, Fields,
    spanned::Spanned,
};

use crate::util::struct_fields;


const DEFAULT_CAST_MODE: CastMode = CastMode::Lossless;


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

/// Implementation of `derive(MemSink)`.
///
/// In order to improve error messages if the type of a field is unfitting, we
/// use some tricks here. All methods of `MemSink` are implemented by defining
/// an inner function `_impl` that does the main work. This inner function is
/// then called in the outer function. That allows us to explicitly annotate
/// the trait bounds for the fields on the inner function. Thus, the call to
/// the inner function results in a classic "the trait bound ... is not
/// satisfied" error which is easier to understand than errors coming from code
/// that already assumes the trait bound is implemented (that's basically why
/// C++ error messages are bad).
///
/// Furthermore, the call to the inner function is created to contain the span
/// of the field type. Thus, the error message will point to the type of the
/// field which makes it even easier to understand.
pub(crate) fn derive_mem_sink(input: &DeriveInput) -> Result<TokenStream2, Error> {
    const ERR_NO_MESH: &str = "no field named `mesh` or with attribute `#[lox(mesh)]` found";

    fn cast_error(cast_mode: CastMode, prop_name: &str) -> String {
        format!(
            "`set_{}` called with unexpected primitive type '{{:?}}' \
                that cannot be casted as `{}` to the target type of the sink \
                (this is most likely a bug in the source implementation as the type \
                was not passed to `prepare_{}s` before)",
            prop_name,
            cast_mode,
            prop_name,
        )
    }

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
    let mesh_field = find_field(fields, "mesh")?.ok_or(Error::new(input.span(), ERR_NO_MESH))?;
    let mesh_code = {
        let field_name = &mesh_field.name;
        if mesh_field.cast_mode.is_some() {
            return Err(Error::new(
                mesh_field.name.span(),
                "specifying casting behavior on the `mesh` field does not make sense",
            ));
        }

        let add_vertex = quote_spanned!{mesh_field.ty.span()=>
            lox::traits::MeshMut::add_vertex(&mut self.#field_name)
        };
        let add_face = quote_spanned!{mesh_field.ty.span()=>
            lox::traits::TriMeshMut::add_face(&mut self.#field_name, vertices)
        };
        let size_hint = quote_spanned!{mesh_field.ty.span()=>
            lox::traits::MeshMut::reserve_for_vertices(
                &mut self.#field_name,
                hint.guess_vertex_count(),
            );
            lox::traits::MeshMut::reserve_for_faces(
                &mut self.#field_name,
                hint.guess_face_count(),
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
    };


    // ===== Vertex positions =====
    let vertex_position_field = find_field(fields, "vertex_positions")?;
    let vertex_position_code = {
        if let Some(field) = &vertex_position_field {
            let cast_mode = field.cast_mode.unwrap_or(DEFAULT_CAST_MODE);
            let cast_rigor = cast_mode.rigor_tokens();
            let cast_error = cast_error(cast_mode, "vertex_position");

            let field_name = &field.name;
            let ty = &field.ty;
            let prep_inner_call = quote_spanned!{ty.span()=>
                _impl::<_, N>(&mut self.#field_name, count)
            };
            let set_inner_call = quote_spanned!{ty.span()=>
                _impl::<_, N>(&mut self.#field_name, v, position)
            };

            quote! {
                // TODO: add this back in. Currently, this is not possible
                //       because associated type defaults are a bit broken.
                // type VertexPosition = lox::io::util::OverwriteFor<
                //     <
                //         <#ty as std::ops::Index<lox::VertexHandle>>::Output as lox::math::Pos3Like
                //     >::Scalar
                // >;

                fn prepare_vertex_positions<N: lox::io::Primitive>(
                    &mut self,
                    count: lox::handle::DefaultInt,
                ) -> Result<(), lox::io::Error> {
                    fn _impl<T, N: lox::io::Primitive>(
                        map: &mut T,
                        count: lox::handle::DefaultInt,
                    ) -> Result<(), lox::io::Error>
                    where
                        T: lox::map::PropStoreMut<lox::handle::VertexHandle>,
                        T::Output: lox::math::Pos3Like,
                    {
                        let cast_possible = lox::cast::is_cast_possible::<
                            #cast_rigor,
                            N,
                            <T::Output as Pos3Like>::Scalar,
                        >();

                        if !cast_possible {
                            return Err(lox::io::Error::SinkIncompatible {
                                prop: lox::io::PropKind::VertexPosition,
                                source_type: N::TY,
                            });
                        }

                        map.reserve(count);

                        Ok(())
                    }

                    #prep_inner_call
                }

                fn set_vertex_position<N: lox::io::Primitive>(
                    &mut self,
                    v: lox::VertexHandle,
                    position: lox::cgmath::Point3<N>,
                ) {
                    fn _impl<T, N: lox::io::Primitive>(
                        map: &mut T,
                        v: lox::VertexHandle,
                        position: lox::cgmath::Point3<N>,
                    )
                    where
                        T: lox::map::PropStoreMut<lox::handle::VertexHandle>,
                        T::Output: lox::math::Pos3Like,
                    {
                        let pos = position.map(|s| {
                            lox::cast::try_cast::<#cast_rigor, _, _>(s)
                                .unwrap_or_else(|| panic!(#cast_error, N::TY))
                        });
                        map.insert(v, pos.convert());
                    }

                    #set_inner_call
                }
            }
        } else {
            quote! {}
        }
    };

    // ===== Vertex normals =====
    let vertex_normal_field = find_field(fields, "vertex_normals")?;
    let vertex_normal_code = {
        if let Some(field) = &vertex_normal_field {
            let cast_mode = field.cast_mode.unwrap_or(DEFAULT_CAST_MODE);
            let cast_rigor = cast_mode.rigor_tokens();
            let cast_error = cast_error(cast_mode, "vertex_normal");

            let field_name = &field.name;
            let ty = &field.ty;
            let prep_inner_call = quote_spanned!{ty.span()=>
                _impl::<_, N>(&mut self.#field_name, count)
            };
            let set_inner_call = quote_spanned!{ty.span()=>
                _impl::<_, N>(&mut self.#field_name, v, position)
            };

            quote! {
                // TODO: type wish
                fn prepare_vertex_normals<N: lox::io::Primitive>(
                    &mut self,
                    count: lox::handle::DefaultInt,
                ) -> Result<(), lox::io::Error> {
                    fn _impl<T, N: lox::io::Primitive>(
                        map: &mut T,
                        count: lox::handle::DefaultInt,
                    ) -> Result<(), lox::io::Error>
                    where
                        T: lox::map::PropStoreMut<lox::handle::VertexHandle>,
                        T::Output: lox::math::Vec3Like,
                    {
                        let cast_possible = lox::cast::is_cast_possible::<
                            #cast_rigor,
                            N,
                            <T::Output as Vec3Like>::Scalar,
                        >();

                        if !cast_possible {
                            return Err(lox::io::Error::SinkIncompatible {
                                prop: lox::io::PropKind::VertexPosition,
                                source_type: N::TY,
                            });
                        }

                        map.reserve(count);

                        Ok(())
                    }

                    #prep_inner_call
                }

                fn set_vertex_normal<N: lox::io::Primitive>(
                    &mut self,
                    v: lox::VertexHandle,
                    position: lox::cgmath::Vector3<N>,
                ) {
                    fn _impl<T, N: lox::io::Primitive>(
                        map: &mut T,
                        v: lox::VertexHandle,
                        position: lox::cgmath::Vector3<N>,
                    )
                    where
                        T: lox::map::PropStoreMut<lox::handle::VertexHandle>,
                        T::Output: lox::math::Vec3Like,
                    {
                        let pos = position.map(|s| {
                            lox::cast::try_cast::<#cast_rigor, _, _>(s)
                                .unwrap_or_else(|| panic!(#cast_error, N::TY))
                        });
                        map.insert(v, pos.convert());
                    }

                    #set_inner_call
                }
            }
        } else {
            quote! {}
        }
    };

    // ===== Face normals =====
    let face_normal_field = find_field(fields, "face_normals")?;
    let face_normal_code = {
        if let Some(field) = &face_normal_field {
            let cast_mode = field.cast_mode.unwrap_or(DEFAULT_CAST_MODE);
            let cast_rigor = cast_mode.rigor_tokens();
            let cast_error = cast_error(cast_mode, "face_normal");

            let field_name = &field.name;
            let ty = &field.ty;
            let prep_inner_call = quote_spanned!{ty.span()=>
                _impl::<_, N>(&mut self.#field_name, count)
            };
            let set_inner_call = quote_spanned!{ty.span()=>
                _impl::<_, N>(&mut self.#field_name, v, position)
            };

            quote! {
                // TODO: type wish
                fn prepare_face_normals<N: lox::io::Primitive>(
                    &mut self,
                    count: lox::handle::DefaultInt,
                ) -> Result<(), lox::io::Error> {
                    fn _impl<T, N: lox::io::Primitive>(
                        map: &mut T,
                        count: lox::handle::DefaultInt,
                    ) -> Result<(), lox::io::Error>
                    where
                        T: lox::map::PropStoreMut<lox::handle::FaceHandle>,
                        T::Output: lox::math::Vec3Like,
                    {
                        let cast_possible = lox::cast::is_cast_possible::<
                            #cast_rigor,
                            N,
                            <T::Output as Vec3Like>::Scalar,
                        >();

                        if !cast_possible {
                            return Err(lox::io::Error::SinkIncompatible {
                                prop: lox::io::PropKind::VertexPosition,
                                source_type: N::TY,
                            });
                        }

                        map.reserve(count);

                        Ok(())
                    }

                    #prep_inner_call
                }

                fn set_face_normal<N: lox::io::Primitive>(
                    &mut self,
                    v: lox::FaceHandle,
                    position: lox::cgmath::Vector3<N>,
                ) {
                    fn _impl<T, N: lox::io::Primitive>(
                        map: &mut T,
                        v: lox::FaceHandle,
                        position: lox::cgmath::Vector3<N>,
                    )
                    where
                        T: lox::map::PropStoreMut<lox::handle::FaceHandle>,
                        T::Output: lox::math::Vec3Like,
                    {
                        let pos = position.map(|s| {
                            lox::cast::try_cast::<#cast_rigor, _, _>(s)
                                .unwrap_or_else(|| panic!(#cast_error, N::TY))
                        });
                        map.insert(v, pos.convert());
                    }

                    #set_inner_call
                }
            }
        } else {
            quote! {}
        }
    };


    // ===== The `finish()` method =====
    let finish_code = {
        macro_rules! gen_prop_check {
            ($field:ident, $name:literal, $count:ident) => {
                if let Some(field) = $field {
                    let name = &field.name;
                    let err_msg = concat!("missing ", $name, " ({} provided, {} expected)");
                    quote! {
                        if lox::map::PropStore::num_props(&self.#name) != $count {
                            let msg = format!(
                                #err_msg,
                                lox::map::PropStore::num_props(&self.#name),
                                $count,
                            );
                            return Err(lox::io::Error::DataIncomplete(msg));
                        }
                    }
                } else {
                    quote! {}
                }
            }
        }

        let mesh_field_name = &mesh_field.name;
        let vpos = gen_prop_check!(vertex_position_field, "vertex positions", _num_vertices);
        let vnormal = gen_prop_check!(vertex_normal_field, "vertex normals", _num_vertices);
        let fnormal = gen_prop_check!(face_normal_field, "face normals", _num_faces);

        quote! {
            fn finish(&mut self) -> Result<(), lox::io::Error> {
                let _num_vertices = lox::traits::Mesh::num_vertices(&self.#mesh_field_name);
                let _num_faces = lox::traits::Mesh::num_faces(&self.#mesh_field_name);

                #vpos
                #vnormal
                #fnormal

                Ok(())
            }
        }
    };


    // Prepare stuff for impl header.
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    // Combine everything.
    let out = quote! {
        impl #impl_generics lox::io::MemSink for #name #ty_generics #where_clause {
            #mesh_code
            #finish_code
            #vertex_position_code
            #vertex_normal_code
            #face_normal_code
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
    values: HashMap<String, LoxAttrValue>,
}

struct LoxAttrValue {
    #[allow(dead_code)] // TODO
    name: syn::Ident,
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
            // `lox(foo)`
            syn::NestedMeta::Meta(syn::Meta::Word(w)) => (w, HashMap::new()),

            // `lox(foo(...))`
            syn::NestedMeta::Meta(syn::Meta::List(l)) => {
                let mut values = HashMap::new();
                for nested in &l.nested {
                    match nested {
                        syn::NestedMeta::Meta(syn::Meta::NameValue(v)) => {
                            if values.contains_key(&v.ident.to_string()) {
                                return Err(Error::new(
                                    v.ident.span(),
                                    format!("duplicate key '{}'", v.ident),
                                ));
                            }

                            values.insert(v.ident.to_string(), LoxAttrValue {
                                name: v.ident.clone(),
                                value: v.lit.clone(),
                            });
                        }
                        _ => {
                            return Err(Error::new(
                                nested.span(),
                                "expected named value, found something else",
                            ));
                        }
                    }
                }

                (&l.ident, values)
            }

            // `lox(foo = lit)`
            syn::NestedMeta::Meta(syn::Meta::NameValue(v)) => {
                return Err(Error::new(
                    v.span(),
                    "expected ident, found named value",
                ));
            }

            // `lox("hi")`
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

#[derive(Debug, Clone, Copy)]
enum CastMode {
    Lossless,
    Clamping,
    Rounding,
    Lossy,
    // TODO: add "none" casting mode
}

impl CastMode {
    fn rigor_tokens(&self) -> TokenStream2 {
        match self {
            CastMode::Lossless => quote! { lox::cast::Lossless },
            CastMode::Clamping => quote! { lox::cast::AllowClamping },
            CastMode::Rounding => quote! { lox::cast::AllowRounding },
            CastMode::Lossy => quote! { lox::cast::Lossy },
        }
    }
}

impl fmt::Display for CastMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CastMode::Lossless => "lossless",
            CastMode::Clamping => "clamping",
            CastMode::Rounding => "rounding",
            CastMode::Lossy => "lossy",
        }.fmt(f)
    }
}

#[derive(Clone)]
struct FoundField {
    name: syn::Ident,
    ty: syn::Type,
    cast_mode: Option<CastMode>,
}

/// Searches for fields either named `needle` or that do have a lox attribute
/// `needle`. Attributes win over names, so if both exist, the one with the
/// attribute is returned.
fn find_field(
    fields: &syn::Fields,
    needle: &str,
) -> Result<Option<FoundField>, Error> {
    // Find the field with ident `needle`.
    let field_with_name = fields.iter()
        .find(|f| f.ident.as_ref().map(|i| i == needle).unwrap_or(false))
        .map(|f| {
            FoundField {
                name: f.ident.clone().unwrap(),
                ty: f.ty.clone(),

                // If the field also has an attribute specifying the cast type,
                // it is found below and will have precedence over this result
                // anyway.
                cast_mode: None,
            }
        });

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
            if let Some(attr) = lox_attrs.get(needle) {
                let cast_mode = parse_cast_attr(attr.values.get("cast"))?;

                fields_with_attr.push(FoundField {
                    name: f.ident.clone().unwrap(),
                    ty: f.ty.clone(),
                    cast_mode,
                });
            }
        }
    }

    // Make sure the attribute is only attached to one field
    if fields_with_attr.len() > 1 {
        return Err(Error::new(
            fields_with_attr[1].name.span(),
            format!("more than one field with '{}' attribute", needle),
        ));
    }

    // If there is a field that is explicitly marked with the attribute `attr`,
    // we return that. Otherwise we return the field with the fitting name, if
    // that exists.
    Ok(fields_with_attr.get(0).cloned().or(field_with_name))
}

/// Parses the value of the `cast` attribute to get the cast mode.
fn parse_cast_attr(v: Option<&LoxAttrValue>) -> Result<Option<CastMode>, Error> {
    match v.map(|v| &v.value) {
        None => Ok(None),
        Some(syn::Lit::Str(slit)) => {
            match slit.value().as_str() {
                "lossless" => Ok(Some(CastMode::Lossless)),
                "clamping" => Ok(Some(CastMode::Clamping)),
                "rounding" => Ok(Some(CastMode::Rounding)),
                "lossy" => Ok(Some(CastMode::Lossy)),
                other => {
                    Err(Error::new(
                        slit.span(),
                        format!("unknown casting mode '{}'", other),
                    ))
                }
            }
        }
        Some(lit) => {
            Err(Error::new(
                lit.span(),
                "expected string literal, found something else",
            ))
        }
    }
}

/// Returns `true` **iff** the given fields are named fields.
fn are_named(fields: &syn::Fields) -> bool {
    match fields {
        Fields::Named(_) => true,
        _ => false,
    }
}
