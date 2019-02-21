#[allow(unused_imports)] // TODO
use proc_macro2::{
    TokenStream, Span,
};
use syn::{
    Attribute, DeriveInput, Error, Fields, Ident, Type, Meta, NestedMeta, Lit,
    spanned::Spanned,
};


#[derive(Debug, Clone, Copy)]
#[allow(dead_code)] // TODO
pub(crate) enum CastMode {
    None,
    Lossless,
    Clamping,
    Rounding,
    Lossy,
}

impl CastMode {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "none" => Some(CastMode::None),
            "lossless" => Some(CastMode::Lossless),
            "clamping" => Some(CastMode::Clamping),
            "rounding" => Some(CastMode::Rounding),
            "lossy" => Some(CastMode::Lossy),
            _ => None
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct SpannedCastMode {
    span: Span,
    mode: CastMode,
}


#[derive(Debug)]
pub(crate) struct Field {
    cast_mode: SpannedCastMode,
    span: Span,
    name: Option<Ident>,
    ty: Type,
}

#[derive(Debug)]
#[allow(dead_code)] // TODO
pub(crate) struct Input {
    cast_mode: Option<SpannedCastMode>,
    name: Ident,
    fields: Vec<Field>,
}

impl Input {
    pub(crate) fn from_syn(input: &DeriveInput, derive: &str) -> Result<Self, Error> {
        // // Make sure the input is a struct and extract the fields from it
        // let fields = match &input.data {
        //     syn::Data::Struct(s) => &s.fields,
        //     _ => bail!(input.span(), "only structs can derive `{}`", derive),
        // };

        // let mut fields_with_attr = Vec::new();
        // for f in fields {
        //     // Find all attributes that start with `lox`
        //     let lox_attrs = f.attrs.iter()
        //         .filter(|attr| attr.path.is_ident("lox"))
        //         .collect::<Vec<_>>();

        //     // Make sure there is only one such attribute
        //     if lox_attrs.len() > 1 {
        //         return Err(Error::new(
        //             lox_attrs[1].span(),
        //             "more than one `lox` attribute on field",
        //         ));
        //     }

        //     // If there is a `lox` attribute, let's check it for validity and check
        //     // if it contains `needle`.
        //     if lox_attrs.len() == 1 {
        //         let lox_attrs = parse_lox_attrs(&lox_attrs[0])?;
        //         if let Some(attr) = lox_attrs.get(needle) {
        //             let cast_mode = parse_cast_attr(attr.values.get("cast"))?;

        //             fields_with_attr.push(FoundField {
        //                 name: f.ident.clone().unwrap(),
        //                 ty: f.ty.clone(),
        //                 cast_mode,
        //             });
        //         }
        //     }
        // }

        // visit_lox_attrs(&input.attrs, |ident, lit| {
        //     println!("{} = {:?}", ident, lit);
        // })?;
        let struct_attrs = parse_struct_attrs(&input.attrs)?;

        Ok(Input {
            cast_mode: struct_attrs.cast_mode,
            name: input.ident.clone(),
            fields: vec![],
        })
    }
}

/// All attributes we accept on a struct directly.
struct StructAttrs {
    cast_mode: Option<SpannedCastMode>
}

/// Parse the attributes of a struct.
fn parse_struct_attrs(attrs: &[Attribute]) -> Result<StructAttrs, Error> {
    let mut out = StructAttrs {
        cast_mode: None,
    };

    visit_lox_attrs(attrs, |ident, lit| {
        match () {
            // ===== 'cast' attribute =====
            () if ident == "cast" => {
                let lit = match lit {
                    Some(Lit::Str(lit)) => lit,
                    Some(lit) => bail!(
                        lit.span(),
                        "expected string literal (e.g. `\"lossy\"`), found other kind of literal",
                    ),
                    None => bail!(ident.span(), "expected value (e.g. `cast = \"lossy\"`)"),
                };

                let value = lit.value();
                match CastMode::from_str(&value) {
                    Some(mode) => {
                        if out.cast_mode.is_some() {
                            bail!(lit.span(), "duplicate `cast` attribute");
                        }

                        out.cast_mode = Some(SpannedCastMode {
                            span: lit.span(),
                            mode,
                        });
                    }
                    None => bail!(
                        lit.span(),
                        "invalid cast mode \"{}\" (allowed modes: \"none\", \"lossless\", \
                            \"rounding\", \"clamping\" and \"lossy\")",
                        value,
                    ),
                }
            }

            // ===== Unknown attribute =====
            _ => bail!(
                ident.span(),
                "'{}' is not a valid lox attribute for a struct (maybe you wanted to \
                    put this attribute on a field?)",
                ident,
            ),
        }

        Ok(())
    })?;

    Ok(out)
}

/// Parses and visits all valid `lox` attributes.
///
/// LOX Attributes have the form `#[lox($items)]` where `$items` is a comma
/// separated list of items. An item can be a simple identifier (e.g. `foo`) or
/// a named value (e.g. `foo = true`). The visitor is called for each item in
/// any of the lox attributes in the given attributes. Invalid syntax leads to
/// errors.
///
/// Examples:
///
/// - `#[lox()]`: doesn't call the visitor, but is valid
/// - `#[lox(foo)]`: calls the visitor with `(#foo, None)`
/// - `#[lox(foo = true)]`: calls the visitor with `(#foo, Some(#true))`
/// - `#[lox(foo = true, bar)]`: calls the visitor with `(#foo, Some(#true))`
///   and then with `(#bar, None)`.
fn visit_lox_attrs<F>(attrs: &[Attribute], mut visit: F) -> Result<(), Error>
where
    F: FnMut(Ident, Option<Lit>) -> Result<(), Error>,
{
    for attr in attrs.iter().filter(|attr| attr.path.is_ident("lox")) {
        // Interpret it as "meta list" and get the items or error
        let items = match attr.parse_meta()? {
            Meta::Word(_) => {
                bail!(attr.span(), "empty `lox` attribute (only `#[lox(...)]` is allowed)");
            }
            Meta::NameValue(_) => {
                bail!(attr.span(), "value-like `lox` attribute (only `#[lox(...)]` is allowed)");
            }
            Meta::List(l) => l.nested,
        };

        // For each item, we check if it has a correct form and pass it to the
        // visitor.
        for item in items {
            match item {
                NestedMeta::Literal(lit) => {
                    bail!(
                        lit.span(),
                        "found literal, expected ident (`foo`) or named value (`foo = true`)",
                    );
                }
                NestedMeta::Meta(Meta::List(list)) => {
                    bail!(
                        list.span(),
                        "found nested list, expected ident (`foo`) or named value (`foo = true`)",
                    );
                }
                NestedMeta::Meta(Meta::Word(w)) => visit(w, None)?,
                NestedMeta::Meta(Meta::NameValue(nv)) => visit(nv.ident, Some(nv.lit))?,

            }
        }
    }

    Ok(())
}
