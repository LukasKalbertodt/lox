#[allow(unused_imports)] // TODO
use proc_macro2::{
    TokenStream, Span,
};
use syn::{
    Attribute, DeriveInput, Error, Ident, Type, Meta, NestedMeta, Lit, Generics,
    spanned::Spanned,
};


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

    pub(crate) fn to_str(&self) -> &'static str {
        match self {
            CastMode::None => "none",
            CastMode::Lossless => "lossless",
            CastMode::Clamping => "clamping",
            CastMode::Rounding => "rounding",
            CastMode::Lossy => "lossy",
        }
    }

    pub(crate) fn is_rounding_allowed(&self) -> bool {
        match self {
            CastMode::None => false,
            CastMode::Lossless => false,
            CastMode::Clamping => false,
            CastMode::Rounding => true,
            CastMode::Lossy => true,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct SpannedCastMode {
    pub(crate) span: Span,
    pub(crate) mode: CastMode,
}

impl SpannedCastMode {
    fn from_attr(ident: &Ident, lit: Option<&Lit>) -> Result<SpannedCastMode, Error> {
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
                Ok(SpannedCastMode {
                    span: lit.span(),
                    mode,
                })
            }
            None => bail!(
                lit.span(),
                "invalid cast mode \"{}\" (allowed modes: \"none\", \"lossless\", \
                    \"rounding\", \"clamping\" and \"lossy\")",
                value,
            ),
        }
    }
}

#[derive(Debug)]
pub(crate) struct CoreMeshField {
    pub(crate) span: Span,
    pub(crate) name: Ident,
    pub(crate) ty: Type,
}

#[derive(Debug)]
pub(crate) struct ColorPropField {
    pub(crate) allow_cast: Option<bool>,
    pub(crate) span: Span,
    pub(crate) name: Ident,
    pub(crate) ty: Type,
}


#[derive(Debug)]
pub(crate) struct PropField {
    pub(crate) cast_mode: Option<SpannedCastMode>,
    pub(crate) span: Span,
    pub(crate) name: Ident,
    pub(crate) ty: Type,
}

#[derive(Debug)]
#[allow(dead_code)] // TODO
pub(crate) struct Input {
    pub(crate) cast_mode: Option<SpannedCastMode>,
    pub(crate) name: Ident,
    pub(crate) generics: Generics,

    pub(crate) core_mesh: CoreMeshField,
    pub(crate) vertex_position: Option<PropField>,
    pub(crate) vertex_normal: Option<PropField>,
    pub(crate) vertex_color: Option<ColorPropField>,
    pub(crate) face_normal: Option<PropField>,
    pub(crate) face_color: Option<ColorPropField>,
}

impl Input {
    pub(crate) fn from_syn(input: &DeriveInput, derive: &str) -> Result<Self, Error> {
        // Parse the attributes ofthe struct
        let struct_attrs = parse_struct_attrs(&input.attrs)?;


        // Make sure the input is a struct and extract the fields from it
        let fields = match &input.data {
            syn::Data::Struct(s) => &s.fields,
            _ => bail!(input.span(), "only structs can derive `{}`", derive),
        };

        let mut core_mesh = None;
        let mut vertex_position = None;
        let mut vertex_normal = None;
        let mut vertex_color = None;
        let mut face_normal = None;
        let mut face_color = None;

        for f in fields {
            // Make sure the field is named and extract that name.
            let ident = f.ident.as_ref().ok_or_else(|| {
                let msg = format!(
                    "unit structs are currently not supported by `derive({})` (use a normal \
                        struct with named fields instead)",
                    derive,
                );
                Error::new(input.span(), msg)
            })?;

            macro_rules! check_dupe {
                ($field:ident) => {
                    if $field.is_some() {
                        bail!(ident.span(), "duplicate `{}` field", stringify!($field));
                    }
                }
            }

            macro_rules! set_prop_field {
                ($field:ident, $attrs:ident) => {{
                    check_dupe!($field);
                    $field = Some(PropField {
                        cast_mode: $attrs.cast_mode,
                        span: f.span(),
                        name: ident.clone(),
                        ty: f.ty.clone(),
                    });
                }}
            }

            macro_rules! set_color_field {
                ($field:ident, $attrs:ident) => {{
                    if let Some(mode) = $attrs.cast_mode {
                        match mode.mode {
                            CastMode::Clamping | CastMode::Lossy | CastMode::Lossless => {
                                bail!(
                                    mode.span,
                                    "invalid cast mode for color property (only \"none\" and \
                                        \"rounding\" are allowed)",
                                );
                            }
                            _ => {}
                        }
                    }

                    check_dupe!($field);
                    $field = Some(ColorPropField {
                        allow_cast: $attrs.cast_mode.map(|m| m.mode == CastMode::Rounding),
                        span: f.span(),
                        name: ident.clone(),
                        ty: f.ty.clone(),
                    });
                }}
            }

            let attrs = parse_field_attrs(&f.attrs)?;
            if let Some(purpose) = attrs.purpose {
                match purpose {
                    FieldPurpose::CoreMesh => {
                        if let Some(mode) = attrs.cast_mode {
                            bail!(
                                mode.span,
                                "cast mode on `core_mesh` field is not allowed because it \
                                    doesn't make sense",
                            );
                        }

                        check_dupe!(core_mesh);

                        core_mesh = Some(CoreMeshField {
                            span: f.span(),
                            name: ident.clone(),
                            ty: f.ty.clone(),
                        });
                    }
                    FieldPurpose::VertexPosition => set_prop_field!(vertex_position, attrs),
                    FieldPurpose::VertexNormal => set_prop_field!(vertex_normal, attrs),
                    FieldPurpose::VertexColor => set_color_field!(vertex_color, attrs),
                    FieldPurpose::FaceNormal => set_prop_field!(face_normal, attrs),
                    FieldPurpose::FaceColor => set_color_field!(face_color, attrs),
                }

            } else {
                if let Some(mode) = attrs.cast_mode {
                    bail!(
                        mode.span,
                        "cast mode specified, but this field does not have a purpose \
                            (maybe you forgot to add an attribute like `vertex_position`?)",
                    );
                }
            }
        }

        let core_mesh = match core_mesh {
            Some(m) => m,
            None => {
                bail!(
                    input.ident.span(),
                    "no `core_mesh` field found, but a core mesh is required for `derive({})` \
                        (maybe you forgot to annotate `#[lox(core_mesh)]`?)",
                    derive,
                );
            }
        };

        Ok(Input {
            cast_mode: struct_attrs.cast_mode,
            name: input.ident.clone(),
            generics: input.generics.clone(),
            core_mesh,
            vertex_position,
            vertex_normal,
            vertex_color,
            face_normal,
            face_color,
        })
    }
}

#[derive(Debug, Copy, Clone)]
#[allow(dead_code)]
enum FieldPurpose {
    CoreMesh,
    VertexPosition,
    VertexNormal,
    VertexColor,
    FaceNormal,
    FaceColor,
}

impl FieldPurpose {
    fn keyword(&self) -> &'static str {
        match self {
            FieldPurpose::CoreMesh => "core_mesh",
            FieldPurpose::VertexPosition => "vertex_position",
            FieldPurpose::VertexNormal => "vertex_normal",
            FieldPurpose::VertexColor => "vertex_color",
            FieldPurpose::FaceNormal => "face_normal",
            FieldPurpose::FaceColor => "face_color",
        }
    }
}

/// All attributes we accept on a struct directly.
#[derive(Debug)]
struct FieldAttrs {
    cast_mode: Option<SpannedCastMode>,
    purpose: Option<FieldPurpose>,
}

/// Parse the attributes of a field.
fn parse_field_attrs(attrs: &[Attribute]) -> Result<FieldAttrs, Error> {
    let mut out = FieldAttrs {
        cast_mode: None,
        purpose: None,
    };

    macro_rules! check_purpose {
        ($ident:ident, $variant:ident) => {{
            if let Some(before) = &out.purpose {
                bail!(
                    $ident.span(),
                    "duplicate field purpose (field was already marked as '{}' before)",
                    before.keyword(),
                );
            }
            out.purpose = Some(FieldPurpose::$variant);
        }}
    }

    visit_lox_attrs(attrs, |ident, lit| {
        match ident.to_string().as_str() {
            // ===== 'cast' attribute =====
            "cast" => {
                let mode = SpannedCastMode::from_attr(&ident, lit.as_ref())?;

                if out.cast_mode.is_some() {
                    bail!(lit.span(), "duplicate `cast` attribute");
                }

                out.cast_mode = Some(mode);
            }

            // ===== Field purposes =====
            "core_mesh" => check_purpose!(ident, CoreMesh),
            "vertex_position" => check_purpose!(ident, VertexPosition),
            "vertex_normal" => check_purpose!(ident, VertexNormal),
            "vertex_color" => check_purpose!(ident, VertexColor),
            "face_normal" => check_purpose!(ident, FaceNormal),
            "face_color" => check_purpose!(ident, FaceColor),

            // ===== Unknown attribute =====
            _ => bail!(ident.span(), "'{}' is not a valid lox attribute for a field", ident),
        }

        Ok(())
    })?;

    Ok(out)
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
        match ident.to_string().as_str() {
            // ===== 'cast' attribute =====
            "cast" => {
                let mode = SpannedCastMode::from_attr(&ident, lit.as_ref())?;

                if out.cast_mode.is_some() {
                    bail!(lit.span(), "duplicate `cast` attribute");
                }

                out.cast_mode = Some(mode);
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
