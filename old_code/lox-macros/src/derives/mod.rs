//! Contains all code for custom derives.

use quote::{quote, quote_spanned};
use proc_macro2::TokenStream as TokenStream2;
use syn::{
    DeriveInput, Error, Fields,
    spanned::Spanned,
};

use crate::util::struct_fields;


// pub(crate) mod input;
// pub(crate) mod mem_sink;
// pub(crate) mod mem_source;


// /// Specifies the default casting mode when the casting mode is not explicitly
// /// specifid.
// const DEFAULT_CAST_MODE: input::CastMode = input::CastMode::Lossy;

// /// Specifies whether casting colors (in "rounding" mode) is allowed when a
// /// casting mode is not explicitly specified.
// const DEFAULT_COLOR_CAST_ALLOWED: bool = true;
