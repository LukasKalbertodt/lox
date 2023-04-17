//! Utilities to make parsing easier.

// use std::fmt;
use proc_macro2::{Ident, Spacing};
use syn::{
    // Error,
    parse::{self, ParseBuffer},
    // spanned::Spanned,
};

// macro_rules! bail {
//     ($span:expr, $fmt:literal $($tail:tt)*) => {
//         return Err(Error::new($span, format!($fmt $($tail)*)))
//     }
// }


// pub(crate) fn struct_fields(
//     input: &syn::DeriveInput,
//     error_msg: impl fmt::Display,
// ) -> Result<&syn::Fields, syn::Error> {
//     if let syn::Data::Struct(s) = &input.data {
//         Ok(&s.fields)
//     } else {
//         Err(Error::new(input.span(), error_msg))
//     }
// }

/// Adds a few methods to `ParseBuffer` to parse specific tokens.
pub(crate) trait ParseBufferExt {
    /// Consumes an `Ident` token if that's the next token in the stream. If
    /// `expected` is not `None`, the token is compared to the given string. If
    /// the string doesn't match or the next token is not an `Ident`, an error
    /// is returned.
    fn eat_ident<'a>(&self, expected: impl Into<Option<&'a str>>) -> parse::Result<Ident>;

    /// Consumes one or more punctuation tokens. If the next tokens don't match
    /// `expected`, an error is returned.
    ///
    /// This method is callable with a byte slice (`b"+="`). This is
    /// automatically interpreted to check for alone/joint punctuations.
    fn eat_punct(&self, expected: &[u8]) -> parse::Result<()>;
}

impl ParseBufferExt for ParseBuffer<'_> {
    fn eat_ident<'a>(&self, expected: impl Into<Option<&'a str>>) -> parse::Result<Ident> {
        self.step(|cursor| {
            match (expected.into(), cursor.ident()) {
                (None, Some((ident, rest))) => Ok((ident, rest)),
                (Some(expected), Some((ident, rest))) => {
                    if expected == ident.to_string() {
                        Ok((ident, rest))
                    } else {
                        let msg = format!("expected identifier '{}', got '{}'", expected, ident);
                        Err(cursor.error(msg))
                    }
                }
                (_, None) => Err(cursor.error("expected identifier")),
            }
        })
    }

    fn eat_punct(&self, expected: &[u8]) -> parse::Result<()> {
        assert!(expected.len() >= 1 || expected.len() <= 2);

        for i in 0..expected.len() {
            self.step(|cursor| {
                if let Some((punct, rest)) = cursor.punct() {
                    // Declare expected values
                    let c = char::from(expected[i]);
                    let spacing = if i == expected.len() - 1 {
                        Spacing::Alone
                    } else {
                        Spacing::Joint
                    };

                    // Check if the actual token matches
                    if punct.as_char() == c && punct.spacing() == spacing {
                        Ok(((), rest))
                    } else {
                        let msg = format!(
                            "expected '{}', found '{}'",
                            std::str::from_utf8(expected).unwrap(),
                            punct.as_char(),
                        );
                        Err(cursor.error(msg))
                    }
                } else {
                    Err(cursor.error("expected punctuation"))
                }
            })?;
        }

        Ok(())
    }
}
