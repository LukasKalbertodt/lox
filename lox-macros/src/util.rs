//! Utilities to make parsing easier.

use proc_macro2::{Ident, Spacing};
use syn::{
    parse::{ParseStream, Result},
};


/// Adds a few methods to `ParseStream` to parse specific tokens.
pub(crate) trait ParseStreamExt {
    /// Consumes an `Ident` token if that's the next token in the stream. If
    /// `expected` is not `None`, the token is compared to the given string. If
    /// the string doesn't match or the next token is not an `Ident`, an error
    /// is returned.
    fn eat_ident<'a>(&self, expected: impl Into<Option<&'a str>>) -> Result<Ident>;

    /// Consumes one or more punctuation tokens. If the next tokens don't match
    /// `expected`, an error is returned.
    ///
    /// This method is callable with a byte slice (`b"+="`). This is
    /// automatically interpreted to check for alone/joint punctuations.
    fn eat_punct(&self, expected: &[u8]) -> Result<()>;
}

impl ParseStreamExt for ParseStream<'_> {
    fn eat_ident<'a>(&self, expected: impl Into<Option<&'a str>>) -> Result<Ident> {
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

    fn eat_punct(&self, expected: &[u8]) -> Result<()> {
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
