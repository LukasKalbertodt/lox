//! Everything related to meshes.
//!
//! **TODO**: Everything.

#![feature(crate_in_paths)]

extern crate fev_core;

// This is a trick to reexport extern crate as modules without the extern
// crates showing up in the documentation.
//
// See: https://github.com/rust-lang/rust/issues/34537
mod inner {
    pub extern crate fev_impls;
    pub extern crate fev_io;
    pub extern crate fev_map;
}


pub use fev_core::{handle, prop};

pub use inner::fev_map as map;
pub use inner::fev_impls as impls;
pub use inner::fev_io as io;
