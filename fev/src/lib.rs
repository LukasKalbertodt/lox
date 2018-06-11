extern crate fev_core;
extern crate fev_io;


mod io {
    pub use fev_io::*;
}

// // TODO: exporting `core` stuff will look different in the future
// pub mod core {
//     pub use fev_core::*;
// }

pub use fev_core::*;
