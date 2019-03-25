use criterion::black_box;

use lox::{
    io::{
        Error,
        ply::{
            raw::{ElementDef, RawElement, RawSink},
        },
    },
};


/// A raw sink that just puts all data into the `black_box`.
pub struct NullRawSink;

impl RawSink for NullRawSink {
    fn element_group_start(&mut self, def: &ElementDef) -> Result<(), Error> {
        black_box(def);
        Ok(())
    }
    fn element(&mut self, elem: &RawElement) -> Result<(), Error> {
        black_box(elem);
        Ok(())
    }
}
