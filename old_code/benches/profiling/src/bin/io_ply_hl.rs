use std::{
    fs,
    io::Cursor,
};

use criterion::black_box;

use lox::{
    prelude::*,
    io::{
        ply::Reader,
    }
};

use profiling::util::{
    io::NullSinkPos,
};

fn main() {
    let data = get_data();
    let reader = Reader::new(Cursor::new(data)).unwrap();
    let mut sink = NullSinkPos::new();
    let out = reader.transfer_to(&mut sink).expect("failed to read");
    black_box(sink);
    black_box(out);
}

#[inline(never)]
fn get_data() -> Vec<u8> {
    let filename = std::env::args().nth(1).expect("missing source file argument");
    fs::read(filename).expect("failed to read file")
}
