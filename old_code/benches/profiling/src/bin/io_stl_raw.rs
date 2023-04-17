use std::{
    fs,
    io::Cursor,
};

use criterion::black_box;
use lox::{
    io::{
        stl::Reader,
    }
};

fn main() {
    let data = get_data();
    let reader = Reader::new(Cursor::new(data)).unwrap();
    reader.read_raw(|tri| { black_box(tri); Ok(()) }).expect("failed to read");
}

#[inline(never)]
fn get_data() -> Vec<u8> {
    let filename = std::env::args().nth(1).expect("missing source file argument");
    fs::read(filename).expect("failed to read file")
}
