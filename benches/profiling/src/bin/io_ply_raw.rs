use std::{
    fs,
    io::Cursor,
};

use lox::{
    io::{
        ply::Reader,
    }
};

use profiling::util::{
    io::ply::NullRawSink,
};

fn main() {
    let data = get_data();
    let reader = Reader::new(Cursor::new(data)).unwrap();
    let mut sink = NullRawSink;
    reader.read_raw(&mut sink).expect("failed to read");
}

#[inline(never)]
fn get_data() -> Vec<u8> {
    let filename = std::env::args().nth(1).expect("missing source file argument");
    fs::read(filename).expect("failed to read file")
}
