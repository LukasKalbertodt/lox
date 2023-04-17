#![allow(warnings)]

use std::{env, error::Error, fs, io::Write};
use byteorder::{BigEndian, LittleEndian, WriteBytesExt};

use lox::{
    prelude::*,
    ds::{
        HalfEdgeMesh,
        half_edge::PolyConfig,
    },
    map::DenseMap,
    fat::AnyMesh,
    io::{
        self,
        ply::{raw, Reader},
    },
};
use cgmath::Point3;


fn main() -> Result<(), Box<dyn Error>> {
    color_backtrace::install();
    let file = env::args().nth(1).expect("no input filename given");

    // let reader = Reader::open(&file)?;
    // println!("{:#?}", reader.into_raw_storage()?);

    let m: AnyMesh = io::read_file(&file)?;
    // println!("{:#?}", m);

    Ok(())
}
