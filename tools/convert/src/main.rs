#[macro_use]
extern crate structopt;

use std::{
    fs::File,
    io::{Seek, SeekFrom, Read},
};

use failure::{err_msg, Error, ResultExt};
use structopt::StructOpt;

use lox::{
    MeshWithProps,
    ds::SharedVertexMesh,
    io::{FileFormat, stl, ply},
    prelude::*,
};


mod opt;

use crate::opt::Opt;


/// We just catch potential errors here and pretty print them. The actual
/// useful code is in `run()`.
fn main() {
    if let Err(e) = run() {
        println!("An error occured: {}", e);

        for cause in e.iter_causes() {
            println!("  ... caused by: {}", cause);
        }

        if std::env::var("RUST_BACKTRACE") == Ok("1".to_string()) {
            println!();
            println!("{}", e.backtrace());
        }

        std::process::exit(1);
    }
}

fn run() -> Result<(), Error> {
    let opt = Opt::from_args();
    println!("{:?}", opt);

    // Load file
    load_file(&opt).context("could not read source file")?;

    Ok(())
}

fn load_file(opt: &Opt) -> Result<(), Error> {
    let mut file = File::open(&opt.source).context("failed to open file")?;
    let file_start = {
        let mut v = Vec::new();
        file.by_ref().take(1024).read_to_end(&mut v)?;
        file.seek(SeekFrom::Start(0))?;
        v
    };

    let file_format = opt.source_format
        .or_else(|| FileFormat::from_extension(&opt.source))
        // .or_else(|| None)  // TODO: guess from first 1024 bytes
        .ok_or_else(|| err_msg(
            "couldn't determine source file format, please specify it explicitly using \
                '--source-format'"
        ))?;

    println!("source format: {:?}", file_format);

    match file_format {
        FileFormat::Ply => {
            let _reader = ply::Reader::new(file).context("failed to read PLY header")?;

        }
        FileFormat::Stl => {
            let _reader = stl::Reader::new(file).context("failed to read STL header")?;

        }
    }

    Ok(())
}
