//! Defines `Opt` which is used to parse command line arguments.

use structopt::StructOpt;
use lox::{
    io::FileFormat,
};

use crate::{
    util::EncodingRequest,
};


#[derive(StructOpt, Debug)]
pub struct Opt {
    /// Explicitly specify the source file format (otherwise it's guessed from
    /// the extension and file header). Valid values: ply, stl.
    #[structopt(
        long = "--source-format",
        parse(try_from_str = "parse_file_format"),
    )]
    pub source_format: Option<FileFormat>,

    /// Explicitly specify the target file format (otherwise it's guessed from
    /// the extension). Valid values: ply, stl.
    #[structopt(
        short = "-f",
        long = "--target-format",
        parse(try_from_str = "parse_file_format"),
    )]
    pub target_format: Option<FileFormat>,

    /// Specify the target file encoding. Valid values: 'binary' (native
    /// endianess), 'bbe' (binary big endian), 'ble' (binary little endian) and
    /// 'ascii'.
    #[structopt(
        short = "-e",
        long = "--target-encoding",
        default_value = "binary",
    )]
    pub target_encoding: EncodingRequest,

    /// Path to the source mesh file.
    pub source: String,

    /// Path to the target mesh file. The target format is guessed from the extension
    /// given here. Files are not overwritten by default.
    pub target: String,
}

fn parse_file_format(src: &str) -> Result<FileFormat, String> {
    match src {
        "ply" => Ok(FileFormat::Ply),
        "stl" => Ok(FileFormat::Stl),
        other => Err(format!("'{}' is currently not an accepted file format", other)),
    }
}
