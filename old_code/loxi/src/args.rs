//! Defines `Args` which is used to parse command line arguments.

use structopt::StructOpt;
use lox::{
    io::FileFormat,
};

use crate::{
    util::EncodingRequest,
};


#[derive(StructOpt, Debug)]
#[structopt(setting(structopt::clap::AppSettings::VersionlessSubcommands))]
pub struct Args {
    #[structopt(flatten)]
    pub global: GlobalArgs,

    #[structopt(subcommand)]
    pub command: Command,
}

#[derive(StructOpt, Debug)]
pub struct GlobalArgs {

}

#[derive(StructOpt, Debug)]
pub enum Command {
    /// Print information about a mesh file.
    Info {
        #[structopt(flatten)]
        args: InfoArgs,
    },

    /// Converts a mesh from one file format into another one.
    Convert {
        #[structopt(flatten)]
        args: ConvertArgs,
    },
}

#[derive(StructOpt, Debug)]
pub struct ConvertArgs {
    /// Explicitly specify the source file format (otherwise it's guessed from
    /// the extension and file header). Valid values: ply, stl.
    #[structopt(long, parse(try_from_str = parse_file_format))]
    pub source_format: Option<FileFormat>,

    /// Explicitly specify the target file format (otherwise it's guessed from
    /// the extension). Valid values: ply, stl.
    #[structopt(
        short = "-f",
        long,
        parse(try_from_str = parse_file_format),
    )]
    pub target_format: Option<FileFormat>,

    /// Specify the target file encoding. Valid values: 'binary' (native
    /// endianness), 'bbe' (binary big endian), 'ble' (binary little endian) and
    /// 'ascii'.
    #[structopt(
        short = "-e",
        long,
        default_value = "binary",
    )]
    pub target_encoding: EncodingRequest,

    /// Path to the source mesh file.
    pub source: String,

    /// Path to the target mesh file. The target format is guessed from the
    /// extension given here. Files are not overwritten by default.
    pub target: String,

    /// If set, information about the source mesh won't be printed
    #[structopt(long)]
    pub no_info: bool,

    /// If set, face normals from the source mesh won't be written in the
    /// target mesh.
    #[structopt(long)]
    pub without_fnormals: bool,

    /// If set, vertex normals from the source mesh won't be written in the
    /// target mesh.
    #[structopt(long)]
    pub without_vnormals: bool,

    /// If set, face colors from the source mesh won't be written in the
    /// target mesh.
    #[structopt(long)]
    pub without_fcolors: bool,

    /// If set, vertex colors from the source mesh won't be written in the
    /// target mesh.
    #[structopt(long)]
    pub without_vcolors: bool,

    // TODO: --calc-fnormals
}


#[derive(StructOpt, Debug)]
pub struct InfoArgs {
    /// Explicitly specify the source file format (otherwise it's guessed from
    /// the extension and file header). Valid values: ply, stl.
    #[structopt(
        long,
        parse(try_from_str = parse_file_format),
    )]
    pub source_format: Option<FileFormat>,

    /// If specified, only the header (and not the body) of the file will be
    /// read. Some file formats (like PLY) store almost all relevant
    /// information in the header, while others (like OBJ) do not store
    /// anything in the header. Reading the body also has the advantage of
    /// checking whether the file is completely valid. If neither
    /// `--header-only` nor `--read-body` is specified, the body will only be
    /// read if the header does not contain all information.
    #[structopt(
        long,
        conflicts_with = "--read-body"
    )]
    pub header_only: bool,

    /// If specified, the body of the input file is always read completely,
    /// even if the header already contains all relevant information. This has
    /// the advantage that the file completely inspected and any error is
    /// detected.
    #[structopt(long)]
    pub read_body: bool,

    /// If specified, the mesh is analyzed for additional properties (Is the
    /// mesh closed?, Bounding box of all vertice, ...). This requires reading
    /// the file body, so this implies `--read-body`.
    #[structopt(
        short,
        long,
        conflicts_with = "--header-only"
    )]
    pub analyze: bool,

    /// Path to the mesh file.
    pub files: Vec<String>,

    // TODO:
    // - different output types (short, json, ...)
    // - output template?
    // - include/exclude specific information
}

fn parse_file_format(src: &str) -> Result<FileFormat, String> {
    match src {
        "ply" => Ok(FileFormat::Ply),
        "stl" => Ok(FileFormat::Stl),
        other => Err(format!("'{}' is currently not an accepted file format", other)),
    }
}
