use std::{
    fs::File,
    io::BufWriter,
    time::Instant,
};

use failure::{err_msg, format_err, Error, ResultExt};
use lox::{
    prelude::*,
    fat::AnyMesh,
    io::FileFormat,
};


use crate::{
    args::{GlobalArgs, ConvertArgs},
    commands::{
        guess_file_format, reader_and_encoding,
        info::MeshInfo,
    },
};

pub fn run(global_args: &GlobalArgs, args: &ConvertArgs) -> Result<(), Error> {
    let start_time = Instant::now();

    let before_load = Instant::now();
    let mesh_data = load_file(global_args, args).context("could not read source file")?;
    let load_time = before_load.elapsed();

    info!("Mesh information:");
    println!();
    MeshInfo::about_mesh(&mesh_data).print(global_args);
    println!();

    let before_write = Instant::now();
    write_file(global_args, args, &mesh_data).context("could not write target file")?;
    let write_time = before_write.elapsed();

    info!(
        "Processing time: {:.2?} ({:.2?} loading, {:.2?} writing)",
        start_time.elapsed(),
        load_time,
        write_time,
    );

    Ok(())
}

fn load_file(_global_args: &GlobalArgs, args: &ConvertArgs) -> Result<AnyMesh, Error> {
    // Open file and figure out file format
    let filename = &args.source;
    let mut file = File::open(filename)
        .context(format!("failed to open '{}'", filename))?;
    let file_format = guess_file_format(args.source_format, filename, &mut file)?;


    // Parse the header of the file andprint some information.
    let (reader, encoding) = reader_and_encoding(file_format, file)?;
    info!("Source format: {} ({} encoding)", file_format, encoding);

    // Read from the reader into an `AnyMesh`
    let mut mesh = AnyMesh::empty();
    progress!(["Reading '{}'", args.source] => {
        reader.transfer_to(&mut mesh)?;
        mesh.finish()?;
    });

    Ok(mesh)
}

fn write_file(_global_args: &GlobalArgs, args: &ConvertArgs, data: &AnyMesh) -> Result<(), Error> {
    // Figure out the file format
    let file_format = args.target_format
        .or_else(|| FileFormat::from_extension(&args.target))
        .ok_or_else(|| err_msg(
            "couldn't determine target file format, please specify it explicitly using \
                '--target-format'"
        ))?;


    let encoding = args.target_encoding.encoding_for(file_format).ok_or(
        format_err!(
            "the encoding {:?} is not supported by the {} format",
            args.target_encoding,
            file_format,
        )
    )?;

    info!(
        "Target format: {} ({} encoding)",
        file_format,
        encoding,
    );

    let file = BufWriter::new(File::create(&args.target)?);
    let writer = file_format.writer_with_encoding(encoding, file).unwrap();

    progress!(["Writing mesh to '{}'", args.target] => {
        writer.transfer_from(data)?;
    });

    Ok(())
}
