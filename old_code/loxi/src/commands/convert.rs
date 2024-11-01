use std::{
    fs::File,
    io::BufWriter,
    time::Instant,
};

use failure::{err_msg, format_err, Error, ResultExt};
use lox::{
    prelude::*,
    fat::AnyMesh,
    io::{FileEncoding, FileFormat},
};


use crate::{
    args::{GlobalArgs, ConvertArgs},
    commands::{
        guess_file_format, reader_and_encoding,
        info::MeshInfo,
    },
    ui,
};

pub fn run(global_args: &GlobalArgs, args: &ConvertArgs) -> Result<(), Error> {
    let start_time = Instant::now();

    // ----- Prepare ---------------------------------------------------------
    // Figure out the target file format and encoding
    let target_format = args.target_format
        .or_else(|| FileFormat::from_extension(&args.target))
        .ok_or_else(|| err_msg(
            "couldn't determine target file format, please specify it explicitly using \
                '--target-format'"
        ))?;

    let target_encoding = args.target_encoding.encoding_for(target_format).ok_or(
        format_err!(
            "the encoding {:?} is not supported by the {} format",
            args.target_encoding,
            target_format,
        )
    )?;

    // Read file
    let before_load = Instant::now();
    let mut mesh = load_file(global_args, args).context("could not read source file")?;
    let load_time = before_load.elapsed();

    // Print mesh data
    if !args.no_info {
        info!("Source mesh information:");
        println!();
        MeshInfo::about_mesh(&mesh).print(global_args);
        println!();
    }

    // Remove some properties if requested by the user
    let mut dirty = false;
    if args.without_fnormals {
        if mesh.face_normals.is_some() {
            info!("Removing face normals before writing");
            mesh.face_normals = None;
            dirty = true;
        } else {
            warn!(
                "`--without-fnormals` flag was set, but source mesh does not contain face normals"
            );
        }
    }

    if args.without_vnormals {
        if mesh.vertex_normals.is_some() {
            info!("Removing vertex normals before writing");
            mesh.vertex_normals = None;
            dirty = true;
        } else {
            warn!(
                "`--without-vnormals` flag was set, but source mesh does not contain \
                    vertex normals"
            );
        }
    }

    if args.without_fcolors {
        if mesh.face_colors.is_some() {
            info!("Removing face colors before writing");
            mesh.face_colors = None;
            dirty = true;
        } else {
            warn!(
                "`--without-fcolors` flag was set, but source mesh does not contain face colors"
            );
        }
    }

    if args.without_vcolors {
        if mesh.vertex_colors.is_some() {
            info!("Removing vertex colors before writing");
            mesh.vertex_colors = None;
            dirty = true;
        } else {
            warn!(
                "`--without-vcolors` flag was set, but source mesh does not contain \
                    vertex colors"
            );
        }
    }

    // Print mesh data
    if !args.no_info && dirty {
        info!("Target mesh information:");
        println!();
        MeshInfo::about_mesh(&mesh).print(global_args);
        println!();
    }

    // Check target compatibility
    check_compatibility(&mesh, target_format);

    // Write file
    let before_write = Instant::now();
    write_file(&mesh, target_format, target_encoding, global_args, args)
        .context("could not write target file")?;
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

fn write_file(
    data: &AnyMesh,
    format: FileFormat,
    encoding: FileEncoding,
    _global_args: &GlobalArgs,
    args: &ConvertArgs,
) -> Result<(), Error> {
    info!("Target format: {} ({} encoding)", format, encoding);

    // We can `unwrap()` here because we know the encoding is compatible with
    // the format.
    let file = BufWriter::new(File::create(&args.target)?);
    let writer = format.writer_with_encoding(encoding, file).unwrap();

    progress!(["Writing mesh to '{}'", args.target] => {
        writer.transfer_from(data)?;
    });

    Ok(())
}

fn check_compatibility(
    mesh: &AnyMesh,
    format: FileFormat,
) {
    // TODO: add a whole system of things to warn about and what not.
    // `--warn cast`
    // `--warn data-loss`
    // something like that
    match format {
        FileFormat::Stl => {
            let mut unsupported_props = Vec::new();

            if mesh.vertex_normals.is_some() {
                unsupported_props.push("vertex normals");
            }
            if mesh.vertex_colors.is_some() {
                unsupported_props.push("vertex colors");
            }
            if mesh.face_colors.is_some() {
                unsupported_props.push("face colors");
            }

            if !unsupported_props.is_empty() {
                warn!(
                    "Source file contains {}, but these properties are not supported by STL. \
                        These properties will be ignored!",
                    ui::comma_and_list(unsupported_props),
                );
            }
        }
        FileFormat::Ply => {
            // PLY currently supports everything that the IO interface can
            // offer
        }
        _ => unimplemented!(),
    }
}
