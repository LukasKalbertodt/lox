use std::{
    fs::File,
    io::BufWriter,
    time::Instant,
};

use failure::{bail, err_msg, format_err, Error, ResultExt};
use lox::{
    prelude::*,
    fat::AnyMesh,
    io::{
        FileFormat, FileEncoding,
        stl, ply,
    },
};


use crate::{
    args::{GlobalArgs, ConvertArgs},
    commands::guess_file_format,
};

pub fn run(global_args: &GlobalArgs, args: &ConvertArgs) -> Result<(), Error> {
    let start_time = Instant::now();

    let before_load = Instant::now();
    let mesh_data = load_file(global_args, args).context("could not read source file")?;
    let load_time = before_load.elapsed();

    print_mesh_info(&mesh_data);

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

fn print_mesh_info(mesh_data: &AnyMesh) {
    info!("Mesh information:");

    // ===== Vertex Infos ====================================================
    // Collect vertex properties
    let mut vertex_props = vec![];
    if mesh_data.vertex_positions.is_some() {
        vertex_props.push("position");
    }

    let vertex_props = if vertex_props.is_empty() {
        "none".to_string()
    } else {
        let mut out = vertex_props[0].to_string();
        for prop in &vertex_props[1..] {
            out += ", ";
            out += prop;
        }
        out
    };

    println!(
        "    │ {} vertices (properties: {})",
        mesh_data.mesh.num_vertices(),
        vertex_props,
    );


    // ===== Face Infos ======================================================
    println!(
        "    └ {} faces (properties: none)",
        mesh_data.mesh.num_faces(),
    );
}

fn load_file(_global_args: &GlobalArgs, args: &ConvertArgs) -> Result<AnyMesh, Error> {
    // Open file and figure out file format
    let filename = &args.source;
    let mut file = File::open(filename)
        .context(format!("failed to open '{}'", filename))?;
    let file_format = guess_file_format(args.source_format, filename, &mut file)?;


    // Parse the header of the file, print some information and return the
    // abstract reader object.
    macro_rules! get_reader_and_print_info {
        ($($variant:ident => $module:ident,)*) => {
            match file_format {
                $(
                    FileFormat::$variant => {
                        let reader = $module::Reader::new(file)
                            .context(format!("failed to read {} header", FileFormat::$variant))?;

                        info!(
                            "Source format: {} ({} encoding)",
                            file_format,
                            FileEncoding::from(reader.encoding()),
                        );

                        Box::new(reader) as Box<dyn DynStreamSource<_>>
                    }
                )*
                _ => bail!(
                    "File format '{}' not supported (this is probably a bug)",
                    file_format,
                ),
            }
        }
    }

    let reader = get_reader_and_print_info!(
        Ply => ply,
        Stl => stl,
    );


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
