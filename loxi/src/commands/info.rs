use std::{
    fmt,
    fs::File,
    iter,
};

use failure::{Error, ResultExt};
use term_painter::{Color, ToStyle, Style, Painted};
use lox::{
    prelude::*,
    algo::bounding::BoundingBox,
    fat::AnyMesh,
    io::{
        stl, ply,
        ColorType, FileFormat, FileEncoding, PrimitiveType, PrimitiveColorChannelType,
    },
};

use crate::{
    args::{GlobalArgs, InfoArgs},
    commands::{guess_file_format, reader_and_encoding},
    ui,
};
use ply::info::{ColorPropInfo, Vec3PropInfo};

pub fn run(global_args: &GlobalArgs, args: &InfoArgs) -> Result<(), Error> {
    for filename in &args.files {
        // Open file and figure out file format
        let mut file = File::open(filename)
            .context(format!("failed to open file '{}'", filename))?;
        let format = guess_file_format(args.source_format, filename, &mut file)
            .context(format!("failed to guess file format of '{}'", filename))?;


        let header_is_sufficient = match format {
            FileFormat::Stl => false,
            FileFormat::Ply => true,
            _ => unimplemented!(),
        };

        let read_only_header = args.header_only
            || (header_is_sufficient && !args.read_body && !args.analyze);

        // ===== Obtaining the information =======================================
        let (info, mesh) = if read_only_header {
            if !args.header_only && header_is_sufficient {
                info!(
                    "Note: since (for {} files) all information is already contained in the \
                        header, the body of the file is not read. This means that errors in the \
                        file's body are not detected. You can force reading the body \
                        with `--read-body`.",
                    format,
                );
            }

            let info = Info::from_header(format, filename, file, global_args, args)?;
            (info, None)
        } else {
            let (info, mesh) = Info::from_body(format, filename, file, global_args, args)?;
            (info, Some(mesh))
        };


        // ===== Actually printing the information ===============================
        println!();
        Color::White.bold().with(|| {
            println!("══════════╡ {} ╞══════════", Color::Green.paint(&filename));
        });

        println!(
            "File format: {} (encoding: {})",
            Color::BrightWhite.bold().paint(info.format),
            Color::BrightWhite.paint(info.encoding),
        );

        println!();
        info.mesh.print(global_args);
        println!();


        // Printing additional information which require analyzing the mesh
        if args.analyze {
            // We can unwrap because we always read the body in the `analyze` case.
            let mesh = mesh.as_ref().unwrap();

            println!("Additional information:");

            if let Some(vertex_positions) = &mesh.vertex_positions {
                let vertex_positions = mesh.mesh.vertex_handles()
                    .map(|vh| vertex_positions.get_casted_lossy::<f64>(vh).unwrap());

                print!(" - Bounding box");
                let bb = BoundingBox::around(vertex_positions.clone());
                let center: [_; 3] = bb.center().into();
                println!(" (center: {:.3?})", center);
                println!("    - x: {:.3}..{:.3}", bb.x()[0], bb.x()[1]);
                println!("    - y: {:.3}..{:.3}", bb.y()[0], bb.y()[1]);
                println!("    - z: {:.3}..{:.3}", bb.z()[0], bb.z()[1]);
            }


            println!();
        }
    }

    Ok(())
}


/// All the standard info we can get about a mesh in a file.
#[derive(Debug)]
pub struct Info {
    pub format: FileFormat,
    pub encoding: FileEncoding,
    pub mesh: MeshInfo,
}

/// Information about the mesh properties and everything else directly related
/// to the mesh data.
#[derive(Debug)]
pub struct MeshInfo {
    pub vertex: ElementInfo,
    pub edge: ElementInfo,
    pub face: ElementInfo,
}

/// Information about one element (vertex, edge, face).
#[derive(Debug)]
pub struct ElementInfo {
    pub count: MaybeInfo<u64>,
    pub position_type: MaybeInfo<PrimitiveType>,
    pub normal_type: MaybeInfo<PrimitiveType>,
    pub color_type: MaybeInfo<ColorType>,
}

impl ElementInfo {
    /// Returns an instance with all fields set to `MaybeInfo::None`.
    fn none() -> Self {
        Self {
            count: MaybeInfo::None,
            position_type: MaybeInfo::None,
            normal_type: MaybeInfo::None,
            color_type: MaybeInfo::None,
        }
    }
}

impl Info {
    /// Gets information from just reading the header of the input file.
    pub fn from_header(
        format: FileFormat,
        filename: &str,
        file: File,
        _global_args: &GlobalArgs,
        _args: &InfoArgs,
    ) -> Result<Self, Error> {
        // Get information. This is different for each format.
        let err_read_header = format!("failed to read {} header of '{}'", format, filename);
        let info = match format {
            // ===== STL =========================================================
            FileFormat::Stl => {
                let reader = stl::Reader::new(file).context(err_read_header)?;
                let encoding = FileEncoding::from(reader.encoding());
                let triangle_count = MaybeInfo::some_or_unknown(
                    reader.triangle_count().map(Into::into)
                );

                Info {
                    format,
                    encoding,
                    mesh: MeshInfo {
                        vertex: ElementInfo {
                            count: MaybeInfo::Unknown,
                            position_type: MaybeInfo::Known(PrimitiveType::Float32),
                            normal_type: MaybeInfo::None,
                            color_type: MaybeInfo::None,
                        },
                        edge: ElementInfo::none(),
                        face: ElementInfo {
                            count: triangle_count,
                            position_type: MaybeInfo::None,
                            normal_type: MaybeInfo::Known(PrimitiveType::Float32),
                            color_type: MaybeInfo::None,
                        },
                    },
                }
            }

            // ===== PLY =========================================================
            FileFormat::Ply => {
                // Gets information about one PLY element ("vertex", "face", "edge")
                fn get_element_info(
                    reader: &ply::Reader<File>,
                    name: &str,
                ) -> Result<ElementInfo, Error> {
                    if let Some(def) = reader.elements().iter().find(|def| def.name == name) {
                        let position_type = Vec3PropInfo::new(&def, ["x", "y", "z"], "positions")?
                            .map(|info| info.ty.to_primitive_type());
                        let normal_type = Vec3PropInfo::new(&def, ["nx", "ny", "nz"], "normals")?
                            .map(|info| info.ty.to_primitive_type());
                        let color_type = ColorPropInfo::new(&def)?.map(|info| ColorType {
                            alpha: info.has_alpha(),
                            channel_type: PrimitiveColorChannelType::Uint8,
                        });

                        Ok(ElementInfo {
                            count: MaybeInfo::Known(def.count),
                            position_type: MaybeInfo::some_or_none(position_type),
                            normal_type: MaybeInfo::some_or_none(normal_type),
                            color_type: MaybeInfo::some_or_none(color_type),
                        })
                    } else {
                        Ok(ElementInfo::none())
                    }
                }

                let reader = ply::Reader::new(file).context(err_read_header)?;
                let encoding = FileEncoding::from(reader.encoding());

                Info {
                    format,
                    encoding,
                    mesh: MeshInfo {
                        vertex: get_element_info(&reader, "vertex")?,
                        edge: get_element_info(&reader, "edge")?,
                        face: get_element_info(&reader, "face")?,
                    },
                }
            }

            _ => unimplemented!()
        };

        Ok(info)
    }

    /// Reads the whole file into a temporary storage and gets mesh information
    /// from there (more accurate than only reading the header).
    pub fn from_body(
        format: FileFormat,
        filename: &str,
        file: File,
        _global_args: &GlobalArgs,
        _args: &InfoArgs,
    ) -> Result<(Self, AnyMesh), Error> {
        let (reader, encoding) = reader_and_encoding(format, file)?;

        let mut mesh = AnyMesh::empty();
        progress!(["Reading '{}'", filename] => {
            reader.transfer_to(&mut mesh)?;
            mesh.finish()?;
        });

        let info = Self {
            format,
            encoding,
            mesh: MeshInfo::about_mesh(&mesh),
        };

        Ok((info, mesh))
    }
}

impl MeshInfo {
    /// Gets the information from the given `AnyMesh`.
    pub fn about_mesh(mesh: &AnyMesh) -> Self {
        Self {
            vertex: ElementInfo {
                count: MaybeInfo::Known(mesh.mesh.num_vertices() as u64),
                position_type: MaybeInfo::some_or_none(
                    mesh.vertex_positions.as_ref().map(|m| m.primitive_type())
                ),
                normal_type: MaybeInfo::some_or_none(
                    mesh.vertex_normals.as_ref().map(|m| m.primitive_type())
                ),
                color_type: MaybeInfo::some_or_none(
                    mesh.vertex_colors.as_ref().map(|m| m.color_type())
                ),
            },
            edge: ElementInfo::none(),
            face: ElementInfo {
                count: MaybeInfo::Known(mesh.mesh.num_faces() as u64),
                position_type: MaybeInfo::None,
                normal_type: MaybeInfo::some_or_none(
                    mesh.face_normals.as_ref().map(|m| m.primitive_type())
                ),
                color_type: MaybeInfo::some_or_none(
                    mesh.face_colors.as_ref().map(|m| m.color_type())
                ),
            },
        }
    }

    /// Prints the information as a table.
    pub fn print(&self, _global_args: &GlobalArgs) {
        // ----- Prepare table ---------------------------------------------------
        /// Defines the characters to draw a table.
        #[derive(Copy, Clone)]
        struct TableStyle {
            horizontal: char,
            vertical: char,
            cross: char,
            vertical_double: char,
            cross_vertical_double: char,
            t_cross_right: char,
        }

        /// A nice unicode table.
        const UNICODE_TABLE: TableStyle = TableStyle {
            horizontal: '─',
            vertical: '│',
            cross: '┼',
            vertical_double: '║',
            cross_vertical_double: '╫',
            t_cross_right: '┤',
        };

        /// Takes a 2D array of things that implement `Display` and returns a 2D
        /// String array. This is just a helper to avoid writing `to_string()` a
        /// bunch of times.
        macro_rules! string_table {
            ($style:expr => [ $( [ $($cell:expr),* $(,)? ] ),* $(,)? ]) => {
                vec![
                    $(
                        vec![ $($cell.to_string()),* ],
                    )*
                ]
            }
        }

        let style = UNICODE_TABLE;
        let v = &self.vertex;
        let e = &self.edge;
        let f = &self.face;

        let element_labels = ["vertex ·", "edge   ╱", "face   △"];
        let cells = string_table!(style => [
            ["", "count", "position", "normal", "color"],
            [
                element_labels[0],
                v.count.map(ui::fmt_with_thousand_sep),
                v.position_type.map(vec3_type_str),
                v.normal_type.map(vec3_type_str),
                v.color_type.map(color_type_str),
            ],
            [
                element_labels[2],
                e.count.map(ui::fmt_with_thousand_sep),
                e.position_type.map(vec3_type_str),
                e.normal_type.map(vec3_type_str),
                e.color_type.map(color_type_str),
            ],
            [
                element_labels[2],
                f.count.map(ui::fmt_with_thousand_sep),
                f.position_type.map(vec3_type_str),
                f.normal_type.map(vec3_type_str),
                f.color_type.map(color_type_str),
            ],
        ]);


        let num_rows = cells.len();
        let num_cols = cells[0].len();

        let col_widths = (0..num_cols).map(|col| {
            (0..num_rows).map(|row| cells[row][col].chars().count()).max().unwrap()
        }).collect::<Vec<_>>();


        // ----- Print header ----------------------------------------------------
        for col in 0..num_cols {
            print!(" {: ^1$} ", cells[0][col], col_widths[col]);

            match col {
                0 | 1 => print!("{}", style.vertical_double),
                _ => print!("{}", style.vertical),
            }
        }
        println!();


        // ----- Print separator -------------------------------------------------
        for col in 0..num_cols {
            let line = iter::repeat(style.horizontal)
                .take(col_widths[col] + 2)
                .collect::<String>();
            print!("{}", line);

            match col {
                0 | 1 => print!("{}", style.cross_vertical_double),
                _ if col == num_cols - 1 => print!("{}", style.t_cross_right),
                _ => print!("{}", style.cross),
            }
        }
        println!();


        // ----- Print table body ------------------------------------------------
        let label_style = Color::White.bold();
        let count_style = Color::Green.bold();
        let prop_style = Color::BrightBlue.bold();

        for (&label, &info) in element_labels.iter().zip(&[v, e, f]) {
            print!(" {} {}", label_style.paint(label), style.vertical_double);
            print!(
                " {: >2$} {}",
                info.count.map(ui::fmt_with_thousand_sep).painted(count_style),
                style.vertical_double,
                col_widths[1],
            );

            print!(
                " {: >2$} {}",
                info.position_type.map(vec3_type_str).painted(prop_style),
                style.vertical,
                col_widths[2],
            );
            print!(
                " {: >2$} {}",
                info.normal_type.map(vec3_type_str).painted(prop_style),
                style.vertical,
                col_widths[3],
            );
            print!(
                " {: >2$} {}",
                info.color_type.map(color_type_str).painted(prop_style),
                style.vertical,
                col_widths[4],
            );

            println!();
        }
    }
}


/// A piece of data that is either unknown, not existent or existent.
#[derive(Debug, Clone, Copy)]
pub enum MaybeInfo<T> {
    Unknown,
    None,
    Known(T),
}

impl<T> MaybeInfo<T> {
    /// Creates `Self` from an option, mapping `Option::None` to
    /// `Self::Unknown`.
    pub fn some_or_unknown(x: Option<T>) -> Self {
        match x {
            Some(v) => MaybeInfo::Known(v),
            None => MaybeInfo::Unknown,
        }
    }

    /// Creates `Self` from an option, mapping `Option::None` to `Self::None`.
    pub fn some_or_none(x: Option<T>) -> Self {
        match x {
            Some(v) => MaybeInfo::Known(v),
            None => MaybeInfo::None,
        }
    }

    /// Like `Option::map`: maps the inner value.
    pub fn map<U>(self, mapping: impl FnOnce(T) -> U) -> MaybeInfo<U> {
        match self {
            MaybeInfo::Unknown => MaybeInfo::Unknown,
            MaybeInfo::None => MaybeInfo::None,
            MaybeInfo::Known(v) => MaybeInfo::Known(mapping(v)),
        }
    }

    fn painted(self, style: Style) -> Painted<Self> {
        let style = match &self {
            MaybeInfo::Unknown => Color::BrightRed.to_style(),
            MaybeInfo::None => Color::White.dim(),
            MaybeInfo::Known(_) => style,
        };

        style.paint(self)
    }
}

impl<T: fmt::Display> fmt::Display for MaybeInfo<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MaybeInfo::Unknown => "???".fmt(f),
            MaybeInfo::None => "-".fmt(f),
            MaybeInfo::Known(v) => v.fmt(f),
        }
    }
}

fn type_str(ty: PrimitiveType) -> &'static str {
    match ty {
        PrimitiveType::Uint8 => "u8",
        PrimitiveType::Uint16 => "u16",
        PrimitiveType::Uint32 => "u32",
        PrimitiveType::Int8 => "i8",
        PrimitiveType::Int16 => "i16",
        PrimitiveType::Int32 => "i32",
        PrimitiveType::Float32 => "f32",
        PrimitiveType::Float64 => "f64",
    }
}

fn vec3_type_str(ty: PrimitiveType) -> String {
    format!("3× {}", type_str(ty))
}

fn color_type_str(ty: ColorType) -> String {
    let channels = match ty.alpha {
        true => "RGBA",
        false => "RGB",
    };

    format!("{} {}", channels, type_str(ty.channel_type.to_primitive_type()))
}
