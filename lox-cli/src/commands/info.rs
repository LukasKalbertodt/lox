use std::{
    fmt,
    fs::File,
    iter,
};

use failure::{Error, ResultExt};
use term_painter::{Color, ToStyle, Style, Painted};
use lox::{
    io::{
        stl, ply,
        ColorType, FileFormat, FileEncoding, PrimitiveType, PrimitiveColorChannelType,
    },
};

use crate::{
    args::{GlobalArgs, InfoArgs},
    commands::guess_file_format,
    ui,
};

pub fn run(global_args: &GlobalArgs, args: &InfoArgs) -> Result<(), Error> {
    // Open file and figure out file format
    let filename = &args.file;
    let mut file = File::open(filename)
        .context(format!("failed to open file '{}'", filename))?;
    let format = guess_file_format(args.source_format, filename, &mut file)?;

    let header_is_sufficient = match format {
        FileFormat::Stl => false,
        FileFormat::Ply => true,
        _ => unimplemented!(),
    };

    let info = if args.header_only || (header_is_sufficient && !args.read_body) {
        info_from_header(format, file, global_args, args)?
    } else {
        unimplemented!()
    };

    print_info(&info, args);

    Ok(())
}



/// Gets information from just reading the header of the input file.
fn info_from_header(
    format: FileFormat,
    file: File,
    _global_args: &GlobalArgs,
    args: &InfoArgs,
) -> Result<Info, Error> {
    // Get information. This is different for each format.
    let err_read_header = format!("failed to read {} header", format);
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
                    let position_type = def.check_vec3_prop(["x", "y", "z"], "positions")?
                        .map(|(_, ty)| ty.to_primitive_type());
                    let normal_type = def.check_vec3_prop(["nx", "ny", "nz"], "normals")?
                        .map(|(_, ty)| ty.to_primitive_type());
                    let color_type = def.check_color_prop()?.map(|(_, alpha)| ColorType {
                        alpha,
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
                vertex: get_element_info(&reader, "vertex")?,
                edge: get_element_info(&reader, "edge")?,
                face: get_element_info(&reader, "face")?,
            }
        }

        _ => unimplemented!()
    };

    Ok(info)
}

/// Pretty prints all the information.
fn print_info(info: &Info, _args: &InfoArgs) {
    // ----- Print file format and encoding ----------------------------------
    println!(
        "File format: {} (encoding: {})",
        Color::BrightWhite.bold().paint(info.format),
        Color::BrightWhite.paint(info.encoding),
    );


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
    let v = &info.vertex;
    let e = &info.edge;
    let f = &info.face;

    let element_labels = ["vertex ·", "edge   ╱", "face   △"];
    let cells = string_table!(style => [
        ["", "count", "position", "normal", "color"],
        [
            element_labels[0],
            v.count.map(ui::fmt_with_thousand_sep),
            v.position_type.map(type_str),
            v.normal_type.map(type_str),
            v.color_type.map(color_type_str),
        ],
        [
            element_labels[2],
            e.count.map(ui::fmt_with_thousand_sep),
            e.position_type.map(type_str),
            e.normal_type.map(type_str),
            e.color_type.map(color_type_str),
        ],
        [
            element_labels[2],
            f.count.map(ui::fmt_with_thousand_sep),
            f.position_type.map(type_str),
            f.normal_type.map(type_str),
            f.color_type.map(color_type_str),
        ],
    ]);


    let num_rows = cells.len();
    let num_cols = cells[0].len();

    let col_widths = (0..num_cols).map(|col| {
        (0..num_rows).map(|row| cells[row][col].chars().count()).max().unwrap()
    }).collect::<Vec<_>>();


    // ----- Print header ----------------------------------------------------
    println!();
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
            info.position_type.map(type_str).painted(prop_style),
            style.vertical,
            col_widths[2],
        );
        print!(
            " {: >2$} {}",
            info.normal_type.map(type_str).painted(prop_style),
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


/// All the standard info we can get about a mesh in a file.
#[derive(Debug)]
struct Info {
    format: FileFormat,
    encoding: FileEncoding,
    vertex: ElementInfo,
    edge: ElementInfo,
    face: ElementInfo,
}

/// Information about one element (vertex, edge, face).
#[derive(Debug)]
struct ElementInfo {
    count: MaybeInfo<u64>,
    position_type: MaybeInfo<PrimitiveType>,
    normal_type: MaybeInfo<PrimitiveType>,
    color_type: MaybeInfo<ColorType>,
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

/// A piece of data that is either unknown, not existent or existent.
#[derive(Debug, Clone, Copy)]
enum MaybeInfo<T> {
    Unknown,
    None,
    Known(T),
}

impl<T> MaybeInfo<T> {
    fn some_or_unknown(x: Option<T>) -> Self {
        match x {
            Some(v) => MaybeInfo::Known(v),
            None => MaybeInfo::Unknown,
        }
    }

    fn some_or_none(x: Option<T>) -> Self {
        match x {
            Some(v) => MaybeInfo::Known(v),
            None => MaybeInfo::None,
        }
    }

    fn map<U>(self, mapping: impl FnOnce(T) -> U) -> MaybeInfo<U> {
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

fn color_type_str(ty: ColorType) -> String {
    let channels = match ty.alpha {
        true => "RGBA",
        false => "RGB",
    };

    format!("{} {}", channels, type_str(ty.channel_type.to_primitive_type()))
}
