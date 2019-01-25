#![feature(try_from)]

#[macro_use]
extern crate structopt;

use std::{
    convert::TryFrom,
    fs::File,
    io::BufWriter,
    time::Instant,
};

use failure::{bail, err_msg, format_err, Error, ResultExt};
use structopt::StructOpt;
use term_painter::{ToStyle, Color};

use lox::{
    cgmath::Point3,
    ds::SharedVertexMesh,
    handle::DefaultInt,
    map::VecMap,
    io::{
        FileEncoding, FileFormat, StreamingSource, MemSink, MemSource,
        StreamingSink, Primitive, PrimitiveType,
        stl,
        ply,
    },
    prelude::*,
};


mod opt;

use crate::opt::Opt;


macro_rules! print {
    ($($t:tt)*) => {{
        use std::io::{self, Write};

        std::print!($($t)*);
        // If an error occurs here... oh well.
        let _ = io::stdout().flush();
    }}
}

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

    let start_time = Instant::now();

    let before_load = Instant::now();
    let mesh_data = load_file(&opt).context("could not read source file")?;
    let load_time = before_load.elapsed();

    print_mesh_info(&mesh_data);

    let before_write = Instant::now();
    write_file(&opt, &mesh_data).context("could not write target file")?;
    let write_time = before_write.elapsed();

    println!(
        "{}: {:.2?} ({:.2?} loading, {:.2?} writing)",
        Color::Blue.bold().paint("⟨ℹ⟩ Processing time"),
        start_time.elapsed(),
        load_time,
        write_time,
    );

    Ok(())
}

fn print_mesh_info(mesh_data: &MeshData) {
    println!("{}", Color::Green.bold().paint("⟨ℹ⟩ Mesh information:"));

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
        "    {} vertices (properties: {})",
        mesh_data.mesh.num_vertices(),
        vertex_props,
    );


    // ===== Face Infos ======================================================
    println!(
        "    {} faces (properties: none)",
        mesh_data.mesh.num_faces(),
    );
}

fn load_file(opt: &Opt) -> Result<MeshData, Error> {
    let file = File::open(&opt.source).context("failed to open file")?;

    // TODO: guess from first 1024 bytes
    // let _file_start = {
    //     let mut v = Vec::new();
    //     file.by_ref().take(1024).read_to_end(&mut v)?;
    //     file.seek(SeekFrom::Start(0))?;
    //     v
    // };

    let file_format = opt.source_format
        .or_else(|| FileFormat::from_extension(&opt.source))
        .ok_or_else(|| err_msg(
            "couldn't determine source file format, please specify it explicitly using \
                '--source-format'"
        ))?;


    let print_info = |encoding: FileEncoding| {
        println!(
            "{}: {} ({} encoding)",
            Color::Blue.bold().paint("⟨ℹ⟩ Source format"),
            file_format,
            encoding_str(encoding),
        );
    };

    // TODO: there is still quite some duplicate code below. Fix that.
    let mut mesh_data = MeshData::new();
    match file_format {
        FileFormat::Ply => {
            let reader = ply::Reader::new(file).context("failed to read PLY header")?;
            print_info(reader.encoding().into());
            print!("⟨￫⟩ Reading source ...");
            reader.transfer_to(&mut mesh_data).context("failed to read PLY body")?;
            println!(" done");
        }
        FileFormat::Stl => {
            let reader = stl::Reader::new(file).context("failed to read STL header")?;
            print_info(reader.encoding().into());
            print!("⟨￫⟩ Reading source ...");
            reader.transfer_to(&mut mesh_data).context("failed to read STL body")?;
            println!(" done");
        }
        _ => bail!("File format '{}' not supported", file_format),
    }

    Ok(mesh_data)
}

fn write_file(opt: &Opt, data: &MeshData) -> Result<(), Error> {
    let file_format = opt.target_format
        .or_else(|| FileFormat::from_extension(&opt.target))
        .ok_or_else(|| err_msg(
            "couldn't determine target file format, please specify it explicitly using \
                '--target-format'"
        ))?;


    println!(
        "{}: {} ({} encoding)",
        Color::Blue.bold().paint("⟨ℹ⟩ Target format"),
        file_format,
        encoding_str(opt.target_encoding),
    );

    let file = BufWriter::new(File::create(&opt.target)?);

    match file_format {
        FileFormat::Ply => {
            unimplemented!()
        }
        FileFormat::Stl => {
            // TODO: check this earlier, before even reading the source file
            let encoding = stl::Encoding::try_from(opt.target_encoding).map_err(|_| {
                format_err!(
                    "the encoding {:?} is not supported by the STL format",
                    opt.target_encoding
                )
            })?;

            print!("⟨￩⟩ Writing mesh ...");

            stl::Config::new(encoding)
                .into_sink(file)
                .transfer_from(data)?;

            println!(" done");
        }
        _ => bail!("File format '{}' not supported", file_format),
    }

    Ok(())
}

fn encoding_str(e: FileEncoding) -> &'static str {
    match e {
        FileEncoding::Ascii => "ASCII",
        FileEncoding::BinaryBigEndian => "binary big endian",
        FileEncoding::BinaryLittleEndian => "binary little endian",
    }
}

#[derive(Debug)]
struct MeshData {
    mesh: SharedVertexMesh,
    vertex_positions: Option<AnyPointMap<VertexHandle>>,
}

impl MeshData {
    fn new() -> Self {
        Self {
            mesh: SharedVertexMesh::new(),
            vertex_positions: None,
        }
    }
}

impl MemSink for MeshData {
    fn add_vertex(&mut self) -> VertexHandle {
        self.mesh.add_vertex()
    }
    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle {
        self.mesh.add_face(vertices)
    }

    fn set_vertex_position<N: Primitive>(
        &mut self,
        handle: VertexHandle,
        position: Point3<N>,
    ) {
        self.vertex_positions
            .get_or_insert_with(|| AnyPointMap::new::<N>())
            .insert(handle, position);
    }
}


impl MemSource for MeshData {
    fn vertices(&self) -> Box<dyn Iterator<Item = VertexHandle> + '_> {
        Box::new(self.mesh.vertices().map(|v| v.handle()))  // TODO: avoid double box
    }
    fn faces(&self) -> Box<dyn Iterator<Item = FaceHandle> + '_> {
        Box::new(self.mesh.faces().map(|f| f.handle())) // TODO: avoid double box
    }

    fn num_vertices(&self) -> DefaultInt {
        self.mesh.num_vertices()
    }
    fn num_faces(&self) -> DefaultInt {
        self.mesh.num_faces()
    }

    fn vertices_of_face(&self, f: FaceHandle) -> [VertexHandle; 3] {
        self.mesh.vertices_of_face(f)
    }

    fn vertex_position_type(&self) -> Option<PrimitiveType> {
        self.vertex_positions.as_ref().map(|m| m.primitive_type())
    }
    fn vertex_position<T: Primitive>(&self, v: VertexHandle) -> Point3<T> {
        self.vertex_positions
            .as_ref()
            .expect("requested non-existent vertex position from `MemSource`")
            .get(v)
            .unwrap_or_else(|| panic!("missing vertex position for {:?}", v))
    }
}

#[derive(Debug)]
enum AnyPointMap<H: Handle> {
    Uint8(VecMap<H, Point3<u8>>),
    Int8(VecMap<H, Point3<i8>>),
    Uint16(VecMap<H, Point3<u16>>),
    Int16(VecMap<H, Point3<i16>>),
    Uint32(VecMap<H, Point3<u32>>),
    Int32(VecMap<H, Point3<i32>>),
    Float32(VecMap<H, Point3<f32>>),
    Float64(VecMap<H, Point3<f64>>),
}

impl<H: Handle> AnyPointMap<H> {
    fn new<T: Primitive>() -> Self {
        match T::TY {
            PrimitiveType::Uint8 => AnyPointMap::Uint8(VecMap::new()),
            PrimitiveType::Int8 => AnyPointMap::Int8(VecMap::new()),
            PrimitiveType::Uint16 => AnyPointMap::Uint16(VecMap::new()),
            PrimitiveType::Int16 => AnyPointMap::Int16(VecMap::new()),
            PrimitiveType::Uint32 => AnyPointMap::Uint32(VecMap::new()),
            PrimitiveType::Int32 => AnyPointMap::Int32(VecMap::new()),
            PrimitiveType::Float32 => AnyPointMap::Float32(VecMap::new()),
            PrimitiveType::Float64 => AnyPointMap::Float64(VecMap::new()),
        }
    }

    fn primitive_type(&self) -> PrimitiveType {
        match self {
            AnyPointMap::Uint8(_) => PrimitiveType::Uint8,
            AnyPointMap::Int8(_) => PrimitiveType::Int8,
            AnyPointMap::Uint16(_) => PrimitiveType::Uint16,
            AnyPointMap::Int16(_) => PrimitiveType::Int16,
            AnyPointMap::Uint32(_) => PrimitiveType::Uint32,
            AnyPointMap::Int32(_) => PrimitiveType::Int32,
            AnyPointMap::Float32(_) => PrimitiveType::Float32,
            AnyPointMap::Float64(_) => PrimitiveType::Float64,
        }
    }

    fn get<T: Primitive>(&self, handle: H) -> Option<Point3<T>> {
        macro_rules! get {
            ($map:ident) => {{
                $map.get(handle).map(|p| {
                    p.map(|s| s.downcast_as().unwrap())
                })
            }}
        }

        // Make sure the inserted type matches the type of the map
        if T::TY != self.primitive_type() {
            panic!(
                "type mismatch requesting '{:?}' from an AnyPointMap with type '{:?}'",
                T::TY,
                self.primitive_type(),
            )
        }

        // Since we know here that the types match, all those `to_*` convert
        // functions won't ever return `None`. In fact, the compiler can
        // probably prove that since it has static type information about `T`.
        match self {
            AnyPointMap::Uint8(map) => get!(map),
            AnyPointMap::Int8(map) => get!(map),
            AnyPointMap::Uint16(map) => get!(map),
            AnyPointMap::Int16(map) => get!(map),
            AnyPointMap::Uint32(map) => get!(map),
            AnyPointMap::Int32(map) => get!(map),
            AnyPointMap::Float32(map) => get!(map),
            AnyPointMap::Float64(map) => get!(map),
        }
    }

    fn insert<T: Primitive>(&mut self, handle: H, pos: Point3<T>) {
        // This function optimizes very well! As seen [here][godbolt], it is
        // just one check of our discriminant against a constant. If the check
        // fails, jump to panic, otherwise the value is just pushed to the
        // vector. Optimal, so to speak!
        //
        // [godbolt]: https://rust.godbolt.org/z/dyWROg

        macro_rules! insert {
            ($map:ident, $convert:ident) => {{
                $map.insert(handle, pos.map(|s| s.downcast_as().unwrap()));
            }}
        }

        // Make sure the inserted type matches the type of the map
        if T::TY != self.primitive_type() {
            panic!(
                "type mismatch inserting '{:?}' into an AnyPointMap with type '{:?}'",
                T::TY,
                self.primitive_type(),
            )
        }

        // Since we know here that the types match, all those `to_*` convert
        // functions won't ever return `None`. In fact, the compiler can
        // probably prove that since it has static type information about `T`.
        match self {
            AnyPointMap::Uint8(map) => insert!(map, to_u8),
            AnyPointMap::Int8(map) => insert!(map, to_i8),
            AnyPointMap::Uint16(map) => insert!(map, to_u16),
            AnyPointMap::Int16(map) => insert!(map, to_i16),
            AnyPointMap::Uint32(map) => insert!(map, to_u32),
            AnyPointMap::Int32(map) => insert!(map, to_i32),
            AnyPointMap::Float32(map) => insert!(map, to_f32),
            AnyPointMap::Float64(map) => insert!(map, to_f64),
        }
    }
}
