#[macro_use]
extern crate structopt;

use std::{
    fs::File,
    io::{Seek, SeekFrom, Read},
    time::Instant,
};

use failure::{err_msg, Error, ResultExt};
use structopt::StructOpt;

use lox::{
    cgmath::Point3,
    ds::SharedVertexMesh,
    handle::DefaultInt,
    map::VecMap,
    io::{
        FileFormat, StreamingSource, MemSink, MemSource, StreamingSink,
        Primitive, PrimitiveType,
        stl,
        ply,
    },
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
    // println!("{:?}", opt);

    let start_time = Instant::now();

    // Load file
    let mesh_data = load_file(&opt).context("could not read source file")?;

    // println!("{:#?}", mesh_data);
    println!(
        "vertices: {}, faces: {}",
        mesh_data.mesh.num_vertices(),
        mesh_data.mesh.num_faces(),
    );

    write_file(&opt, &mesh_data)?;

    println!("Processing time: {:.2?}", start_time.elapsed());

    Ok(())
}

fn load_file(opt: &Opt) -> Result<MeshData, Error> {
    let mut file = File::open(&opt.source).context("failed to open file")?;
    let _file_start = {
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

    let mut mesh_data = MeshData::new();
    match file_format {
        FileFormat::Ply => {
            let reader = ply::Reader::new(file).context("failed to read PLY header")?;
            reader.transfer_to(&mut mesh_data).context("failed to read PLY body")?;

        }
        FileFormat::Stl => {
            let reader = stl::Reader::new(file).context("failed to read STL header")?;
            reader.transfer_to(&mut mesh_data).context("failed to read STL body")?;
        }
    }

    Ok(mesh_data)
}

fn write_file(opt: &Opt, data: &MeshData) -> Result<(), Error> {
    let file_format = opt.source_format
        .or_else(|| FileFormat::from_extension(&opt.source))
        .ok_or_else(|| err_msg(
            "couldn't determine target file format, please specify it explicitly using \
                '--target-format'"
        ))?;

    match file_format {
        FileFormat::Ply => {
            unimplemented!()
        }
        FileFormat::Stl => {
            stl::Config::binary()
                .into_sink(File::create(&opt.target)?)
                .transfer_from(data)?;
        }
    }

    Ok(())
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
