use std::{
    io::{self, Write},
};

use byteorder::{LittleEndian, WriteBytesExt};
use cgmath::prelude::*;

use crate::{
    TriVerticesOfFace, TriMesh,
    handle::{FaceHandle, VertexHandle},
    map::{EmptyMap, FacePropMap, VertexPropMap},
    prop::{Pos3Like, Vec3Like},
    io::{Error, StreamSink, MemSource, PrimitiveType},
    // io::{Error, IntoMeshWriter, MeshWriter, StreamSink, MemSource, PrimitiveType},
};
use super::Encoding;


const DEFAULT_SOLID_NAME: &str = "mesh";
// type DummyMap = EmptyMap<[f32; 3]>;


// ===============================================================================================
// ===== STL Config
// ===============================================================================================

/// Used to configure and create a [`Writer`] or [`Sink`].
///
/// This is used to configure basic settings for the file to be written. Use
/// the [`IntoMeshWriter`][crate::io::IntoMeshWriter] implementation to obtain
/// a writer or the [`Config::into_sink`] method to create a streaming sink.
#[derive(Clone, Debug)]
pub struct Config {
    solid_name: String,
    encoding: Encoding,
}

impl Config {
    /// Creates a new builder instance from the given encoding. For
    /// convenience, you can use [`Config::binary()`] or [`Config::ascii()`]
    /// directly.
    pub fn new(encoding: Encoding) -> Self {
        Self {
            solid_name: DEFAULT_SOLID_NAME.into(),
            encoding,
        }
    }

    /// Creates a new builder instance for a binary STL file.
    pub fn binary() -> Self {
        Self::new(Encoding::Binary)
    }

    /// Creates a new builder instance for an ASCII STL file.
    ///
    /// **Note**: please don't use this. STL ASCII files are even more space
    /// inefficient than binary STL files. If you can avoid it, never use ASCII
    /// STL. In fact, consider not using STL at all.
    pub fn ascii() -> Self {
        Self::new(Encoding::Ascii)
    }

    /// Sets the solid name for this file. This is only used for ASCII files!
    pub fn with_solid_name(self, name: impl Into<String>) -> Self {
        Self {
            solid_name: name.into(),
            .. self
        }
    }

    /// Creates a streaming sink.
    pub fn into_sink<W: io::Write>(self, writer: W) -> Sink<W> {
        Sink {
            config: self,
            writer,
        }
    }
}

// impl<'a, MeshT, PosM> IntoMeshWriter<'a, MeshT, PosM> for Config
// where
//     MeshT: 'a + TriMesh + TriVerticesOfFace,
//     PosM: 'a + VertexPropMap,
//     PosM::Target: Pos3Like<Scalar = f32>,
// {
//     type Writer = Writer<'a, MeshT, PosM, DummyMap>;
//     fn into_writer(self, mesh: &'a MeshT, vertex_positions: &'a PosM) -> Self::Writer {
//         Writer {
//             config: self,
//             mesh,
//             vertex_positions,
//             face_normals: None,
//         }
//     }
// }


// ===============================================================================================
// ===== STL Sink
// ===============================================================================================

/// The [`StreamSink`] for STL files. Is created via [`Config::into_sink`].
#[derive(Debug)]
pub struct Sink<W: io::Write> {
    config: Config,
    writer: W,
}

impl<W: io::Write> StreamSink for Sink<W> {
    fn transfer_from<S: MemSource>(self, src: &S) -> Result<(), Error> {
        macro_rules! pos_fn {
            ($orig_type:ident) => {{
                let fun = |vh| {
                    src.vertex_position::<$orig_type>(vh)
                        .map(|s| s as f32) // TODO cast via `cast` module
                        .convert()
                };
                Box::new(fun) as Box<dyn Fn(VertexHandle) -> [f32; 3]>
            }}
        }

        let pos_type = src.vertex_position_type().expect("fucky wucky"); // TODO
        let vertex_positions = match pos_type {
            PrimitiveType::Uint8 => pos_fn!(u8),
            PrimitiveType::Int8 => pos_fn!(i8),
            PrimitiveType::Uint16 => pos_fn!(u16),
            PrimitiveType::Int16 => pos_fn!(i16),
            PrimitiveType::Uint32 => pos_fn!(u32),
            PrimitiveType::Int32 => pos_fn!(i32),
            PrimitiveType::Float32 => pos_fn!(f32),
            PrimitiveType::Float64 => pos_fn!(f64),
        };

        write(
            self.writer,
            &self.config,
            src.num_faces(),
            src.faces(),
            |fh| src.vertices_of_face(fh),
            |vh| Some(vertex_positions(vh)),

            // TODO: use real normals once `MemSource` provides them
            |_, positions| Some(calc_normal(positions)),
        )
    }
}


// ===============================================================================================
// ===== STL Writer
// ===============================================================================================

// /// A writer able to write binary and ASCII STL files.
// ///
// /// To create a writer, you need to create a [`Config`] first (probably via
// /// `Config::binary()`) and call `into_writer(..)` on it. Once you have a
// /// writer, you can optionally add face normals to it. If you don't add your
// /// own face normals, normals are calculated from the vertex positions on the
// /// fly (this requires every face to have a non-zero area!).
// ///
// /// You can then actually write data via the
// /// [`MeshWriter`][crate::io::MeshWriter] trait.
// ///
// /// Don't be scared by all the generic parameters and trait bounds. For the
// /// most part, you don't need to worry about that at all.
// #[derive(Debug)]
// pub struct Writer<'a, MeshT, PosM, NormalM>
// where
//     MeshT: TriMesh + TriVerticesOfFace,
//     PosM: VertexPropMap,
//     PosM::Target: Pos3Like<Scalar = f32>,
//     NormalM: FacePropMap,
//     NormalM::Target: Vec3Like<Scalar = f32>,
// {
//     config: Config,
//     mesh: &'a MeshT,
//     vertex_positions: &'a PosM,
//     face_normals: Option<&'a NormalM>,
// }

// impl<'a, MeshT, PosM> Writer<'a, MeshT, PosM, DummyMap>
// where // TODO: remove once implied bounds land
//     MeshT: TriMesh + TriVerticesOfFace,
//     PosM: VertexPropMap,
//     PosM::Target: Pos3Like<Scalar = f32>,
// {
//     /// Instructs the writer to use the given face normals instead of
//     /// calculating normals on the fly. If any of your faces have a zero area,
//     /// you need to call that as automatically calculating normals won't work.
//     pub fn with_face_normals<NormalM>(
//         self,
//         face_normals: &'a NormalM,
//     ) -> Writer<'a, MeshT, PosM, NormalM>
//     where
//         NormalM: FacePropMap,
//         NormalM::Target: Vec3Like<Scalar = f32>,
//     {
//         Writer {
//             config: self.config,
//             mesh: self.mesh,
//             vertex_positions: self.vertex_positions,
//             face_normals: Some(face_normals),
//         }
//     }
// }


// impl<MeshT, PosM, NormalM> MeshWriter for Writer<'_, MeshT, PosM, NormalM>
// where // TODO: remove once implied bounds land
//     MeshT: TriMesh + TriVerticesOfFace,
//     PosM: VertexPropMap,
//     PosM::Target: Pos3Like<Scalar = f32>,
//     NormalM: FacePropMap,
//     NormalM::Target: Vec3Like<Scalar = f32>,
// {
//     type Error = Error;

//     fn write_to(&self, w: impl Write) -> Result<(), Self::Error> {
//         write(
//             w,
//             &self.config,
//             self.mesh.num_faces(),
//             self.mesh.faces().map(|face| face.handle()),
//             |fh| self.mesh.vertices_of_face(fh),
//             |vh| self.vertex_positions.get(vh).map(|p| p.convert()),
//             |fh, positions| {
//                 self.face_normals
//                     .map(|normals| normals.get(fh).map(|n| n.convert()))
//                     .unwrap_or_else(|| Some(calc_normal(positions)))
//             },
//         )
//     }
// }


// ===============================================================================================
// ===== Functions for body writing
// ===============================================================================================

/// Actual writing implementation.
fn write(
    mut w: impl Write,
    config: &Config,
    num_faces: u32,
    faces: impl Iterator<Item = FaceHandle>,
    vertices_of_face: impl Fn(FaceHandle) -> [VertexHandle; 3],
    positions: impl Fn(VertexHandle) -> Option<[f32; 3]>,
    normals: impl Fn(FaceHandle, &[[f32; 3]; 3]) -> Option<[f32; 3]>,
) -> Result<(), Error> {
    // Retrieves the positions and normal of the given face.
    let get_pos_and_normal = |face_handle| -> ([[f32; 3]; 3], [f32; 3]) {
        let [va, vb, vc] = vertices_of_face(face_handle);

        // Get positions from map and convert them to array
        let get = |h| -> [f32; 3] {
            positions(h)
                .unwrap_or_else(|| panic!("no position for {:?} while writing STL", h))
        };
        let positions = [get(va), get(vb), get(vc)];

        // If a normal map was specified, retrieve the normal from there,
        // otherwise calculate it via cross product.
        let normal = normals(face_handle, &positions)
            .unwrap_or_else(|| panic!("no normal for {:?} while writing STL", face_handle));

        (positions, normal)
    };

    if config.encoding == Encoding::Ascii {
        // ===============================================================
        // ===== STL ASCII
        // ===============================================================
        writeln!(w, "solid {}", config.solid_name)?;

        for face_handle in faces {
            let (positions, normal) = get_pos_and_normal(face_handle);

            // Write face normal
            write!(w, "  facet normal ")?;
            write_ascii_vector(&mut w, normal)?;
            writeln!(w, "")?;

            // Write all vertex positions
            writeln!(w, "    outer loop")?;
            for &vertex_pos in &positions {

                write!(w, "      vertex ")?;
                write_ascii_vector(&mut w, vertex_pos)?;
                writeln!(w, "")?;
            }

            writeln!(w, "    endloop")?;
            writeln!(w, "  endfacet")?;
        }

        writeln!(w, "endsolid {}", config.solid_name)?;
    } else {
        // ===============================================================
        // ===== STL binary
        // ===============================================================
        // First, a 80 bytes useless header that must not begin with
        // "solid"!
        let signature = b"LOX   ... PLY specs are terrible ...";
        let padding = vec![b' '; 80 - signature.len()];
        w.write_all(signature)?;
        w.write_all(&padding)?;

        // Next, number of triangles
        w.write_u32::<LittleEndian>(num_faces)?;

        for face_handle in faces {
            let (positions, [nx, ny, nz]) = get_pos_and_normal(face_handle);

            // Write face normal
            w.write_f32::<LittleEndian>(nx)?;
            w.write_f32::<LittleEndian>(ny)?;
            w.write_f32::<LittleEndian>(nz)?;

            // Write all vertex positions
            for &[x, y, z] in &positions {
                w.write_f32::<LittleEndian>(x)?;
                w.write_f32::<LittleEndian>(y)?;
                w.write_f32::<LittleEndian>(z)?;
            }

            // Write "attribute byte count". As Wikipedia beautifully put
            // it: "this should be zero because most software does not
            // understand anything else." Great. Some people abuse this to
            // store color or other information. This is terrible, we won't
            // do that.
            w.write_u16::<LittleEndian>(0)?;
        }
    }

    Ok(())
}

fn calc_normal(positions: &[[f32; 3]; 3]) -> [f32; 3] {
        let pos_a = positions[0].to_point3();
        let pos_b = positions[1].to_point3();
        let pos_c = positions[2].to_point3();

        let normal = (pos_b - pos_a).cross(pos_c - pos_a).normalize();
        [normal.x, normal.y, normal.z]
}

/// Writes the three values of the given vector (in STL ASCII encoding,
/// separated
/// by ' ') into the writer.
fn write_ascii_vector(w: &mut impl Write, [x, y, z]: [f32; 3]) -> Result<(), io::Error> {
    write_ascii_f32(w, x)?;
    write!(w, " ")?;
    write_ascii_f32(w, y)?;
    write!(w, " ")?;
    write_ascii_f32(w, z)?;

    Ok(())
}

/// Writes the given `f32` in STL ASCII format into the given writer.
///
/// The STL specification is terribly underspecified. The only information
/// about how to encode floats in ASCII is this:
///
/// > The numerical data in the facet normal and vertex lines are single
/// > precision floats, for example, 1.23456E+789. A facet normal coordinate
/// > may have a leading minus sign; a vertex coordinate may not.
///
/// I don't think the last sentence makes any sense: why forbid negative
/// coordinates? In any case, no one in the real world cares about that: there
/// are plenty of STL files out there with negative vertex coordinates.
///
/// About the actual format: clearly unhelpful. In real world, STL files floats
/// are encoded all over the place. I've seen `1`, `1.2`, `10.2`, `1.02e1`,
/// `1.020000E+001` and more. We just stick to the exact format mentioned in
/// the "specification". This does not necessarily make any sense and wastes
/// memory, but so does ASCII STL. Just don't use the ASCII STL format!
fn write_ascii_f32(w: &mut impl Write, v: f32) -> Result<(), io::Error> {
    use std::num::FpCategory;

    match v.classify() {
        FpCategory::Normal | FpCategory::Subnormal => {
            let exponent = v.abs().log10().floor();
            let mantissa = v / 10f32.powf(exponent);
            write!(w, "{}E{:+}", mantissa, exponent)
        }
        _ => {
            // `v` is either infinite, `NaN` or zero. We want to serialize
            // the zeroes as `0.0`.
            write!(w, "{:.1}", v)
        }
    }
}
