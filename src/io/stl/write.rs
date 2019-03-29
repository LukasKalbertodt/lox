use std::{
    cmp,
    io::{self, Write},
};

use byteorder::{ByteOrder, LittleEndian, WriteBytesExt};
use cgmath::prelude::*;

use crate::{
    traits::*,
    prop::{Pos3Like, Vec3Like},
    io::{PropKind, Error, ErrorKind, StreamSink, MemSource},
};
use super::{Encoding, RawTriangle};


// ----------------------------------------------------------------------------

/// The solid name used when the user didn't specify one.
const DEFAULT_SOLID_NAME: &str = "mesh";


// ===============================================================================================
// ===== STL Config
// ===============================================================================================

/// Used to configure and create a [`Writer`].
///
/// This is used to configure basic settings for the file to be written. You
/// can use the [`Config::into_writer`] method to create a [`Writer`] that can
/// be used as streaming sink.
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

    /// Sets the solid name for this file.
    ///
    /// The given name must be an ASCII string (otherwise the function panics).
    /// If a binary file is written, only 76 bytes of the string are written to
    /// the file.
    pub fn with_solid_name(self, name: impl Into<String>) -> Self {
        let solid_name = name.into();
        assert!(solid_name.is_ascii());

        Self {
            solid_name,
            .. self
        }
    }

    /// Creates a [`Writer`] with `self` as config.
    pub fn into_writer<W: io::Write>(self, writer: W) -> Writer<W> {
        Writer::new(self, writer)
    }
}


// ===============================================================================================
// ===== STL Writer
// ===============================================================================================

/// A writer able to write binary and ASCII STL files. Implements
/// [`StreamSink`].
#[derive(Debug)]
pub struct Writer<W: io::Write> {
    config: Config,
    writer: W,
}

impl<W: io::Write> Writer<W> {
    /// Creates a new STL writer with the given STL config which will write to
    /// the given `io::Write` instance.
    pub fn new(config: Config, writer: W) -> Self {
        Self { config, writer }
    }


    /// Low level function to write STL files.
    ///
    /// You usually don't need to use this function directly and instead use a
    /// high level interface. This function is still exposed to give you more
    /// or less complete control.
    pub fn write_raw(
        self,
        num_triangles: u32,
        triangles: impl IntoIterator<Item = Result<RawTriangle, Error>>,
    ) -> Result<(), Error> {
        if self.config.encoding == Encoding::Ascii {
            self.write_raw_ascii(triangles)
        } else {
            self.write_raw_binary(num_triangles, triangles)
        }
    }

    #[inline(never)]
    pub fn write_raw_binary(
        self,
        num_triangles: u32,
        triangles: impl IntoIterator<Item = Result<RawTriangle, Error>>,
    ) -> Result<(), Error> {
        let config = self.config;
        let mut w = self.writer;

        // First, a 80 bytes useless header that must not begin with "solid".
        // We try to fit the solid name in it.
        let name_len = cmp::min(config.solid_name.len(), 76);
        let signature = format!("LOX {}", &config.solid_name[..name_len]);
        let padding = vec![b' '; 80 - signature.len()];
        w.write_all(signature.as_bytes())?;
        w.write_all(&padding)?;

        // Next, number of triangles
        w.write_u32::<LittleEndian>(num_triangles)?;

        const TRI_SIZE: usize = 4 * 3 * 4 + 2;
        let mut buf = [0; TRI_SIZE];

        for triangle in triangles {
            let triangle = triangle?;

            // Write face normal
            LittleEndian::write_f32(&mut buf[00..04], triangle.normal[0]);
            LittleEndian::write_f32(&mut buf[04..08], triangle.normal[1]);
            LittleEndian::write_f32(&mut buf[08..12], triangle.normal[2]);

            LittleEndian::write_f32(&mut buf[12..16], triangle.vertices[0][0]);
            LittleEndian::write_f32(&mut buf[16..20], triangle.vertices[0][1]);
            LittleEndian::write_f32(&mut buf[20..24], triangle.vertices[0][2]);
            LittleEndian::write_f32(&mut buf[24..28], triangle.vertices[1][0]);
            LittleEndian::write_f32(&mut buf[28..32], triangle.vertices[1][1]);
            LittleEndian::write_f32(&mut buf[32..36], triangle.vertices[1][2]);
            LittleEndian::write_f32(&mut buf[36..40], triangle.vertices[2][0]);
            LittleEndian::write_f32(&mut buf[40..44], triangle.vertices[2][1]);
            LittleEndian::write_f32(&mut buf[44..48], triangle.vertices[2][2]);

            LittleEndian::write_u16(&mut buf[48..50], triangle.attribute_byte_count);

            w.write_all(&buf)?;
        }

        Ok(())
    }

    #[inline(never)]
    pub fn write_raw_ascii(
        self,
        triangles: impl IntoIterator<Item = Result<RawTriangle, Error>>,
    ) -> Result<(), Error> {
        let config = self.config;
        let mut w = self.writer;

        writeln!(w, "solid {}", config.solid_name)?;

        for triangle in triangles {
            let triangle = triangle?;

            // Write face normal
            write!(w, "  facet normal ")?;
            write_ascii_vector(&mut w, triangle.normal)?;
            writeln!(w, "")?;

            // Write all vertex positions
            writeln!(w, "    outer loop")?;
            for &vertex_pos in &triangle.vertices {

                write!(w, "      vertex ")?;
                write_ascii_vector(&mut w, vertex_pos)?;
                writeln!(w, "")?;
            }

            writeln!(w, "    endloop")?;
            writeln!(w, "  endfacet")?;
        }

        writeln!(w, "endsolid {}", config.solid_name)?;

        Ok(())
    }
}

impl<W: io::Write> StreamSink for Writer<W> {
    #[inline(never)]
    fn transfer_from<S: MemSource>(self, src: &S) -> Result<(), Error> {
        // Make sure we have positions
        if src.vertex_position_type().is_none() {
            return Err(Error::new(|| ErrorKind::DataIncomplete {
                prop: PropKind::VertexPosition,
                msg: "source does not provide vertex positions, but STL requires them".into(),
            }));
        }

        let mesh = src.core_mesh();
        let has_normals = src.face_normal_type().is_some();

        // The triangle iterator
        let triangles = mesh.face_handles().map(|fh| {
            let [va, vb, vc] = mesh.vertices_of_face(fh);

            // Get positions from map and convert them to array
            let get_v = |vh| -> Result<[f32; 3], Error> {
                src.vertex_position::<f32>(vh)
                    .and_then(|opt| {
                        opt.ok_or_else(|| Error::new(|| ErrorKind::DataIncomplete {
                            prop: PropKind::VertexPosition,
                            msg: format!("no position for {:?} while writing STL", vh),
                        }))
                    })
                    .map(|p| p.convert())  // to array form
            };
            let vertices = [get_v(va)?, get_v(vb)?, get_v(vc)?];

            let normal = if has_normals {
                src.face_normal::<f32>(fh)?
                    .ok_or_else(|| Error::new(|| ErrorKind::DataIncomplete {
                        prop: PropKind::FaceNormal,
                        msg: format!("no normal for {:?} while writing STL", fh),
                    }))?
                    .convert() // to array form
            } else {
                calc_normal(&vertices)
            };

            Ok(RawTriangle {
                vertices,
                normal,

                // As Wikipedia beautifully put it: "this should be zero
                // because most software does not understand anything else."
                // Great. Some people abuse this to store color or other
                // information. This is terrible, we won't do that.
                attribute_byte_count: 0,
            })
        });

        self.write_raw(mesh.num_faces(), triangles)
    }
}


// ===============================================================================================
// ===== Helper functions
// ===============================================================================================

/// Calculates the normal of the face defined be the three vertices in CCW.
fn calc_normal(positions: &[[f32; 3]; 3]) -> [f32; 3] {
    let pos_a = positions[0].to_point3();
    let pos_b = positions[1].to_point3();
    let pos_c = positions[2].to_point3();

    let normal = (pos_b - pos_a).cross(pos_c - pos_a).normalize();
    [normal.x, normal.y, normal.z]
}


// ===============================================================================================
// ===== Functions for body writing
// ===============================================================================================

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
