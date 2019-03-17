//! Everything related to writing a PLY file.
//!
//! # Random notes on the format
//!
//! Unfortunately, the PLY format is terribly underspecified (as are most mesh
//! formats). Therefore, here are a few notes on missing information or this
//! particular implementation. You will find more comments on this below.
//!
//! - The specs say "The header is a series of carriage-return terminated
//!   lines", but the example files used by the specs and all files in the wild
//!   use `'\n'` as terminator and not `'\r'` (carriage-return).
//!
//!


use std::{
    io::{self, Write},
};

use byteorder::{BigEndian, LittleEndian, WriteBytesExt};

use crate::{
    handle::{hsize, FaceHandle, Handle, VertexHandle},
    io::{
        Error, StreamSink, MemSource, Primitive, PrimitiveType, PropKind,
        util::HandleIndexMap,
    },
    traits::*,
    util::TriArrayExt,
};
use super::{
    Encoding,
    raw::{
        ElementDef, RawSource, Serializer, PropVec, PropertyDef, PropertyType,
        ScalarType, ListLenType,
    },
};



// ===============================================================================================
// ===== PLY Config
// ===============================================================================================

#[derive(Clone, Debug)]
pub struct Config {
    encoding: Encoding,
    comments: Vec<String>,
}

impl Config {
    pub fn ascii() -> Self {
        Self::new(Encoding::Ascii)
    }

    pub fn binary() -> Self {
        Self::new(Encoding::BinaryBigEndian)
    }

    pub fn new(encoding: Encoding) -> Self {
        Self {
            encoding,
            comments: vec![],
        }
    }

    /// Adds a `comment` line to the file header. The given string must not
    /// contain `'\n'` or else this method panics.
    pub fn add_comment(mut self, comment: impl Into<String>) -> Self {
        let comment = comment.into();

        assert!(!comment.contains('\n'), "PLY comments must not contain '\\n'!");

        self.comments.push(comment);
        self
    }

    pub fn into_writer<W: io::Write>(self, writer: W) -> Writer<W> {
        Writer {
            config: self,
            writer,
        }
    }
}


// ===============================================================================================
// ===== PLY Sink
// ===============================================================================================
/// A writer able to write binary and ASCII PLY files. Implements
/// [`StreamSink`].
#[derive(Debug)]
pub struct Writer<W: io::Write> {
    config: Config,
    writer: W,
}

impl<W: io::Write> Writer<W> {
    /// Creates a new PLY writer with the given PLY config which will write to
    /// the given `io::Write` instance.
    pub fn new(config: Config, writer: W) -> Self {
        Self { config, writer }
    }

    /// Low level function to write PLY files.
    ///
    /// You usually don't need to use this function directly and instead use a high
    /// level interface. This function is still exposed to give you more or less
    /// complete control.
    pub fn write_raw(
        mut self,
        header: &[ElementDef],
        source: impl RawSource,
    ) -> Result<(), Error> {
        let w = &mut self.writer;

        // ===================================================================
        // ===== Write header (this part is always ASCII)
        // ===================================================================
        // Magic signature
        w.write_all(b"ply\n")?;

        // The line defining the format of the file
        let format_line = match self.config.encoding {
            Encoding::Ascii => b"format ascii 1.0\n" as &[_],
            Encoding::BinaryBigEndian => b"format binary_big_endian 1.0\n",
            Encoding::BinaryLittleEndian => b"format binary_little_endian 1.0\n",
        };
        w.write_all(format_line)?;

        // Add all comments
        for comment in &self.config.comments {
            writeln!(w, "comment {}", comment)?;
        }

        // Define all elements with their properties
        println!("{:#?}", header);
        for element_def in header {
            writeln!(w, "element {} {}", element_def.name, element_def.count)?;
            for prop in &*element_def.property_defs {
                match prop.ty {
                    PropertyType::Scalar(ty) => {
                        writeln!(w, "property {} {}", ty.ply_type_name(), prop.name)?;
                    }
                    PropertyType::List { scalar_type, len_type } => {
                        writeln!(
                            w,
                            "property list {} {} {}",
                            len_type.to_scalar_type().ply_type_name(),
                            scalar_type.ply_type_name(),
                            prop.name,
                        )?;

                    }
                }
            }
        }

        w.write_all(b"end_header\n")?;


        // ===================================================================
        // ===== Write body
        // ===================================================================

        match self.config.encoding {
            Encoding::Ascii => source.serialize_into(AsciiSerializer::new(w)),
            Encoding::BinaryBigEndian => source.serialize_into(BinaryBeSerializer::new(w)),
            Encoding::BinaryLittleEndian => source.serialize_into(BinaryLeSerializer::new(w)),
        }
    }
}

impl<W: io::Write> StreamSink for Writer<W> {
    fn transfer_from<S: MemSource>(self, src: &S) -> Result<(), Error> {
        // Random notes:
        // - The order of header fields have to match the order of adding
        //   properties

        let mesh = src.core_mesh();

        // ====================================================================
        // ===== Create header description
        // ====================================================================

        // ----- Vertex element -----------------
        let mut vertex_def = ElementDef {
            name: "vertex".into(),
            count: mesh.num_vertices().into(),
            property_defs: PropVec::new(),
        };

        // Position: x y z
        if let Some(ty) = src.vertex_position_type() {
            let ty = PropertyType::Scalar(closest_ply_type(ty));
            vertex_def.property_defs.push(PropertyDef { ty, name: "x".into() });
            vertex_def.property_defs.push(PropertyDef { ty, name: "y".into() });
            vertex_def.property_defs.push(PropertyDef { ty, name: "z".into() });
        }

        // Normal: nx ny nz
        if let Some(ty) = src.vertex_normal_type() {
            let ty = PropertyType::Scalar(closest_ply_type(ty));
            vertex_def.property_defs.push(PropertyDef { ty, name: "nx".into() });
            vertex_def.property_defs.push(PropertyDef { ty, name: "ny".into() });
            vertex_def.property_defs.push(PropertyDef { ty, name: "nz".into() });
        }


        // ----- Face element -----------------
        let mut face_def = ElementDef {
            name: "face".into(),
            count: mesh.num_faces().into(),
            property_defs: PropVec::new(),
        };

        // Connectivity: vertex_indices
        face_def.property_defs.push(PropertyDef {
            ty: PropertyType::List {
                len_type: ListLenType::UChar,
                scalar_type: ScalarType::UInt,
            },
            name: "vertex_indices".into(),
        });

        // Normal: nx ny nz
        if let Some(ty) = src.face_normal_type() {
            let ty = PropertyType::Scalar(closest_ply_type(ty));
            face_def.property_defs.push(PropertyDef { ty, name: "nx".into() });
            face_def.property_defs.push(PropertyDef { ty, name: "ny".into() });
            face_def.property_defs.push(PropertyDef { ty, name: "nz".into() });
        }


        // ====================================================================
        // ===== Body writing implementation
        // ====================================================================

        struct HelperSource<'a, SrcT: MemSource>(&'a SrcT);

        impl<SrcT: MemSource> RawSource for HelperSource<'_, SrcT> {
            fn serialize_into<S: Serializer>(self, mut ser: S) -> Result<(), Error> {
                let src = self.0;
                let mesh = src.core_mesh();

                // ===========================================================
                // ===== Prepare function pointers
                // ===========================================================
                fn vertex_noop<S, SrcT>(
                    _: &mut S,
                    _: &SrcT,
                    _: VertexHandle,
                ) -> Result<(), Error> {
                    Ok(())
                }

                fn face_noop<S, SrcT>(
                    _: &mut S,
                    _: &SrcT,
                    _: FaceHandle,
                ) -> Result<(), Error> {
                    Ok(())
                }

                macro_rules! make_fn_ptr {
                    ($ty:expr, $fun:ident, $noop:ident $(,)?) => {
                        match $ty {
                            None => $noop::<S, SrcT>,
                            Some(ScalarType::Char) => $fun::<S, SrcT, i8>,
                            Some(ScalarType::Short) => $fun::<S, SrcT, i16>,
                            Some(ScalarType::Int) => $fun::<S, SrcT, i32>,
                            Some(ScalarType::UChar) => $fun::<S, SrcT, u8>,
                            Some(ScalarType::UShort) => $fun::<S, SrcT, u16>,
                            Some(ScalarType::UInt) => $fun::<S, SrcT, u32>,
                            Some(ScalarType::Float) => $fun::<S, SrcT, f32>,
                            Some(ScalarType::Double) => $fun::<S, SrcT, f64>,
                        }
                    }
                }

                // ----- Vertex positions ------------------------------------
                fn write_vertex_position<S: Serializer, SrcT: MemSource, P: Primitive>(
                    ser: &mut S,
                    src: &SrcT,
                    handle: VertexHandle,
                ) -> Result<(), Error> {
                    let pos = src.vertex_position::<P>(handle).and_then(|opt| {
                        opt.ok_or_else(|| Error::DataIncomplete {
                            prop: PropKind::VertexPosition,
                            msg: format!("no position for {:?} while writing PLY", handle),
                        })
                    })?;

                    ser.add(pos.x)?;
                    ser.add(pos.y)?;
                    ser.add(pos.z)?;

                    Ok(())
                }

                let write_v_position = make_fn_ptr!(
                    src.vertex_position_type().map(closest_ply_type),
                    write_vertex_position,
                    vertex_noop,
                );

                // ----- Vertex normals --------------------------------------
                fn write_vertex_normal<S: Serializer, SrcT: MemSource, P: Primitive>(
                    ser: &mut S,
                    src: &SrcT,
                    handle: VertexHandle,
                ) -> Result<(), Error> {
                    let pos = src.vertex_normal::<P>(handle).and_then(|opt| {
                        opt.ok_or_else(|| Error::DataIncomplete {
                            prop: PropKind::VertexNormal,
                            msg: format!("no normal for {:?} while writing PLY", handle),
                        })
                    })?;

                    ser.add(pos.x)?;
                    ser.add(pos.y)?;
                    ser.add(pos.z)?;

                    Ok(())
                }

                let write_v_normal = make_fn_ptr!(
                    src.vertex_normal_type().map(closest_ply_type),
                    write_vertex_normal,
                    vertex_noop,
                );

                // ----- Face normals ----------------------------------------
                fn write_face_normal<S: Serializer, SrcT: MemSource, P: Primitive>(
                    ser: &mut S,
                    src: &SrcT,
                    handle: FaceHandle,
                ) -> Result<(), Error> {
                    let pos = src.face_normal::<P>(handle).and_then(|opt| {
                        opt.ok_or_else(|| Error::DataIncomplete {
                            prop: PropKind::FaceNormal,
                            msg: format!("no normal for {:?} while writing PLY", handle),
                        })
                    })?;

                    ser.add(pos.x)?;
                    ser.add(pos.y)?;
                    ser.add(pos.z)?;

                    Ok(())
                }

                let write_f_normal = make_fn_ptr!(
                    src.face_normal_type().map(closest_ply_type),
                    write_face_normal,
                    face_noop,
                );


                // ===========================================================
                // ===== Write all the data
                // ===========================================================
                let mut indices_map = HandleIndexMap::new();

                for (i, vh) in mesh.vertex_handles().enumerate() {
                    indices_map.add(vh, i as hsize);

                    write_v_position(&mut ser, src, vh)?;
                    write_v_normal(&mut ser, src, vh)?;

                    ser.end_element()?;
                }

                for fh in mesh.face_handles() {
                    // Vertex indices
                    let [a, b, c] = mesh.vertices_of_face(fh)
                        .map(|vh| indices_map.get(vh).unwrap());
                    ser.add_u8(3)?;
                    ser.add_u32(a)?;
                    ser.add_u32(b)?;
                    ser.add_u32(c)?;

                    // Other face properties
                    write_f_normal(&mut ser, src, fh)?;

                    ser.end_element()?;
                }

                Ok(())
            }
        }

        // ...
        self.write_raw(&[vertex_def, face_def], HelperSource(src))
    }
}

/// Returns the PLY scalar type that most closely matches the given
/// `PrimitiveType`. Right now, the mapping is 1:1 (always perfect), but this
/// might change when additional primitive types are added.
fn closest_ply_type(ty: PrimitiveType) -> ScalarType {
    match ty {
        PrimitiveType::Uint8 => ScalarType::UChar,
        PrimitiveType::Int8 => ScalarType::Char,
        PrimitiveType::Uint16 => ScalarType::UShort,
        PrimitiveType::Int16 => ScalarType::Short,
        PrimitiveType::Uint32 => ScalarType::UInt,
        PrimitiveType::Int32 => ScalarType::Int,
        PrimitiveType::Float32 => ScalarType::Float,
        PrimitiveType::Float64 => ScalarType::Double,
    }
}


// ===============================================================================================
// ===== Definition of ASCII and binary serializers
// ===============================================================================================

#[derive(Debug)]
struct AsciiSerializer<'a, W: Write> {
    writer: &'a mut W,
    at_start_of_line: bool,
}

impl<'a, W: Write> AsciiSerializer<'a, W> {
    fn new(w: &'a mut W) -> Self {
        Self {
            writer: w,
            at_start_of_line: true,
        }
    }

    fn write_seperator(&mut self) -> Result<(), Error> {
        if self.at_start_of_line {
            self.at_start_of_line = false;
        } else {
            self.writer.write_all(b" ")?;
        }

        Ok(())
    }
}

impl<W: io::Write> Serializer for AsciiSerializer<'_, W> {
    fn add_i8(&mut self, v: i8) -> Result<(), Error> {
        self.write_seperator()?;
        write!(self.writer, "{}", v)?;
        Ok(())
    }
    fn add_i16(&mut self, v: i16) -> Result<(), Error> {
        self.write_seperator()?;
        write!(self.writer, "{}", v)?;
        Ok(())
    }
    fn add_i32(&mut self, v: i32) -> Result<(), Error> {
        self.write_seperator()?;
        write!(self.writer, "{}", v)?;
        Ok(())
    }
    fn add_u8(&mut self, v: u8) -> Result<(), Error> {
        self.write_seperator()?;
        write!(self.writer, "{}", v)?;
        Ok(())
    }
    fn add_u16(&mut self, v: u16) -> Result<(), Error> {
        self.write_seperator()?;
        write!(self.writer, "{}", v)?;
        Ok(())
    }
    fn add_u32(&mut self, v: u32) -> Result<(), Error> {
        self.write_seperator()?;
        write!(self.writer, "{}", v)?;
        Ok(())
    }
    fn add_f32(&mut self, v: f32) -> Result<(), Error> {
        self.write_seperator()?;
        write!(self.writer, "{}", v)?;
        Ok(())
    }
    fn add_f64(&mut self, v: f64) -> Result<(), Error> {
        self.write_seperator()?;
        write!(self.writer, "{}", v)?;
        Ok(())
    }

    fn end_element(&mut self) -> Result<(), Error> {
        self.writer.write_all(b"\n")?;
        self.at_start_of_line = true;
        Ok(())
    }
}

macro_rules! gen_binary_block {
    ($name:ident, $endianess:ident) => {
        /// A PLY block which doesn't insert separators and serializes everything binary.
        #[derive(Debug)]
        struct $name<'a, W: Write> {
            writer: &'a mut W,
        }

        impl<'a, W: Write> $name<'a, W> {
            fn new(w: &'a mut W) -> Self {
                Self {
                    writer: w,
                }
            }
        }

        impl<W: io::Write> Serializer for $name<'_, W> {
            fn add_i8(&mut self, v: i8) -> Result<(), Error> {
                self.writer.write_i8(v).map_err(|e| e.into())
            }
            fn add_i16(&mut self, v: i16) -> Result<(), Error> {
                self.writer.write_i16::<$endianess>(v).map_err(|e| e.into())
            }
            fn add_i32(&mut self, v: i32) -> Result<(), Error> {
                self.writer.write_i32::<$endianess>(v).map_err(|e| e.into())
            }
            fn add_u8(&mut self, v: u8) -> Result<(), Error> {
                self.writer.write_u8(v).map_err(|e| e.into())
            }
            fn add_u16(&mut self, v: u16) -> Result<(), Error> {
                self.writer.write_u16::<$endianess>(v).map_err(|e| e.into())
            }
            fn add_u32(&mut self, v: u32) -> Result<(), Error> {
                self.writer.write_u32::<$endianess>(v).map_err(|e| e.into())
            }
            fn add_f32(&mut self, v: f32) -> Result<(), Error> {
                self.writer.write_f32::<$endianess>(v).map_err(|e| e.into())
            }
            fn add_f64(&mut self, v: f64) -> Result<(), Error> {
                self.writer.write_f64::<$endianess>(v).map_err(|e| e.into())
            }

            fn end_element(&mut self) -> Result<(), Error> {
                // NOOP
                Ok(())
            }
        }
    }
}

gen_binary_block!(BinaryBeSerializer, BigEndian);
gen_binary_block!(BinaryLeSerializer, LittleEndian);
