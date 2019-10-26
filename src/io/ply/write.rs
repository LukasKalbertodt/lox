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
//! - For ASCII encoding we simply use the `fmt::Display` impl of all types.
//!   It's not clear if that's OK, but I haven't encountered any issues
//!   regarding this with other programs so far.
//!


use std::{
    convert::TryFrom,
    mem,
    io::{self, Write},
    slice,
};

use byteorder::{BigEndian, LittleEndian};

use crate::{
    prelude::*,
    handle::hsize,
    io::{
        Error, ErrorKind, PrimitiveType, PropKind, ColorType,
        util::HandleIndexMap,
    },
};
use super::{
    Encoding,
    raw::{
        ElementDef, RawSource, Serializer, PropVec, PropertyDef, PropertyType,
        ScalarType, ListLenType, PlyScalar,
    },
};



// ===============================================================================================
// ===== PLY Config
// ===============================================================================================

/// Used to configure and create a [`Writer`].
///
/// This is used to configure basic settings for the file to be written. Most
/// importantly, this is the file encoding. Additionally, you can add comments
/// to the file header. With [`Config::into_writer`] you can create a
/// [`Writer`] that can be used as streaming sink.
#[derive(Clone, Debug)]
pub struct Config {
    encoding: Encoding,
    comments: Vec<String>,
}

impl Config {
    /// Creates a new configuration with binary encoding (native endianness).
    pub fn binary() -> Self {
        Self::new(Encoding::binary_native())
    }

    /// Creates a new configuration with ASCII encoding. *Try avoid using ASCII
    /// encoding!*
    ///
    /// ASCII encoding is usually a lot less space efficient and a lot slower
    /// to read and write. Therefore you should instead using a binary
    /// encoding. The PLY file header is always ASCII.
    pub fn ascii() -> Self {
        Self::new(Encoding::Ascii)
    }

    /// Creates a new configuration with the given encoding.
    pub fn new(encoding: Encoding) -> Self {
        Self {
            encoding,
            comments: vec![],
        }
    }

    /// Adds a `comment` line to the file header.
    ///
    /// The given string must not contain `'\n'` or else this method panics.
    /// Note that there are probably other characters that could invalidate the
    /// file header or at least confuse some parsers. You should thus take care
    /// what strings you pass in.
    pub fn add_comment(mut self, comment: impl Into<String>) -> Self {
        let comment = comment.into();

        assert!(!comment.contains('\n'), "PLY comments must not contain '\\n'!");

        self.comments.push(comment);
        self
    }

    /// Creates a writer with the given `io::Write` instance and `self` as
    /// configuration.
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
///
/// There are two ways to write data: (a) using the high level API
/// [`StreamSink`] (you probably want to do that), or (b) using the low level
/// API via [`write_raw`][Writer::write_raw] (you only need to do that in very
/// special situations).
///
///
/// # Example
///
/// ```
/// use std::{
///     fs::File,
///     io::BufWriter,
/// };
/// use lox::io::{
///     Error, StreamSink, MemSource,
///     ply::{Config, Writer},
/// };
///
/// fn write_both_encodings(src: &impl MemSource) -> Result<(), Error> {
///     // We use `BufWriter` here because unbuffered file access is usually a
///     // lot slower.
///     let file_a = File::create("mesh_ascii.ply")?;
///     Config::ascii().into_writer(BufWriter::new(file_a)).transfer_from(src)?;
///
///     let file_b = File::create("mesh_binary.ply")?;
///     Config::binary().into_writer(BufWriter::new(file_b)).transfer_from(src)?;
///
///     Ok(())
/// }
/// ```
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
    /// You usually don't need to use this function directly and instead use a
    /// high level interface. This function is still exposed to give you almost
    /// full control. That way, users of this library never have to rewrite the
    /// abstraction over encodings and the actual writing themselves but can
    /// instead use this function. The `StreamSink` interface is built on top
    /// of this function.
    ///
    /// **Note**: this function is *not* easy to use. It is purposefully very
    /// low level. This should be your last resort.
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

        // Color: red green blue [alpha]
        if let Some(desc) = src.vertex_color_type() {
            // In theory, you can store float colors in PLY, but in practice
            // everyone expects `uchar` as color channel type.
            let ty = PropertyType::Scalar(ScalarType::UChar);
            vertex_def.property_defs.push(PropertyDef { ty, name: "red".into() });
            vertex_def.property_defs.push(PropertyDef { ty, name: "green".into() });
            vertex_def.property_defs.push(PropertyDef { ty, name: "blue".into() });

            if desc.alpha {
                vertex_def.property_defs.push(PropertyDef { ty, name: "alpha".into() });
            }
        }


        // ----- Face element -----------------
        let mut face_def = ElementDef {
            name: "face".into(),
            count: mesh.num_faces().into(),
            property_defs: PropVec::new(),
        };

        // Connectivity: vertex_indices. To save memory, we figure out the
        // smallest list len type we can use. It would be handy to have an
        // upper bound of vertices per face from the mesh, but if we haven't,
        // we can still take the number of vertices as upper bound.
        let vi_list_len_type =
            if mesh.is_tri_mesh() || mesh.num_vertices() < u8::max_value() as hsize {
                ListLenType::UChar
            } else if mesh.num_vertices() < u16::max_value() as hsize {
                ListLenType::UShort
            } else {
                ListLenType::UInt
            };

        face_def.property_defs.push(PropertyDef {
            ty: PropertyType::List {
                len_type: vi_list_len_type,
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

        // Color: red green blue [alpha]
        if let Some(desc) = src.face_color_type() {
            // In theory, you can store float colors in PLY, but in practice
            // everyone expects `uchar` as color channel type.
            let ty = PropertyType::Scalar(ScalarType::UChar);
            face_def.property_defs.push(PropertyDef { ty, name: "red".into() });
            face_def.property_defs.push(PropertyDef { ty, name: "green".into() });
            face_def.property_defs.push(PropertyDef { ty, name: "blue".into() });

            if desc.alpha {
                face_def.property_defs.push(PropertyDef { ty, name: "alpha".into() });
            }
        }


        // ====================================================================
        // ===== Body writing implementation
        // ====================================================================

        struct HelperSource<'a, SrcT: MemSource> {
            src: &'a SrcT,
            vi_list_len_type: ListLenType,
        }

        impl<SrcT: MemSource> RawSource for HelperSource<'_, SrcT> {
            fn serialize_into<S: Serializer>(self, mut ser: S) -> Result<(), Error> {
                let src = self.src;
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

                macro_rules! make_color_fn_ptr {
                    ($desc:expr, $fn_rgb:ident, $noop:ident $(,)?) => {
                        match $desc {
                            None => $noop::<S, SrcT>,
                            Some(ColorType { alpha: false, .. }) => {
                                $fn_rgb::<S, SrcT, [u8; 3]>
                            }
                            Some(ColorType { alpha: true, .. }) => {
                                $fn_rgb::<S, SrcT, [u8; 4]>
                            }
                        }
                    }
                }

                // ----- Vertex positions ------------------------------------
                fn write_vertex_position<S: Serializer, SrcT: MemSource, P: PlyScalar>(
                    ser: &mut S,
                    src: &SrcT,
                    handle: VertexHandle,
                ) -> Result<(), Error> {
                    let pos = match src.vertex_position::<P>(handle)? {
                        Some(pos) => pos,
                        None => return Err(Error::new(|| ErrorKind::DataIncomplete {
                            prop: PropKind::VertexPosition,
                            msg: format!("no position for {:?} while writing PLY", handle),
                        })),
                    };

                    let mut array: [P; 3] = pos.convert();
                    ser.add_slice(&mut array)?;

                    Ok(())
                }

                let write_v_position = make_fn_ptr!(
                    src.vertex_position_type().map(closest_ply_type),
                    write_vertex_position,
                    vertex_noop,
                );

                // ----- Vertex normals --------------------------------------
                fn write_vertex_normal<S: Serializer, SrcT: MemSource, P: PlyScalar>(
                    ser: &mut S,
                    src: &SrcT,
                    handle: VertexHandle,
                ) -> Result<(), Error> {
                    let normal = match src.vertex_normal::<P>(handle)? {
                        Some(normal) => normal,
                        None => return Err(Error::new(|| ErrorKind::DataIncomplete {
                            prop: PropKind::VertexNormal,
                            msg: format!("no normal for {:?} while writing PLY", handle),
                        })),
                    };

                    let mut array: [P; 3] = normal.convert();
                    ser.add_slice(&mut array)?;

                    Ok(())
                }

                let write_v_normal = make_fn_ptr!(
                    src.vertex_normal_type().map(closest_ply_type),
                    write_vertex_normal,
                    vertex_noop,
                );

                // ----- Vertex colors --------------------------------------
                fn write_vertex_color<S: Serializer, SrcT: MemSource, C: ColorLike>(
                    ser: &mut S,
                    src: &SrcT,
                    handle: VertexHandle,
                ) -> Result<(), Error>
                where
                    C::Channel: PlyScalar,
                {
                    let color = match src.vertex_color::<C>(handle)? {
                        Some(color) => color,
                        None => return Err(Error::new(|| ErrorKind::DataIncomplete {
                            prop: PropKind::VertexColor,
                            msg: format!("no color for {:?} while writing PLY", handle),
                        })),
                    };

                    // The branch is optimized away here
                    if C::HAS_ALPHA {
                        let mut array: [C::Channel; 4] = color.convert();
                        ser.add_slice(&mut array)?;
                    } else {
                        let mut array: [C::Channel; 3] = color.convert();
                        ser.add_slice(&mut array)?;
                    }

                    Ok(())
                }

                let write_v_color = make_color_fn_ptr!(
                    src.vertex_color_type(),
                    write_vertex_color,
                    vertex_noop,
                );

                // ----- Face normals ----------------------------------------
                fn write_face_normal<S: Serializer, SrcT: MemSource, P: PlyScalar>(
                    ser: &mut S,
                    src: &SrcT,
                    handle: FaceHandle,
                ) -> Result<(), Error> {
                    let normal = match src.face_normal::<P>(handle)? {
                        Some(normal) => normal,
                        None => return Err(Error::new(|| ErrorKind::DataIncomplete {
                            prop: PropKind::FaceNormal,
                            msg: format!("no normal for {:?} while writing PLY", handle),
                        })),
                    };

                    let mut array: [P; 3] = normal.convert();
                    ser.add_slice(&mut array)?;

                    Ok(())
                }

                let write_f_normal = make_fn_ptr!(
                    src.face_normal_type().map(closest_ply_type),
                    write_face_normal,
                    face_noop,
                );

                // ----- Face colors --------------------------------------
                fn write_face_color<S: Serializer, SrcT: MemSource, C: ColorLike>(
                    ser: &mut S,
                    src: &SrcT,
                    handle: FaceHandle,
                ) -> Result<(), Error>
                where
                    C::Channel: PlyScalar,
                {
                    let color = match src.face_color::<C>(handle)? {
                        Some(color) => color,
                        None => return Err(Error::new(|| ErrorKind::DataIncomplete {
                            prop: PropKind::FaceColor,
                            msg: format!("no color for {:?} while writing PLY", handle),
                        })),
                    };

                    // The branch is optimized away here
                    if C::HAS_ALPHA {
                        let mut array: [C::Channel; 4] = color.convert();
                        ser.add_slice(&mut array)?;
                    } else {
                        let mut array: [C::Channel; 3] = color.convert();
                        ser.add_slice(&mut array)?;
                    }

                    Ok(())
                }

                let write_f_color = make_color_fn_ptr!(
                    src.face_color_type(),
                    write_face_color,
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
                    write_v_color(&mut ser, src, vh)?;

                    ser.end_element()?;
                }

                let mut vertex_indices = Vec::new();

                for fh in mesh.face_handles() {
                    // Vertex indices: get handles from mesh and get the
                    // corresponding index.
                    for vh in mesh.vertices_around_face(fh) {
                        // A panic here means that the mesh returned a handle
                        // from `vertices_around_face` that was not returned by
                        // `vertex_handles()`. That's not allowed.
                        let index = indices_map.get(vh)
                            .expect("corrupt mesh: face adjacent to non-existing vertex");

                        // This is just relevant if the feature `large-handle`
                        // is activated: if the handle is larger than 2^32, we
                        // cannot store it in PLY, so we return an error.
                        let ply_index = u32::try_from(index).map_err(|_| {
                            Error::new(|| ErrorKind::SinkIncompatible(
                                "PLY does not support indices larger than 2^32, but the mesh \
                                    returned such a large handle (either this is a huge mesh or \
                                    the mesh type is doing something strange)".into()
                            ))
                        })?;

                        vertex_indices.push(ply_index);
                    }


                    // Actually store vertex indices
                    match self.vi_list_len_type {
                        // All the casts are fine, as we've chosen
                        // `vi_list_len_type` appropriately above.
                        ListLenType::UChar => ser.add::<u8>(vertex_indices.len() as u8)?,
                        ListLenType::UShort => ser.add::<u16>(vertex_indices.len() as u16)?,
                        ListLenType::UInt => ser.add::<u32>(vertex_indices.len() as u32)?,
                    }
                    ser.add_slice(&mut vertex_indices)?;
                    vertex_indices.clear();

                    // Other face properties
                    write_f_normal(&mut ser, src, fh)?;
                    write_f_color(&mut ser, src, fh)?;

                    ser.end_element()?;
                }

                Ok(())
            }
        }


        // Actually start the write operation.
        self.write_raw(&[vertex_def, face_def], HelperSource { src, vi_list_len_type })
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
// These serializers are just used to abstract over the encoding (and things
// like separators and line endings).

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

    fn write_separator(&mut self) -> Result<(), Error> {
        if self.at_start_of_line {
            self.at_start_of_line = false;
        } else {
            self.writer.write_all(b" ")?;
        }

        Ok(())
    }
}

impl<W: io::Write> Serializer for AsciiSerializer<'_, W> {
    fn add<P: PlyScalar>(&mut self, v: P) -> Result<(), Error> {
        self.write_separator()?;
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
    ($name:ident, $endianness:ident) => {
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
            fn add<P: PlyScalar>(&mut self, v: P) -> Result<(), Error> {
                let mut s = [v];
                self.add_slice(&mut s)
            }

            fn add_slice<P: PlyScalar>(&mut self, s: &mut [P]) -> Result<(), Error> {
                P::to_endianness::<$endianness>(s);

                // TODO: revisit this unsafe part when unsafe code guidelines
                // have been published. I'm fairly sure all of this is safe. At
                // least the function `as_byte_slice` in isolation looks safe,
                // since all types that implement `PlyScalar` can be transmuted
                // safely. However, it still worries me that at the bottom
                // `bytes` and `s` both refer to the same data, once mutably,
                // once immutably. Hopefully, the fact that `s` is marked as
                // borrowed, is sufficient to make this OK.

                #[inline(always)]
                fn as_byte_slice<P: PlyScalar>(input: &[P]) -> &[u8] {
                    unsafe {
                        let len = input.len() * mem::size_of::<P>();
                        let ptr = input.as_ptr() as *const u8;
                        slice::from_raw_parts(ptr, len)
                    }
                }

                let bytes = as_byte_slice(&*s);
                self.writer.write_all(bytes).map_err(|e| e.into())
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
