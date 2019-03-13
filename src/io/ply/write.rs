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

#![allow(dead_code)] // TODO

use std::{
    collections::HashSet,
    io::{self, Write},
};

use byteorder::{BigEndian, LittleEndian, WriteBytesExt};
use cgmath::{Point3, Vector3};

use crate::{
    handle::{FaceHandle, Handle, VertexHandle},
    map::{FnMap, PropMap, FacePropMap, VertexPropMap},
    prop::{Pos3Like, Vec3Like},
    // io::{IntoMeshWriter, MeshWriter, StreamSink, MemSource, PrimitiveType},
    io::{Error, StreamSink, MemSource, Primitive, PrimitiveType, PropKind},
    traits::*,
    util::TriArrayExt,
};
use super::{
    Encoding, Serialize, SingleSerialize, PropSerializer, PropType,
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

// impl<'a, MeshT, PosM> IntoMeshWriter<'a, MeshT, PosM> for Config
// where
//     MeshT: 'a + TriMesh + TriVerticesOfFace,
//     PosM: 'a + VertexPropMap,
//     PosM::Target: Pos3Like,
//     <PosM::Target as Pos3Like>::Scalar: SingleSerialize,
// {
//     type Writer = Writer<'a, MeshT, ListPosElem<'a, PosM, EmptyList>, EmptyList>;
//     fn into_writer(self, mesh: &'a MeshT, vertex_positions: &'a PosM) -> Self::Writer {
//         Writer {
//             config: self,
//             mesh,
//             vertex_props: ListPosElem {
//                 map: vertex_positions,
//                 tail: EmptyList,
//             },
//             vertex_prop_names: ["x", "y", "z"].iter().map(|s| s.to_string()).collect(),
//             face_props: EmptyList,
//             face_prop_names: HashSet::new(),
//         }
//     }
// }


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

        if let Some(ty) = src.vertex_position_type() {
            let ty = PropertyType::Scalar(closest_ply_type(ty));
            vertex_def.property_defs.push(PropertyDef { ty, name: "x".into() });
            vertex_def.property_defs.push(PropertyDef { ty, name: "y".into() });
            vertex_def.property_defs.push(PropertyDef { ty, name: "z".into() });
        }

        // ----- Face element -----------------
        let mut face_def = ElementDef {
            name: "face".into(),
            count: mesh.num_faces().into(),
            property_defs: PropVec::new(),
        };
        face_def.property_defs.push(PropertyDef {
            ty: PropertyType::List {
                len_type: ListLenType::UChar,
                scalar_type: ScalarType::UInt,
            },
            name: "vertex_indices".into(),
        });


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
                type FnPtr<S> = fn(&mut S) -> Result<(), Error>;
                fn vertex_noop<S, SrcT>(
                    _: &mut S,
                    _: &SrcT,
                    _: VertexHandle,
                ) -> Result<(), Error> {
                    Ok(())
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

                let write_v_position = match src.vertex_position_type().map(closest_ply_type) {
                    None => vertex_noop::<S, SrcT>,
                    Some(ScalarType::Char) => write_vertex_position::<S, SrcT, i8>,
                    Some(ScalarType::Short) => write_vertex_position::<S, SrcT, i16>,
                    Some(ScalarType::Int) => write_vertex_position::<S, SrcT, i32>,
                    Some(ScalarType::UChar) => write_vertex_position::<S, SrcT, u8>,
                    Some(ScalarType::UShort) => write_vertex_position::<S, SrcT, u16>,
                    Some(ScalarType::UInt) => write_vertex_position::<S, SrcT, u32>,
                    Some(ScalarType::Float) => write_vertex_position::<S, SrcT, f32>,
                    Some(ScalarType::Double) => write_vertex_position::<S, SrcT, f64>,
                };


                // ===========================================================
                // ===== Write all the data
                // ===========================================================
                for vh in mesh.vertex_handles() {
                    write_v_position(&mut ser, src, vh)?;

                    ser.end_element()?;
                }

                for fh in mesh.face_handles() {
                    // Vertex indices
                    // TODO: use indirect map in case mesh handles are shitty
                    let [a, b, c] = mesh.vertices_of_face(fh);
                    ser.add_u8(3)?;
                    ser.add_u32(a.idx())?;
                    ser.add_u32(b.idx())?;
                    ser.add_u32(c.idx())?;

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
// ===== PLY Writer
// ===============================================================================================

// #[derive(Debug)]
// pub struct Writer<'a, MeshT, VertexPropsT, FacePropsT>
// where
//     MeshT: TriMesh + TriVerticesOfFace,
//     VertexPropsT: PropList<VertexHandle>,
//     FacePropsT: PropList<FaceHandle>,
// {
//     config: Config,
//     mesh: &'a MeshT,
//     vertex_props: VertexPropsT,
//     vertex_prop_names: HashSet<String>,
//     face_props: FacePropsT,
//     face_prop_names: HashSet<String>,
// }

// impl<'a, MeshT, VertexPropsT, FacePropsT> Writer<'a, MeshT, VertexPropsT, FacePropsT>
// where
//     MeshT: TriMesh + TriVerticesOfFace,
//     VertexPropsT: 'a + PropList<VertexHandle>,
//     FacePropsT: 'a + PropList<FaceHandle>,
// {
//     /// Adds the given vertex property to the PLY file. The given string is used
//     /// as property name.
//     pub fn add_vertex_prop<MapT>(mut self, name: impl Into<String>, map: &'a MapT)
//         -> Writer<'a, MeshT, impl 'a + PropList<VertexHandle>, FacePropsT>
//     where
//         MapT: 'a + VertexPropMap,
//         MapT::Target: Serialize,
//     {
//         let name = name.into();
//         self.add_vertex_prop_name(name.clone());

//         Writer {
//             config: self.config,
//             mesh: self.mesh,
//             vertex_props: ListSingleElem {
//                 name,
//                 map,
//                 tail: self.vertex_props,
//             },
//             vertex_prop_names: self.vertex_prop_names,
//             face_props: self.face_props,
//             face_prop_names: self.face_prop_names,
//         }
//     }

//     /// Adds the given face property to the PLY file. The given string is used
//     /// as property name.
//     pub fn add_face_prop<MapT>(mut self, name: impl Into<String>, map: &'a MapT)
//         -> Writer<'a, MeshT, VertexPropsT, impl 'a + PropList<FaceHandle>>
//     where
//         MapT: 'a + FacePropMap,
//         MapT::Target: Serialize,
//     {
//         let name = name.into();
//         self.add_face_prop_name(name.clone());

//         Writer {
//             config: self.config,
//             mesh: self.mesh,
//             vertex_props: self.vertex_props,
//             vertex_prop_names: self.vertex_prop_names,
//             face_props: ListSingleElem {
//                 name: name.into(),
//                 map,
//                 tail: self.face_props,
//             },
//             face_prop_names: self.face_prop_names,
//         }
//     }

//     /// Adds the given map as vertex normals. The normal will be serialized
//     /// with the three property names `nx`, `ny` and `nz`.
//     pub fn with_vertex_normals<MapT>(mut self, map: &'a MapT)
//         -> Writer<'a, MeshT, impl 'a + PropList<VertexHandle>, FacePropsT>
//     where
//         MapT: 'a + VertexPropMap,
//         MapT::Target: Vec3Like,
//         <MapT::Target as Vec3Like>::Scalar: SingleSerialize,
//     {
//         self.add_vertex_prop_name("nx".into());
//         self.add_vertex_prop_name("ny".into());
//         self.add_vertex_prop_name("nz".into());

//         Writer {
//             config: self.config,
//             mesh: self.mesh,
//             vertex_props: ListVertexNormalElem {
//                 map,
//                 tail: self.vertex_props,
//             },
//             vertex_prop_names: self.vertex_prop_names,
//             face_props: self.face_props,
//             face_prop_names: self.face_prop_names,
//         }
//     }

//     fn add_vertex_prop_name(&mut self, name: String) {
//         let is_new = self.vertex_prop_names.insert(name.clone());
//         if !is_new {
//             panic!(
//                 "attempt to add a vertex property to PLY file with name '{}', \
//                     but that name is already used",
//                 name,
//             );
//         }
//     }

//     fn add_face_prop_name(&mut self, name: String) {
//         let is_new = self.face_prop_names.insert(name.clone());
//         if !is_new {
//             panic!(
//                 "attempt to add a face property to PLY file with name '{}', \
//                     but that name is already used",
//                     name,
//             );
//         }
//     }

//     // TODO: color (just add another list element type like `PosElem`)
// }

// impl<MeshT, VertexPropsT, FacePropsT> MeshWriter for Writer<'_, MeshT, VertexPropsT, FacePropsT>
// where // TODO: remove once implied bounds land
//     MeshT: TriMesh + TriVerticesOfFace,
//     VertexPropsT: PropList<VertexHandle>,
//     FacePropsT: PropList<FaceHandle>,
// {
//     type Error = Error;

//     fn write_to(&self, w: impl Write) -> Result<(), Self::Error> {
//         write(
//             w,
//             &self.config,
//             self.mesh.num_vertices(),
//             self.mesh.num_faces(),
//             self.mesh.vertices().map(|v| v.handle()),
//             self.mesh.faces().map(|f| f.handle()),
//             |fh| self.mesh.vertices_of_face(fh),
//             &self.vertex_props,
//             &self.face_props,
//         )
//     }
// }


// ===============================================================================================
// ===== Helper stuff for the heterogeneous list stored inside the MeshWriter
// ===============================================================================================

/// A heterogenous list of property maps with a PLY field name. For internal
/// use, you don't have to worry about this!
///
/// The lists are basically stored backwards, because adding something to the
/// back of the list is kind of a hassle (not algorithmically, but realizing
/// this in Rust's type system). So all operations recurse first and then do
/// their work. This makes tail recursion impossible, but tail recursion isn't
/// a thing in Rust anyway.
pub trait PropList<H: Handle> {
    fn write_header(&self, w: &mut impl Write) -> Result<(), Error>;
    fn write_block(&self, handle: H, block: &mut impl Block) -> Result<(), Error>;
}


// ----- EmptyList --------------------------------------------------------
#[derive(Debug)]
pub struct EmptyList;

impl<H: Handle> PropList<H> for EmptyList {
    fn write_header(&self, _: &mut impl Write) -> Result<(), Error> {
        Ok(())
    }
    fn write_block(&self, _: H, _: &mut impl Block) -> Result<(), Error> {
        Ok(())
    }
}


// ----- ListPosElem --------------------------------------------------------
#[derive(Debug)]
pub struct ListPosElem<'m, MapT, TailT> {
    map: &'m MapT,
    tail: TailT,
}

impl<MapT, TailT> PropList<VertexHandle> for ListPosElem<'_, MapT, TailT>
where
    TailT: PropList<VertexHandle>,
    MapT: VertexPropMap,
    MapT::Target: Pos3Like,
    <MapT::Target as Pos3Like>::Scalar: SingleSerialize,
{
    fn write_header(&self, w: &mut impl Write) -> Result<(), Error> {
        self.tail.write_header(w)?;

        let ty = <<MapT::Target as Pos3Like>::Scalar as SingleSerialize>::SINGLE_TYPE;
        let ty = ty.ply_type_name();
        writeln!(w, "property {} x", ty)?;
        writeln!(w, "property {} y", ty)?;
        writeln!(w, "property {} z", ty)?;

        Ok(())
    }

    fn write_block(&self, handle: VertexHandle, block: &mut impl Block) -> Result<(), Error> {
        self.tail.write_block(handle, {block})?;

        let pos = self.map.get(handle).unwrap_or_else(|| {
            panic!("vertex position PropMap incomplete: no value for handle {:?}", handle);
        });

        block.add(&pos.x())?;
        block.add(&pos.y())?;
        block.add(&pos.z())?;

        Ok(())
    }
}

// // ----- ListPosElem --------------------------------------------------------
// #[derive(Debug)]
// pub struct ListVertexNormalElem<'m, MapT, TailT> {
//     map: &'m MapT,
//     tail: TailT,
// }

// impl<MapT, TailT> PropList<VertexHandle> for ListVertexNormalElem<'_, MapT, TailT>
// where
//     TailT: PropList<VertexHandle>,
//     MapT: VertexPropMap,
//     MapT::Target: Vec3Like,
//     <MapT::Target as Vec3Like>::Scalar: SingleSerialize,
// {
//     fn write_header(&self, w: &mut impl Write) -> Result<(), Error> {
//         self.tail.write_header(w)?;

//         let ty = <<MapT::Target as Vec3Like>::Scalar as SingleSerialize>::SINGLE_TYPE;
//         let ty = ty.ply_type_name();
//         writeln!(w, "property {} nx", ty)?;
//         writeln!(w, "property {} ny", ty)?;
//         writeln!(w, "property {} nz", ty)?;

//         Ok(())
//     }

//     fn write_block(&self, handle: VertexHandle, block: &mut impl Block) -> Result<(), Error> {
//         self.tail.write_block(handle, {block})?;

//         let pos = self.map.get(handle).unwrap_or_else(|| {
//             panic!("face normal PropMap incomplete: no value for handle {:?}", handle);
//         });

//         block.add(&pos.x())?;
//         block.add(&pos.y())?;
//         block.add(&pos.z())?;

//         Ok(())
//     }
// }


// // ----- ListSingleElem ------------------------------------------------------
// #[derive(Debug)]
// pub struct ListSingleElem<'a , TailT, MapT: 'a> {
//     name: String,
//     map: &'a MapT,
//     tail: TailT,
// }

// impl<H: Handle, TailT: PropList<H>, MapT> PropList<H> for ListSingleElem<'_, TailT, MapT>
// where
//     MapT: PropMap<H>,
//     MapT::Target: Serialize,
// {
//     fn write_header(&self, w: &mut impl Write) -> Result<(), Error> {
//         self.tail.write_header(w)?;

//         match <MapT::Target as Serialize>::TYPE {
//             PropType::Single(ty) => {
//                 writeln!(w, "property {} {}", ty.ply_type_name(), self.name)?;
//             }
//             PropType::DynLenSeq(ty) => {
//                 writeln!(w, "property list uint {} {}", ty.ply_type_name(), self.name)?;
//             }
//             PropType::FixedLenSeq { len, ty } => {
//                 for i in 0..len {
//                     writeln!(w, "property {} {}[{}]", ty.ply_type_name(), self.name, i)?;
//                 }
//             }
//         }

//         Ok(())
//     }

//     fn write_block(&self, handle: H, block: &mut impl Block) -> Result<(), Error> {
//         self.tail.write_block(handle, {block})?;

//         let prop = self.map.get(handle).unwrap_or_else(|| {
//             panic!("PropMap for '{}' incomplete: no value for handle {:?}", self.name, handle);
//         });
//         block.add(&*prop)?;

//         Ok(())
//     }
// }

// ===============================================================================================
// ===== Definition of ASCII and binary serializers
// ===============================================================================================

/// A block holds all properties for one specific element. In the ASCII format
/// this equivalent to "one line". This trait generalizes over property
/// seperators and block terminators (' ' and '\n' for ASCII, nothing for
/// binary formats).
pub trait Block {
    /// Adds the given property to the block. This function is in charge of
    /// inserting seperators when necessary.
    fn add(&mut self, prop: &impl Serialize) -> Result<(), Error>;

    /// Finishes the block. Writes '\n' for ASCII format, does nothing for
    /// binary formats.
    fn finish(self) -> Result<(), Error>;
}


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
