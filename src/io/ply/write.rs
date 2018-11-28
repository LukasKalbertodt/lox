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
    collections::HashSet,
    io::Write,
};

use byteorder::{BigEndian, LittleEndian, WriteBytesExt};

use crate::{
    Mesh, MeshUnsorted, ExplicitFace, ExplicitVertex,
    handle::{FaceHandle, Handle, VertexHandle},
    map::{PropMap, FacePropMap, VertexPropMap},
    math::{Pos3Like, Vec3Like},
    io::{IntoMeshWriter, MeshWriter},
};
use super::{Error, Encoding, Serialize, SingleSerialize, PropSerializer, PropType};



// ===============================================================================================
// ===== PLY Serializer
// ===============================================================================================

#[derive(Clone, Debug)]
pub struct Serializer {
    encoding: Encoding,
    comments: Vec<String>,
}

impl Serializer {
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
}

impl<'a, MeshT, PosM> IntoMeshWriter<'a, MeshT, PosM> for Serializer
where
    MeshT: 'a + Mesh + MeshUnsorted + ExplicitFace + ExplicitVertex,
    PosM: 'a + VertexPropMap,
    PosM::Target: Pos3Like,
    <PosM::Target as Pos3Like>::Scalar: SingleSerialize,
{
    type Writer = Writer<'a, MeshT, ListPosElem<'a, PosM, EmptyList>, EmptyList>;
    fn into_writer(self, mesh: &'a MeshT, vertex_positions: &'a PosM) -> Self::Writer {
        Writer {
            ser: self,
            mesh,
            vertex_props: ListPosElem {
                map: vertex_positions,
                tail: EmptyList,
            },
            vertex_prop_names: ["x", "y", "z"].iter().map(|s| s.to_string()).collect(),
            face_props: EmptyList,
            face_prop_names: HashSet::new(),
        }
    }
}


// ===============================================================================================
// ===== PLY Writer
// ===============================================================================================

#[derive(Debug)]
pub struct Writer<'a, MeshT, VertexPropsT, FacePropsT>
where
    MeshT: Mesh + MeshUnsorted + ExplicitFace + ExplicitVertex,
    VertexPropsT: PropList<VertexHandle>,
    FacePropsT: PropList<FaceHandle>,
{
    ser: Serializer,
    mesh: &'a MeshT,
    vertex_props: VertexPropsT,
    vertex_prop_names: HashSet<String>,
    face_props: FacePropsT,
    face_prop_names: HashSet<String>,
}

impl<'a, MeshT, VertexPropsT, FacePropsT> Writer<'a, MeshT, VertexPropsT, FacePropsT>
where
    MeshT: Mesh + MeshUnsorted + ExplicitFace + ExplicitVertex,
    VertexPropsT: 'a + PropList<VertexHandle>,
    FacePropsT: 'a + PropList<FaceHandle>,
{
    /// Adds the given vertex property to the PLY file. The given string is used
    /// as property name.
    pub fn add_vertex_prop<MapT>(mut self, name: impl Into<String>, map: &'a MapT)
        -> Writer<'a, MeshT, impl 'a + PropList<VertexHandle>, FacePropsT>
    where
        MapT: 'a + VertexPropMap,
        MapT::Target: Serialize,
    {
        let name = name.into();
        self.add_vertex_prop_name(name.clone());

        Writer {
            ser: self.ser,
            mesh: self.mesh,
            vertex_props: ListSingleElem {
                name,
                map,
                tail: self.vertex_props,
            },
            vertex_prop_names: self.vertex_prop_names,
            face_props: self.face_props,
            face_prop_names: self.face_prop_names,
        }
    }

    /// Adds the given face property to the PLY file. The given string is used
    /// as property name.
    pub fn add_face_prop<MapT>(mut self, name: impl Into<String>, map: &'a MapT)
        -> Writer<'a, MeshT, VertexPropsT, impl 'a + PropList<FaceHandle>>
    where
        MapT: 'a + FacePropMap,
        MapT::Target: Serialize,
    {
        let name = name.into();
        self.add_face_prop_name(name.clone());

        Writer {
            ser: self.ser,
            mesh: self.mesh,
            vertex_props: self.vertex_props,
            vertex_prop_names: self.vertex_prop_names,
            face_props: ListSingleElem {
                name: name.into(),
                map,
                tail: self.face_props,
            },
            face_prop_names: self.face_prop_names,
        }
    }

    /// Adds the given map as vertex normals. The normal will be serialized
    /// with the three property names `nx`, `ny` and `nz`.
    pub fn with_vertex_normals<MapT>(mut self, map: &'a MapT)
        -> Writer<'a, MeshT, impl 'a + PropList<VertexHandle>, FacePropsT>
    where
        MapT: 'a + VertexPropMap,
        MapT::Target: Vec3Like,
        <MapT::Target as Vec3Like>::Scalar: SingleSerialize,
    {
        self.add_vertex_prop_name("nx".into());
        self.add_vertex_prop_name("ny".into());
        self.add_vertex_prop_name("nz".into());

        Writer {
            ser: self.ser,
            mesh: self.mesh,
            vertex_props: ListVertexNormalElem {
                map,
                tail: self.vertex_props,
            },
            vertex_prop_names: self.vertex_prop_names,
            face_props: self.face_props,
            face_prop_names: self.face_prop_names,
        }
    }

    fn add_vertex_prop_name(&mut self, name: String) {
        let is_new = self.vertex_prop_names.insert(name.clone());
        if !is_new {
            panic!(
                "attempt to add a vertex property to PLY file with name '{}', \
                    but that name is already used",
                name,
            );
        }
    }

    fn add_face_prop_name(&mut self, name: String) {
        let is_new = self.face_prop_names.insert(name.clone());
        if !is_new {
            panic!(
                "attempt to add a face property to PLY file with name '{}', \
                    but that name is already used",
                    name,
            );
        }
    }

    // TODO: color (just add another list element type like `PosElem`)
}

impl<MeshT, VertexPropsT, FacePropsT> MeshWriter for Writer<'_, MeshT, VertexPropsT, FacePropsT>
where // TODO: remove once implied bounds land
    MeshT: Mesh + MeshUnsorted + ExplicitFace + ExplicitVertex,
    VertexPropsT: PropList<VertexHandle>,
    FacePropsT: PropList<FaceHandle>,
{
    type Error = Error;

    fn write_to(&self, mut w: impl Write) -> Result<(), Self::Error> {
        // ===================================================================
        // ===== Write header (this part is always ASCII)
        // ===================================================================
        // Magic signature
        w.write_all(b"ply\n")?;

        // The line defining the format of the file
        let format_line = match self.ser.encoding {
            Encoding::Ascii => b"format ascii 1.0\n" as &[_],
            Encoding::BinaryBigEndian => b"format binary_big_endian 1.0\n",
            Encoding::BinaryLittleEndian => b"format binary_little_endian 1.0\n",
        };
        w.write_all(format_line)?;

        // Add all comments
        for comment in &self.ser.comments {
            writeln!(w, "comment {}", comment)?;
        }

        // Define `vertex` element with all properties
        writeln!(w, "element vertex {}", self.mesh.num_vertices())?;
        self.vertex_props.write_header(&mut w)?;

        // Define `face` element with all properties
        writeln!(w, "element face {}", self.mesh.num_faces())?;
        writeln!(w, "property list uchar uint vertex_indices")?;
        self.face_props.write_header(&mut w)?;

        w.write_all(b"end_header\n")?;


        // ===================================================================
        // ===== Write body
        // ===================================================================
        macro_rules! do_with_block {
            ($block:ident) => {{
                // Write all vertex properties
                for v in self.mesh.vertices() {
                    let mut block = $block::new(&mut w);
                    self.vertex_props.write_block(v.handle(), &mut block)?;
                    block.finish()?;
                }

                for f in self.mesh.faces() {
                    let mut block = $block::new(&mut w);

                    // Write special `vertex_indices` data
                    let indices = self.mesh.vertices_of_face(f.handle());
                    block.add(&3u8)?;
                    block.add(&[indices[0].id(), indices[1].id(), indices[2].id()])?;

                    // Write all properties
                    self.face_props.write_block(f.handle(), &mut block)?;

                    block.finish()?;
                }
            }}
        }

        match self.ser.encoding {
            Encoding::Ascii => do_with_block!(AsciiBlock),
            Encoding::BinaryBigEndian => do_with_block!(BinaryBeBlock),
            Encoding::BinaryLittleEndian => do_with_block!(BinaryLeBlock),
        }

        Ok(())
    }
}


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

// ----- ListPosElem --------------------------------------------------------
#[derive(Debug)]
pub struct ListVertexNormalElem<'m, MapT, TailT> {
    map: &'m MapT,
    tail: TailT,
}

impl<MapT, TailT> PropList<VertexHandle> for ListVertexNormalElem<'_, MapT, TailT>
where
    TailT: PropList<VertexHandle>,
    MapT: VertexPropMap,
    MapT::Target: Vec3Like,
    <MapT::Target as Vec3Like>::Scalar: SingleSerialize,
{
    fn write_header(&self, w: &mut impl Write) -> Result<(), Error> {
        self.tail.write_header(w)?;

        let ty = <<MapT::Target as Vec3Like>::Scalar as SingleSerialize>::SINGLE_TYPE;
        let ty = ty.ply_type_name();
        writeln!(w, "property {} nx", ty)?;
        writeln!(w, "property {} ny", ty)?;
        writeln!(w, "property {} nz", ty)?;

        Ok(())
    }

    fn write_block(&self, handle: VertexHandle, block: &mut impl Block) -> Result<(), Error> {
        self.tail.write_block(handle, {block})?;

        let pos = self.map.get(handle).unwrap_or_else(|| {
            panic!("face normal PropMap incomplete: no value for handle {:?}", handle);
        });

        block.add(&pos.x())?;
        block.add(&pos.y())?;
        block.add(&pos.z())?;

        Ok(())
    }
}


// ----- ListSingleElem ------------------------------------------------------
#[derive(Debug)]
pub struct ListSingleElem<'a , TailT, MapT: 'a> {
    name: String,
    map: &'a MapT,
    tail: TailT,
}

impl<H: Handle, TailT: PropList<H>, MapT> PropList<H> for ListSingleElem<'_, TailT, MapT>
where
    MapT: PropMap<H>,
    MapT::Target: Serialize,
{
    fn write_header(&self, w: &mut impl Write) -> Result<(), Error> {
        self.tail.write_header(w)?;

        match <MapT::Target as Serialize>::TYPE {
            PropType::Single(ty) => {
                writeln!(w, "property {} {}", ty.ply_type_name(), self.name)?;
            }
            PropType::DynLenSeq(ty) => {
                writeln!(w, "property list uint {} {}", ty.ply_type_name(), self.name)?;
            }
            PropType::FixedLenSeq { len, ty } => {
                for i in 0..len {
                    writeln!(w, "property {} {}[{}]", ty.ply_type_name(), self.name, i)?;
                }
            }
        }

        Ok(())
    }

    fn write_block(&self, handle: H, block: &mut impl Block) -> Result<(), Error> {
        self.tail.write_block(handle, {block})?;

        let prop = self.map.get(handle).unwrap_or_else(|| {
            panic!("PropMap for '{}' incomplete: no value for handle {:?}", self.name, handle);
        });
        block.add(&*prop)?;

        Ok(())
    }
}

// ===============================================================================================
// ===== Definition of blocks (including implementation of `PropSerializer`)
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

/// A PLY block which inserts spaces and newline seperators and serializes
/// values as ASCII text.
#[derive(Debug)]
struct AsciiBlock<'a, W: Write> {
    writer: &'a mut W,
    at_start: bool,
}

impl<'a, W: Write> AsciiBlock<'a, W> {
    fn new(w: &'a mut W) -> Self {
        Self {
            writer: w,
            at_start: true,
        }
    }
}

impl<W: Write> Block for AsciiBlock<'_, W> {
    fn add(&mut self, prop: &impl Serialize) -> Result<(), Error> {
        if self.at_start {
            self.at_start = false;
        } else {
            self.writer.write_all(b" ")?;
        }

        prop.serialize(self)
    }

    fn finish(self) -> Result<(), Error> {
        self.writer.write_all(b"\n")?;
        Ok(())
    }
}

impl<W: Write> PropSerializer for &mut AsciiBlock<'_, W> {
    fn serialize_i8(self, v: i8) -> Result<(), Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_i16(self, v: i16) -> Result<(), Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_i32(self, v: i32) -> Result<(), Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_u8(self, v: u8) -> Result<(), Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_u16(self, v: u16) -> Result<(), Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_u32(self, v: u32) -> Result<(), Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_f32(self, v: f32) -> Result<(), Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_f64(self, v: f64) -> Result<(), Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }

    fn serialize_fixed_len_seq<'a, I, E>(self, values: I) -> Result<(), Error>
    where
        I: IntoIterator<Item = &'a E>,
        E: 'a + SingleSerialize,
    {
        // Fixed sized sequences don't need to encode the length for each
        // property.
        self.at_start = true;
        for v in values {
            self.add(v)?;
        }

        Ok(())
    }

    fn serialize_dyn_len_seq<'a, I, E>(self, values: I) -> Result<(), Error>
    where
        I: IntoIterator<Item = &'a E>,
        I::IntoIter: ExactSizeIterator,
        E: 'a + SingleSerialize,
    {
        let iter = values.into_iter();

        self.at_start = true;
        self.add(&(iter.len() as u32))?;

        for v in iter {
            self.add(v)?;
        }

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

        impl<W: Write> Block for $name<'_, W> {
            fn add(&mut self, prop: &impl Serialize) -> Result<(), Error> {
                prop.serialize(self)
            }

            fn finish(self) -> Result<(), Error> {
                Ok(())
            }
        }

        impl<W: Write> PropSerializer for &mut $name<'_, W> {
            fn serialize_i8(self, v: i8) -> Result<(), Error> {
                self.writer.write_i8(v).map_err(|e| e.into())
            }
            fn serialize_i16(self, v: i16) -> Result<(), Error> {
                self.writer.write_i16::<$endianess>(v).map_err(|e| e.into())
            }
            fn serialize_i32(self, v: i32) -> Result<(), Error> {
                self.writer.write_i32::<$endianess>(v).map_err(|e| e.into())
            }
            fn serialize_u8(self, v: u8) -> Result<(), Error> {
                self.writer.write_u8(v).map_err(|e| e.into())
            }
            fn serialize_u16(self, v: u16) -> Result<(), Error> {
                self.writer.write_u16::<$endianess>(v).map_err(|e| e.into())
            }
            fn serialize_u32(self, v: u32) -> Result<(), Error> {
                self.writer.write_u32::<$endianess>(v).map_err(|e| e.into())
            }
            fn serialize_f32(self, v: f32) -> Result<(), Error> {
                self.writer.write_f32::<$endianess>(v).map_err(|e| e.into())
            }
            fn serialize_f64(self, v: f64) -> Result<(), Error> {
                self.writer.write_f64::<$endianess>(v).map_err(|e| e.into())
            }

            fn serialize_fixed_len_seq<'a, I, E>(self, values: I) -> Result<(), Error>
            where
                I: IntoIterator<Item = &'a E>,
                E: 'a + SingleSerialize,
            {
                // Fixed sized sequences don't need to encode the length for each
                // property.
                for v in values {
                    self.add(v)?;
                }

                Ok(())
            }

            fn serialize_dyn_len_seq<'a, I, E>(self, values: I) -> Result<(), Error>
            where
                I: IntoIterator<Item = &'a E>,
                I::IntoIter: ExactSizeIterator,
                E: 'a + SingleSerialize,
            {
                let iter = values.into_iter();

                self.add(&(iter.len() as u32))?;

                for v in iter {
                    self.add(v)?;
                }

                Ok(())
            }
        }
    }
}

gen_binary_block!(BinaryBeBlock, BigEndian);
gen_binary_block!(BinaryLeBlock, LittleEndian);
