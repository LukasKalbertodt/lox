#![allow(unused_imports)] // TODO

use std::{
    io::{self, Write},
};

use byteorder::{LittleEndian, WriteBytesExt};
use cgmath::prelude::*;

use crate::{
    Mesh, MeshUnsorted, ExplicitFace, ExplicitVertex,
    handle::{Handle, VertexHandle},
    map::{EmptyMap, FacePropMap, VertexPropMap},
    math::{Pos3Like, Vec3Like},
    io::{IntoMeshWriter, MeshWriter},
};
use super::{Error, Format, Serialize, PropSerializer};



// ===============================================================================================
// ===== PLY Serializer
// ===============================================================================================


#[derive(Clone, Debug)]
pub struct Serializer {
    format: Format,
    comments: Vec<String>,
}

impl Serializer {
    pub fn ascii() -> Self {
        Self::new(Format::Ascii)
    }

    pub fn binary() -> Self {
        Self::new(Format::BinaryBigEndian)
    }

    pub fn new(format: Format) -> Self {
        Self {
            format,
            comments: vec![],
        }
    }

    pub fn add_comment(mut self, comment: impl Into<String>) -> Self {
        self.comments.push(comment.into());
        self
    }
}

impl<'a, MeshT, PosM> IntoMeshWriter<'a, MeshT, PosM> for Serializer
where
    MeshT: 'a + Mesh + MeshUnsorted + ExplicitFace + ExplicitVertex,
    PosM: 'a + VertexPropMap,
    PosM::Target: Pos3Like,
    <PosM::Target as Pos3Like>::Scalar: Serialize,
{
    type Writer = Writer<'a, MeshT, ListPosElem<'a, PosM, EmptyList>>;
    fn into_writer(self, mesh: &'a MeshT, vertex_positions: &'a PosM) -> Self::Writer {
        Writer {
            ser: self,
            mesh,
            vertex_props: ListPosElem {
                map: vertex_positions,
                tail: EmptyList,
            },
        }
    }
}


// ===============================================================================================
// ===== PLY Writer
// ===============================================================================================

#[derive(Debug)]
pub struct Writer<'a, MeshT, VertexPropsT>
where
    MeshT: Mesh + MeshUnsorted + ExplicitFace + ExplicitVertex,
    VertexPropsT: PropList<VertexHandle>,
{
    ser: Serializer,
    mesh: &'a MeshT,
    vertex_props: VertexPropsT,
}


// impl<'a, MeshT> Writer<'a, MeshT>
// where // TODO: remove once implied bounds land
//     MeshT: Mesh + MeshUnsorted + ExplicitFace + ExplicitVertex,

// {
//     fn new<PosM>(ser: Serializer, mesh: &'a MeshT, vertex_positions: &'a PosM) -> Self
//     where
//         PosM: VertexPropMap,
//         PosM::Target: Pos3Like,
//         <PosM::Target as Pos3Like>::Scalar: Serialize,
//     {

//     }
// }

impl<MeshT, VertexPropsT> MeshWriter for Writer<'_, MeshT, VertexPropsT>
where // TODO: remove once implied bounds land
    MeshT: Mesh + MeshUnsorted + ExplicitFace + ExplicitVertex,
    VertexPropsT: PropList<VertexHandle>,
{
    type Error = Error;

    fn write_to(&self, mut w: impl Write) -> Result<(), Self::Error> {
        // ===================================================================
        // ===== Write header (this part is always ASCII)
        // ===================================================================
        // Magic signature
        w.write_all(b"ply\n")?;

        // The line defining the format of the file
        let format_line = match self.ser.format {
            Format::Ascii => b"format ascii 1.0\n" as &[_],
            Format::BinaryBigEndian => b"format binary_big_endian 1.0\n",
            Format::BinaryLittleEndian => b"format binary_little_endian 1.0\n",
        };
        w.write_all(format_line)?;


        // TODO: would be nice to write some meta data, such as date, into the
        // file

        // TODO: it would be nice to let the user add comments to the file if
        // they want to.


        // Define `vertex` element with all properties
        writeln!(w, "element vertex {}", self.mesh.num_vertices())?;
        self.vertex_props.write_header(&mut w)?;

        // Define `face` element with all properties
        writeln!(w, "element face {}", self.mesh.num_faces())?;
        writeln!(w, "property list uchar uint vertex_indices")?;
        // face_props.write_header(&mut w)?;

        w.write_all(b"end_header\n")?;

        match self.ser.format {
            Format::Ascii => {
                for v in self.mesh.vertices() {
                    // writeln!(w, "{:?}", v.handle());
                    self.vertex_props.write_block(v.handle(), AsciiBlock::new(&mut w))?;
                }

                for f in self.mesh.faces() {
                    let mut block = AsciiBlock::new(&mut w);

                    // Write special `vertex_indices` data
                    let indices = self.mesh.vertices_of_face(f.handle());
                    block.add(&3u8)?;
                    block.add(&indices[0].id())?;
                    block.add(&indices[1].id())?;
                    block.add(&indices[2].id())?;
                    block.finish()?;
                }
            }
            Format::BinaryBigEndian => unimplemented!(),
            Format::BinaryLittleEndian => unimplemented!(),
        }

        Ok(())
    }
}


// ===============================================================================================
// ===== Helper stuff for the heterogeneous list stored inside the MeshWriter
// ===============================================================================================

pub trait PropList<H: Handle> {
    fn write_header(&self, w: &mut impl Write) -> Result<(), Error>;
    fn write_block(&self, handle: H, block: impl Block) -> Result<(), Error>;
}


#[derive(Debug)]
pub struct EmptyList;

impl<H: Handle> PropList<H> for EmptyList {
    fn write_header(&self, _: &mut impl Write) -> Result<(), Error> {
        Ok(())
    }
    fn write_block(&self, _: H, block: impl Block) -> Result<(), Error> {
        block.finish()
    }
}

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
    <MapT::Target as Pos3Like>::Scalar: Serialize,
{
    fn write_header(&self, w: &mut impl Write) -> Result<(), Error> {
        let type_name = <<MapT::Target as Pos3Like>::Scalar as Serialize>::HEADER_TYPE_NAME;
        writeln!(w, "property {} x", type_name);
        writeln!(w, "property {} y", type_name);
        writeln!(w, "property {} z", type_name);
        self.tail.write_header(w)
    }

    fn write_block(&self, handle: VertexHandle, mut block: impl Block) -> Result<(), Error> {
        let pos = self.map.get(handle).unwrap_or_else(|| {
            panic!("vertex position PropMap incomplete: no value for handle {:?}", handle);
        });

        block.add(pos.x())?;
        block.add(pos.y())?;
        block.add(pos.z())?;

        self.tail.write_block(handle, block)
    }
}


// #[derive(Debug)]
// pub struct ListSingleElem<TailT> {
//     name: String,
//     tail: TailT,
// }

// impl<H: Handle, TailT: PropList<H>> PropList<H> for ListSingleElem<TailT> {
//     fn write_header(&self, w: &mut impl Write) -> Result<(), Error> {
//         write!(w, "TODO: {}", self.name);
//         self.tail.write_header(w)
//     }
// }

// ===============================================================================================
// ===== Definition of blocks (properties for one specific element)
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

/// A PLY block which inserts spaces and newline seperators.
#[derive(Debug)]
struct AsciiBlock<'a, W: 'a + Write> {
    writer: &'a mut W,
    at_start: bool,
}

impl<'a, W: 'a + Write> AsciiBlock<'a, W> {
    fn new(w: &'a mut W) -> Self {
        Self {
            writer: w,
            at_start: true,
        }
    }
}

impl<'a, W: 'a + Write> Block for AsciiBlock<'a, W> {
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

impl<'a, W: Write + 'a> PropSerializer for &mut AsciiBlock<'a, W> {
    type Error = Error;

    fn serialize_i8(self, v: i8) -> Result<(), Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_i16(self, v: i16) -> Result<(), Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_i32(self, v: i32) -> Result<(), Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_u8(self, v: u8) -> Result<(), Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_u16(self, v: u16) -> Result<(), Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_u32(self, v: u32) -> Result<(), Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_f32(self, v: f32) -> Result<(), Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_f64(self, v: f64) -> Result<(), Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
}



// ===============================================================================================
// ===== Serializer for PLY
// ===============================================================================================

// /// Serializes properties as PLY ASCII (simply using the `Display` impl).
// ///
// /// Unfortunately, the PLY format isn't too detailed. So we can't know for sure
// /// how to serialize numbers. E.g. it's not specified how many decimal digits
// /// are allowed/required and if floats should be printed in scientific notation
// /// or not. So we just use `Display` which seems to work.
// struct AsciiSerializer<'a, W: Write + 'a + ?Sized> {
//     writer: &'a mut W,
// }

// impl<'a, W: Write + 'a + ?Sized> AsciiSerializer<'a, W> {
//     fn new(w: &'a mut W) -> Self {
//         Self {
//             writer: w,
//         }
//     }
// }
