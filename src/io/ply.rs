use std::{
    io::{self, Write},
    ops,
};

use crate::{
    TriMesh,
    handle::{FaceHandle, Handle, HandleIndex, VertexHandle},
    map::{AttrMap, FaceMap, VertexMap},
    io::{PrimitiveSerialize},
};


pub struct Ply<'a, Idx: 'a + HandleIndex> {
    format: PlyFormat,
    vertex_attrs: Vec<(String, &'a SerMap<VertexHandle<Idx>>)>,
    face_attrs: Vec<(String, &'a SerMap<FaceHandle<Idx>>)>,
}

trait SerMap<H: Handle> {
    fn get(&self, handle: H) -> &PrimitiveSerialize;
}

impl<M> SerMap<M::Handle> for M
where
    M: AttrMap,
    M::Output: PrimitiveSerialize + Sized,
{
    fn get(&self, handle: M::Handle) -> &PrimitiveSerialize {
        &self[handle]
    }
}

impl<'a, Idx: 'a + HandleIndex> Ply<'a, Idx> {
    pub fn ascii() -> Self {
        Self::new(PlyFormat::Ascii)
    }

    pub fn binary() -> Self {
        Self::new(PlyFormat::BinaryBigEndian)
    }

    pub fn new(format: PlyFormat) -> Self {
        Self {
            format,
            vertex_attrs: Vec::new(),
            face_attrs: Vec::new(),
        }
    }

    pub fn add_vertex_attr<M, S>(&mut self, s: S, map: &'a M) -> &mut Self
    where
        S: Into<String>,
        M: VertexMap<Idx> + ops::Index<VertexHandle<Idx>>,
        <M as ops::Index<VertexHandle<Idx>>>::Output: PrimitiveSerialize + Sized,
    {
        self.vertex_attrs.push((s.into(), map));
        self
    }

    pub fn add_face_attr<M, S>(&mut self, s: S, map: &'a M) -> &mut Self
    where
        S: Into<String>,
        M: FaceMap<Idx> + ops::Index<FaceHandle<Idx>>,
        <M as ops::Index<FaceHandle<Idx>>>::Output: PrimitiveSerialize + Sized,
    {
        self.face_attrs.push((s.into(), map));
        self
    }

    pub fn write<M, W>(&self, w: &mut W, mesh: &M) -> Result<(), io::Error>
    where M: TriMesh<Idx = Idx>,
          M::Idx: PrimitiveSerialize,
          W: Write,
    {
        // ===================================================================
        // Write header
        // ===================================================================
        w.write_all(b"ply\n")?;

        match self.format {
            PlyFormat::Ascii => w.write_all(b"format ascii 1.0\n")?,
            PlyFormat::BinaryBigEndian => w.write_all(b"format binary_big_endian 1.0\n")?,
            PlyFormat::BinaryLittleEndian => w.write_all(b"format binary_little_endian 1.0\n")?,
        }

        let idx_type = match M::Idx::num_bytes() {
            1 => "uchar",
            2 => "ushort",
            4 => "uint",
            _ => panic!(),   // TODO: return error!
        };

        // List elements
        writeln!(w, "element vertex {}", mesh.num_vertices())?;
        for (attr_name, _) in &self.vertex_attrs {
            // TODO: It's not always `int`
            writeln!(w, "property int {}", attr_name)?;
        }


        writeln!(w, "element face {}", mesh.num_faces())?;
        writeln!(w, "property list uchar {} vertex_indices", idx_type)?;

        // TODO: would be nice to write some meta data, such as date, into the
        // file

        w.write_all(b"end_header\n")?;


        // ===================================================================
        // Write body
        // ===================================================================
        match self.format {
            PlyFormat::Ascii => {
                for vertex in mesh.vertices() {
                    let mut first = true;
                    for (_, attr_map) in &self.vertex_attrs {
                        if first {
                            first = false;
                        } else {
                            write!(w, " ")?;
                        }

                        attr_map.get(vertex).write_ascii(w)?;
                    }
                    writeln!(w, "")?;
                }

                for face in mesh.faces() {
                    // TODO: other face attributes
                    w.write_all(b"3")?;
                    for &v in &mesh.vertices_of_face(face) {
                        w.write_all(b" ")?;
                        v.idx().write_ascii(w)?;
                    }
                }
                // let vertices = mesh.vertice_of_face
            }
            _ => unimplemented!(),
        }

        Ok(())
    }
}

pub enum PlyFormat {
    Ascii,
    BinaryBigEndian,
    BinaryLittleEndian,
}

// pub enum Error {

// }

// impl
