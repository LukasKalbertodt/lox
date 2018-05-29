use std::{
    io::{self, Write},
    ops,
};

use crate::{
    TriMesh,
    Pos3D,
    handle::{FaceHandle, Handle, HandleIndex, VertexHandle},
    map::{PropMap, FaceMap, VertexMap},
    io::{PrimitiveSerialize},
};


pub struct Ply<'a, Idx: 'a + HandleIndex> {
    format: PlyFormat,
    vertex_attrs: Vec<(String, &'a SerMap<'a, VertexHandle<Idx>>)>,
    face_attrs: Vec<(String, &'a SerMap<'a, FaceHandle<Idx>>)>,
}

trait SerMap<'a, H: Handle> {
    fn get(&self, handle: H) -> Option<&(PrimitiveSerialize + 'a)>;
}

impl<'a, H: Handle, M> SerMap<'a, H> for M
where
    M: PropMap<H>,
    M::Output: 'a + PrimitiveSerialize + Sized,
{
    fn get(&self, handle: H) -> Option<&(PrimitiveSerialize + 'a)> {
        <M as PropMap<H>>::get(self, handle).map(|p| p as &PrimitiveSerialize)
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

    pub fn with_vertex_positions<M>(&mut self, map: &'a M) -> &mut Self
    where
        M: VertexMap<Idx> + ops::Index<VertexHandle<Idx>>,
        <M as ops::Index<VertexHandle<Idx>>>::Output: Pos3D + Sized,
        <<M as ops::Index<VertexHandle<Idx>>>::Output as Pos3D>::Scalar: PrimitiveSerialize,
    {
        // TODO
        self
    }

    pub fn add_vertex_attr<M, S>(&mut self, s: S, map: &'a M) -> &mut Self
    where
        S: Into<String>,
        M: VertexMap<Idx>,
        M::Output: PrimitiveSerialize + Sized,
    {
        self.vertex_attrs.push((s.into(), map));
        self
    }

    pub fn add_face_attr<M, S>(&mut self, s: S, map: &'a M) -> &mut Self
    where
        S: Into<String>,
        M: FaceMap<Idx>,
        M::Output: PrimitiveSerialize + Sized,
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

                        attr_map.get(vertex)
                            .expect("attempt to use a incomplete PropMap for PLY serialization")
                            .write_ascii(w)?;
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
