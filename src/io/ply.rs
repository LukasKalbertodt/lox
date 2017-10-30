use std::io::{self, Write};
use std::fmt::Display;

use super::MeshSerialize;
use handle::HandleIndex;
use TriMesh;


pub struct Ply<W: Write> {
    writer: W,
    format: Format,
}

impl<W: Write> Ply<W> {
    pub fn ascii(writer: W) -> Self {
        Self {
            writer,
            format: Format::Ascii,
        }
    }

    pub fn binary_be(writer: W) -> Self {
        Self {
            writer,
            format: Format::BinaryBigEndian,
        }
    }

    pub fn binary_le(writer: W) -> Self {
        Self {
            writer,
            format: Format::BinaryLittleEndian,
        }
    }
}

impl<W: Write> MeshSerialize for Ply<W> {
    type Err = io::Error;
    fn serialize<M>(mut self, mesh: &M) -> Result<(), Self::Err>
        where M: TriMesh,
              M::Idx: Display
    {
        let w = &mut self.writer;

        // ===================================================================
        // Write header
        // ===================================================================
        w.write_all(b"ply\n")?;

        match self.format {
            Format::Ascii => w.write_all(b"format ascii 1.0\n")?,
            Format::BinaryBigEndian => w.write_all(b"format binary_big_endian 1.0\n")?,
            Format::BinaryLittleEndian => w.write_all(b"format binary_little_endian 1.0\n")?,
        }

        let idx_type = match M::Idx::num_bytes() {
            1 => "uchar",
            2 => "ushort",
            4 => "uint",
            _ => panic!(),   // TODO: return error!
        };

        writeln!(w, "element face {}", mesh.num_faces())?;
        writeln!(w, "property list uchar {} vertex_indices", idx_type)?;

        w.write_all(b"end_header\n")?;


        // ===================================================================
        // Write body
        // ===================================================================
        match self.format {
            Format::Ascii => {
                for face in mesh.faces() {
                    w.write_all(b"3")?;
                    for v in mesh.vertices_of_face(face) {
                        write!(w, " {}", v)?;
                    }
                }
                // let vertices = mesh.vertice_of_face
            }
            _ => unimplemented!(),
        }

        Ok(())
    }
}

enum Format {
    Ascii,
    BinaryBigEndian,
    BinaryLittleEndian,
}

// pub enum Error {

// }

// impl
