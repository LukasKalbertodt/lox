use std::io::{self, Write};

use super::{MeshSerializer, PrimitiveSerialize};
use handle::{Handle, HandleIndex, VertexHandle};
use maps::AttrMap;
use TriMesh;


// pub struct Ply<'a, Idx> {
//     format: Format,
//     attributes: HashMap<String, ()>,
// }

// impl<'a, M, Idx> Ply<'a, M>
// where
//     Idx: HandleIndex,
//     M: AttrMap<VertexHandle<Idx>>,
//     // M::Output:
// {
//     pub fn ascii() -> Self {
//         Self::new(Format::Ascii)
//     }

//     pub fn binary() -> Self {
//         Self::new(Format::BinaryBigEndian)
//     }

//     pub fn new(format: Format) -> Self {
//         Self {
//             format,

//         }
//     }

//     pub fn with_positions(self, map: &'a M) -> Self {
//         Self {
//             format: self.format,
//             positions: Some(map),
//         }
//     }
// }

// impl<'a, M> MeshSerializer for Ply<'a, M> {
//     type Err = io::Error;

//     fn serialize<M, W>(&self, w: &mut W, mesh: &M) -> Result<(), Self::Err>
//     where M: TriMesh,
//           M::Idx: PrimitiveSerialize,
//           W: Write,
//     {
//         // ===================================================================
//         // Write header
//         // ===================================================================
//         w.write_all(b"ply\n")?;

//         match self.format {
//             Format::Ascii => w.write_all(b"format ascii 1.0\n")?,
//             Format::BinaryBigEndian => w.write_all(b"format binary_big_endian 1.0\n")?,
//             Format::BinaryLittleEndian => w.write_all(b"format binary_little_endian 1.0\n")?,
//         }

//         let idx_type = match M::Idx::num_bytes() {
//             1 => "uchar",
//             2 => "ushort",
//             4 => "uint",
//             _ => panic!(),   // TODO: return error!
//         };

//         // List elements
//         writeln!(w, "element vertex {}", mesh.num_vertices())?;

//         writeln!(w, "element face {}", mesh.num_faces())?;
//         writeln!(w, "property list uchar {} vertex_indices", idx_type)?;

//         // TODO: would be nice to write some meta data, such as date, into the
//         // file

//         w.write_all(b"end_header\n")?;


//         // ===================================================================
//         // Write body
//         // ===================================================================
//         match self.format {
//             Format::Ascii => {
//                 for face in mesh.faces() {
//                     w.write_all(b"3")?;
//                     for &v in &mesh.vertices_of_face(face) {
//                         w.write_all(b" ")?;
//                         v.idx().write_ascii(w)?;
//                     }
//                 }
//                 // let vertices = mesh.vertice_of_face
//             }
//             _ => unimplemented!(),
//         }

//         Ok(())
//     }
// }

enum Format {
    Ascii,
    BinaryBigEndian,
    BinaryLittleEndian,
}

// pub enum Error {

// }

// impl
