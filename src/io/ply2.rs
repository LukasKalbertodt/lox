use std::{
    io::{self, Write},
    ops::Deref,
};

use crate::{
    TriMesh,
    Pos3Like,
    handle::{DefaultIndex, DefaultIndexExt, FaceHandle, Handle, VertexHandle},
    map::{PropMap, FaceMap, VertexMap},
    io::{PropKind, MeshSerializer, PropSerialize, PrimitiveSerialize, PrimitiveType},
};




pub enum PlyFormat {
    Ascii,
    BinaryBigEndian,
    BinaryLittleEndian,
}


pub struct Ply {
    format: PlyFormat,
    vertex_props: Vec<Property>,
}

struct Property {
    kind: PropKind,
}


impl Ply {
    pub fn ascii() -> Self {
        Self::new(PlyFormat::Ascii)
    }

    pub fn binary() -> Self {
        Self::new(PlyFormat::BinaryBigEndian)
    }

    pub fn new(format: PlyFormat) -> Self {
        Self {
            format,
            vertex_props: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub enum PlyError {
    Something,
}

impl From<io::Error> for PlyError {
    fn from(_src: io::Error) -> Self {
        PlyError::Something
    }
}

impl MeshSerializer for Ply {
    type Error = PlyError;

    fn add_vertex_prop<PropT: PropSerialize>(
        &mut self,
        prop: &PropT,
    ) -> Result<&mut Self, Self::Error> {
        self.vertex_props.push(Property {
            kind: PropT::kind(),
        });

        Ok(self)
    }

    fn write<MeshT>(&mut self, mesh: &MeshT, mut w: impl Write) -> Result<(), Self::Error>
    where
        MeshT: TriMesh,
        MeshT::VertexProp: PropSerialize,
    {
        // Add the properties stored inside the mesh to our property list
        self.vertex_props.push(Property {
            kind: MeshT::VertexProp::kind(),
        });

        // ===================================================================
        // Write header
        // ===================================================================
        w.write_all(b"ply\n")?;

        match self.format {
            PlyFormat::Ascii => w.write_all(b"format ascii 1.0\n")?,
            PlyFormat::BinaryBigEndian => w.write_all(b"format binary_big_endian 1.0\n")?,
            PlyFormat::BinaryLittleEndian => w.write_all(b"format binary_little_endian 1.0\n")?,
        }

        let idx_type = match DefaultIndex::num_bytes() {
            1 => "uchar",
            2 => "ushort",
            4 => "uint",
            _ => return Err(PlyError::Something),
        };

        // List elements
        writeln!(w, "element vertex {}", mesh.num_vertices())?;
        for prop in &self.vertex_props {
            match &prop.kind {
                PropKind::Position { scalar_ty } => {
                    let ty_name = primitive_ply_type_name(scalar_ty)?;

                    writeln!(w, "property {} x", ty_name)?;
                    writeln!(w, "property {} y", ty_name)?;
                    writeln!(w, "property {} z", ty_name)?;
                }
                PropKind::Normal { scalar_ty } => {
                    unimplemented!()
                }
                PropKind::Primitive { name, ty } => {
                    let ty_name = primitive_ply_type_name(ty)?;
                    writeln!(w, "property {} {}", ty_name, name)?;
                }
            }
        }


        writeln!(w, "element face {}", mesh.num_faces())?;
        writeln!(w, "property list uchar {} vertex_indices", idx_type)?;

        // TODO: would be nice to write some meta data, such as date, into the
        // file

        w.write_all(b"end_header\n")?;


        // // ===================================================================
        // // Write body
        // // ===================================================================
        // match self.format {
        //     PlyFormat::Ascii => {
        //         for vertex in mesh.vertices() {
        //             let mut first = true;
        //             for (_, _, attr_map) in &self.vertex_attrs {
        //                 if first {
        //                     first = false;
        //                 } else {
        //                     write!(w, " ")?;
        //                 }

        //                 attr_map
        //                     .deref()
        //                     .get(vertex)
        //                     .expect("attempt to use a incomplete PropMap for PLY serialization")
        //                     .write_ascii(w)?;
        //             }
        //             writeln!(w, "")?;
        //         }

        //         for face in mesh.faces() {
        //             // TODO: other face attributes
        //             w.write_all(b"3")?;
        //             for &v in &mesh.vertices_of_face(face) {
        //                 w.write_all(b" ")?;
        //                 v.idx().write_ascii(w)?;
        //             }
        //             w.write_all(b"\n")?;
        //         }
        //         // let vertices = mesh.vertice_of_face
        //     }
        //     _ => unimplemented!(),
        // }

        Ok(())
    }
}


fn primitive_ply_type_name(ty: &PrimitiveType) -> Result<&'static str, PlyError> {
    match ty {
        PrimitiveType::Uint8   => Ok("uchar"),
        PrimitiveType::Uint16  => Ok("ushort"),
        PrimitiveType::Uint32  => Ok("uint"),
        PrimitiveType::Uint64  => Err(PlyError::Something),
        PrimitiveType::Int8    => Ok("char"),
        PrimitiveType::Int16   => Ok("short"),
        PrimitiveType::Int32   => Ok("int"),
        PrimitiveType::Int64   => Err(PlyError::Something),
        PrimitiveType::Float32 => Ok("float"),
        PrimitiveType::Float64 => Ok("double"),
    }
}
