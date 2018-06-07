use std::{
    io::{self, Write},
    ops::Deref,
};

use crate::{
    TriMesh, Pos3Like, Vec3Like,
    handle::{DefaultIndex, DefaultIndexExt, FaceHandle, Handle, VertexHandle},
    map::{PropMap, FaceMap, VertexMap},
    io::{
        IntoMeshWriter, LabeledPropSet, MeshWriter, PrimitiveSerialize, PrimitiveSerializer,
        PrimitiveType, PropLabel, PropSerializer,
    },
};



#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PlyFormat {
    Ascii,
    BinaryBigEndian,
    BinaryLittleEndian,
}


#[derive(Clone, Copy, Debug)]
pub struct Ply {
    format: PlyFormat,
}

impl Ply {
    pub fn ascii() -> Self {
        Self::new(PlyFormat::Ascii)
    }

    pub fn binary() -> Self {
        Self::new(PlyFormat::BinaryBigEndian)
    }

    pub fn new(format: PlyFormat) -> Self {
        Self { format }
    }
}

impl<'a, MeshT> IntoMeshWriter<'a, MeshT> for Ply
where
    MeshT: TriMesh + 'a,
    MeshT::VertexProp: LabeledPropSet,
{
    type Error = PlyError;
    type Writer = PlyWriter<'a, MeshT>;

    fn serialize(self, mesh: &'a MeshT) -> Result<Self::Writer, Self::Error> {
        // Add the properties stored inside the mesh to our property list
        let mut vertex_props = Vec::new();
        vertex_props.push(PropertySet {
            labels: MeshT::VertexProp::LABELS,
            serialize: Box::new(move |w, handle| {
                mesh.vertex_prop(handle)
                    .unwrap()
                    .serialize(PlySerializer { writer: w })
            }),
        });

        Ok(PlyWriter {
            format: self.format,
            mesh,
            vertex_props,
        })
    }
}

pub struct PlyWriter<'a, MeshT: 'a> {
    format: PlyFormat,
    mesh: &'a MeshT,
    vertex_props: Vec<PropertySet<'a>>,
}

struct PropertySet<'a> {
    labels: &'a [PropLabel],
    serialize: Box<'a + Fn(&mut io::Write, VertexHandle) -> Result<(), PlyError>>,
}




#[derive(Debug, Fail)]
pub enum PlyError {
    #[fail(display = "something :(")]
    Something,
}

impl From<io::Error> for PlyError {
    fn from(_src: io::Error) -> Self {
        PlyError::Something
    }
}

impl<'a, MeshT> MeshWriter<'a> for PlyWriter<'a, MeshT>
where
    MeshT: TriMesh + 'a,
    MeshT::VertexProp: LabeledPropSet,
{
    type Error = PlyError;

    // fn add_vertex_prop<PropT: LabeledPropSet>(
    //     &mut self,
    //     prop: &PropT,
    // ) -> Result<&mut Self, Self::Error> {
    //     // self.vertex_props.push(Property {
    //     //     kind: PropT::kind(),
    //     //     serialize: Box::new(|w, handle| {

    //     //     }),
    //     // });

    //     // Ok(self)
    //     unimplemented!()
    // }

    fn write(&mut self, mut w: impl Write) -> Result<(), Self::Error> {
        // ===================================================================
        // Write header
        // ===================================================================
        w.write_all(b"ply\n")?;

        match self.format {
            PlyFormat::Ascii => w.write_all(b"format ascii 1.0\n")?,
            PlyFormat::BinaryBigEndian => w.write_all(b"format binary_big_endian 1.0\n")?,
            PlyFormat::BinaryLittleEndian => w.write_all(b"format binary_little_endian 1.0\n")?,
        }

        let idx_type = match DefaultIndex::NUM_BYTES {
            1 => "uchar",
            2 => "ushort",
            4 => "uint",
            _ => return Err(PlyError::Something),
        };

        // List elements
        writeln!(w, "element vertex {}", self.mesh.num_vertices())?;
        for prop in &self.vertex_props {
            for label in prop.labels {
                match label {
                    PropLabel::Position { scalar_ty } => {
                        let ty_name = primitive_ply_type_name(scalar_ty)?;

                        writeln!(w, "property {} x", ty_name)?;
                        writeln!(w, "property {} y", ty_name)?;
                        writeln!(w, "property {} z", ty_name)?;
                    }
                    PropLabel::Normal { scalar_ty } => {
                        unimplemented!()
                    }
                    PropLabel::Named { name, ty } => {
                        let ty_name = primitive_ply_type_name(ty)?;
                        writeln!(w, "property {} {}", ty_name, name)?;
                    }
                }
            }
        }


        writeln!(w, "element face {}", self.mesh.num_faces())?;
        writeln!(w, "property list uchar {} vertex_indices", idx_type)?;

        // TODO: would be nice to write some meta data, such as date, into the
        // file

        w.write_all(b"end_header\n")?;


        // ===================================================================
        // Write body
        // ===================================================================
        match self.format {
            PlyFormat::Ascii => {
                for vertex_handle in self.mesh.vertices() {
                    let mut first = true;
                    for prop in &self.vertex_props {
                        if first {
                            first = false;
                        } else {
                            write!(w, " ")?;
                        }

                        (prop.serialize)(&mut w, vertex_handle);
                        // attr_map
                        //     .deref()
                        //     .get(vertex)
                        //     .expect("attempt to use a incomplete PropMap for PLY serialization")
                        //     .write_ascii(w)?;
                    }
                    writeln!(w, "")?;
                }

                for face in self.mesh.faces() {
                    // TODO: other face attributes
                    w.write_all(b"3")?;
                    for &v in &self.mesh.vertices_of_face(face) {
                        w.write_all(b" ")?;
                        v.idx().serialize(PlySerializer { writer: &mut w })?;
                    }
                    w.write_all(b"\n")?;
                }
            }
            _ => unimplemented!(),
        }

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


impl<'a, W: Write + 'a + ?Sized> PrimitiveSerializer for PlySerializer<'a, W> {
    type Ok = ();
    type Error = PlyError;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
}

struct PlySerializer<'a, W: Write + 'a + ?Sized> {
    writer: &'a mut W,
}

impl<'a, W: Write + 'a + ?Sized> PropSerializer for PlySerializer<'a, W> {
    type Ok = ();
    type Error = PlyError;

    fn serialize_position<PosT>(&mut self, v: &PosT) -> Result<Self::Ok, Self::Error>
    where
        PosT: Pos3Like,
        PosT::Scalar: PrimitiveSerialize
    {
        v.x().serialize(PlySerializer { writer: self.writer })?;
        write!(self.writer, " ")?;
        v.y().serialize(PlySerializer { writer: self.writer })?;
        write!(self.writer, " ")?;
        v.z().serialize(PlySerializer { writer: self.writer })?;

        Ok(())
    }

    fn serialize_normal<NormalT>(&mut self, v: &NormalT) -> Result<Self::Ok, Self::Error>
    where
        NormalT: Vec3Like,
        NormalT::Scalar: PrimitiveSerialize
    {
        unimplemented!()
    }

    fn serialize_named(
        &mut self,
        name: &str,
        v: &impl PrimitiveSerialize,
    ) -> Result<Self::Ok, Self::Error>
    {
        unimplemented!()
    }
}
