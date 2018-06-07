use std::{
    io::{self, Write},
    ops::Deref,
};

use byteorder::{WriteBytesExt, BigEndian};

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
        macro_rules! property_set_with_serializer {
            ($serializer:ident) => {
                PropertySet {
                    labels: MeshT::VertexProp::LABELS,
                    serialize: Box::new(move |w, handle| {
                        mesh.vertex_prop(handle)
                            .unwrap()
                            .serialize($serializer::new(w))
                    }),
                }
            }
        }

        let prop_set = match self.format {
            PlyFormat::Ascii => property_set_with_serializer!(PlyAsciiPropSerializer),
            PlyFormat::BinaryBigEndian => {
                println!("yo I passed binrary be");
                property_set_with_serializer!(PlyBinaryBePropSerializer)
            },
            PlyFormat::BinaryLittleEndian => unimplemented!(),
        };


        Ok(PlyWriter {
            format: self.format,
            mesh,
            vertex_props: vec![prop_set],
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
                        let ty_name = primitive_ply_type_name(scalar_ty)?;

                        writeln!(w, "property {} nx", ty_name)?;
                        writeln!(w, "property {} ny", ty_name)?;
                        writeln!(w, "property {} nz", ty_name)?;
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
                    for prop in &self.vertex_props {
                        (prop.serialize)(&mut w, vertex_handle);
                    }
                    writeln!(w, "")?;
                }

                for face in self.mesh.faces() {
                    // TODO: other face attributes
                    w.write_all(b"3")?;
                    for &v in &self.mesh.vertices_of_face(face) {
                        w.write_all(b" ")?;
                        v.idx().serialize(PlyAsciiPrimitiveSerializer::new(&mut w))?;
                    }
                    w.write_all(b"\n")?;
                }
            }
            PlyFormat::BinaryBigEndian => {
                for vertex_handle in self.mesh.vertices() {
                    for prop in &self.vertex_props {
                        (prop.serialize)(&mut w, vertex_handle);
                    }
                }

                for face in self.mesh.faces() {
                    // TODO: other face attributes
                    PlyBinaryBePrimitiveSerializer::new(&mut w).serialize_u8(3);
                    for &v in &self.mesh.vertices_of_face(face) {
                        v.idx().serialize(PlyBinaryBePrimitiveSerializer::new(&mut w))?;
                    }
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

struct PlyAsciiPrimitiveSerializer<'a, W: Write + 'a + ?Sized> {
    writer: &'a mut W,
}

impl<'a, W: Write + 'a + ?Sized> PlyAsciiPrimitiveSerializer<'a, W> {
    fn new(w: &'a mut W) -> Self {
        Self {
            writer: w,
        }
    }
}

impl<'a, W: Write + 'a + ?Sized> PrimitiveSerializer for PlyAsciiPrimitiveSerializer<'a, W> {
    type Error = PlyError;

    fn serialize_bool(self, v: bool) -> Result<(), Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_i8(self, v: i8) -> Result<(), Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_i16(self, v: i16) -> Result<(), Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_i32(self, v: i32) -> Result<(), Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_i64(self, v: i64) -> Result<(), Self::Error> {
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
    fn serialize_u64(self, v: u64) -> Result<(), Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_f32(self, v: f32) -> Result<(), Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
    fn serialize_f64(self, v: f64) -> Result<(), Self::Error> {
        write!(self.writer, "{}", v).map_err(|e| e.into())
    }
}


struct PlyBinaryBePrimitiveSerializer<'a, W: Write + 'a + ?Sized> {
    writer: &'a mut W,
}

impl<'a, W: Write + 'a + ?Sized> PlyBinaryBePrimitiveSerializer<'a, W> {
    fn new(w: &'a mut W) -> Self {
        Self {
            writer: w,
        }
    }
}

impl<'a, W: Write + 'a + ?Sized> PrimitiveSerializer for PlyBinaryBePrimitiveSerializer<'a, W> {
    type Error = PlyError;

    fn serialize_bool(self, v: bool) -> Result<(), Self::Error> {
        self.serialize_u8(if v { 1 } else { 0 })
    }
    fn serialize_i8(self, v: i8) -> Result<(), Self::Error> {
        println!("writing i8: {:x}", v);
        self.writer.write_i8(v)?;
        Ok(())
    }
    fn serialize_i16(self, v: i16) -> Result<(), Self::Error> {
        println!("writing i16: {:x}", v);
        self.writer.write_i16::<BigEndian>(v)?;
        Ok(())
    }
    fn serialize_i32(self, v: i32) -> Result<(), Self::Error> {
        println!("writing i32: {:x}", v);
        self.writer.write_i32::<BigEndian>(v)?;
        Ok(())
    }
    fn serialize_i64(self, v: i64) -> Result<(), Self::Error> {
        println!("writing i64: {:x}", v);
        self.writer.write_i64::<BigEndian>(v)?;
        Ok(())
    }
    fn serialize_u8(self, v: u8) -> Result<(), Self::Error> {
        println!("writing u8: {:x}", v);
        self.writer.write_u8(v)?;
        Ok(())
    }
    fn serialize_u16(self, v: u16) -> Result<(), Self::Error> {
        println!("writing u16: {:x}", v);
        self.writer.write_u16::<BigEndian>(v)?;
        Ok(())
    }
    fn serialize_u32(self, v: u32) -> Result<(), Self::Error> {
        println!("writing u32: {:x}", v);
        self.writer.write_u32::<BigEndian>(v)?;
        Ok(())
    }
    fn serialize_u64(self, v: u64) -> Result<(), Self::Error> {
        println!("writing u64: {:x}", v);
        self.writer.write_u64::<BigEndian>(v)?;
        Ok(())
    }
    fn serialize_f32(self, v: f32) -> Result<(), Self::Error> {
        use std::io::Cursor;

        let mut dummy = Cursor::new(Vec::new());
        dummy.write_f32::<BigEndian>(v)?;
        println!("writing f32: {} => {:02x?}", v, dummy.get_ref());

        self.writer.write_f32::<BigEndian>(v)?;
        Ok(())
    }
    fn serialize_f64(self, v: f64) -> Result<(), Self::Error> {
        use std::io::Cursor;

        let mut dummy = Cursor::new(Vec::new());
        dummy.write_f64::<BigEndian>(v)?;
        println!("writing f64: {} => {:02x?}", v, dummy.get_ref());

        self.writer.write_f64::<BigEndian>(v)?;
        Ok(())
    }
}



macro_rules! gen_binary_prop_serializer {
    ($ty_name:ident, $primitive_serializer:ident) => {
        struct $ty_name<'a, W: Write + 'a + ?Sized> {
            writer: &'a mut W,
        }

        impl<'a, W: Write + 'a + ?Sized> $ty_name<'a, W> {
            fn new(w: &'a mut W) -> Self {
                Self { writer: w }
            }

            fn primitive(&mut self) -> $primitive_serializer<W> {
                $primitive_serializer {
                    writer: &mut *self.writer,
                }
            }

            fn serialize_triple(&mut self, v: [&impl PrimitiveSerialize; 3]) -> Result<(), PlyError> {
                v[0].serialize(self.primitive())?;
                v[1].serialize(self.primitive())?;
                v[2].serialize(self.primitive())?;

                Ok(())
            }
        }

        impl<'a, W: Write + 'a + ?Sized> PropSerializer for $ty_name<'a, W> {
            type Error = PlyError;

            fn serialize_position<PosT>(&mut self, v: &PosT) -> Result<(), Self::Error>
            where
                PosT: Pos3Like,
                PosT::Scalar: PrimitiveSerialize
            {
                println!("yooo binary");
                self.serialize_triple([v.x(), v.y(), v.z()])
            }

            fn serialize_normal<NormalT>(&mut self, v: &NormalT) -> Result<(), Self::Error>
            where
                NormalT: Vec3Like,
                NormalT::Scalar: PrimitiveSerialize
            {
                self.serialize_triple([v.x(), v.y(), v.z()])
            }

            fn serialize_named(
                &mut self,
                _name: &str,
                _v: &impl PrimitiveSerialize,
            ) -> Result<(), Self::Error>
            {
                unimplemented!()
            }
        }
    }
}

gen_binary_prop_serializer!(PlyBinaryBePropSerializer, PlyBinaryBePrimitiveSerializer);




struct PlyAsciiPropSerializer<'a, W: Write + 'a + ?Sized> {
    writer: &'a mut W,
    first: bool,
}

impl<'a, W: Write + 'a + ?Sized> PlyAsciiPropSerializer<'a, W> {
    fn new(w: &'a mut W) -> Self {
        Self {
            writer: w,
            first: true,
        }
    }

    fn primitive(&mut self) -> PlyAsciiPrimitiveSerializer<W> {
        PlyAsciiPrimitiveSerializer {
            writer: &mut *self.writer,
        }
    }

    fn insert_sep(&mut self) -> Result<(), PlyError> {
        if self.first {
            self.first = false;
        } else {
            write!(self.writer, " ")?;
        }

        Ok(())
    }

    fn serialize_triple(&mut self, v: [&impl PrimitiveSerialize; 3]) -> Result<(), PlyError> {
        self.insert_sep()?;
        v[0].serialize(self.primitive())?;
        self.writer.write_all(b" ")?;
        v[1].serialize(self.primitive())?;
        self.writer.write_all(b" ")?;
        v[2].serialize(self.primitive())?;

        Ok(())
    }
}

impl<'a, W: Write + 'a + ?Sized> PropSerializer for PlyAsciiPropSerializer<'a, W> {
    type Error = PlyError;

    fn serialize_position<PosT>(&mut self, v: &PosT) -> Result<(), Self::Error>
    where
        PosT: Pos3Like,
        PosT::Scalar: PrimitiveSerialize
    {
        self.serialize_triple([v.x(), v.y(), v.z()])
    }

    fn serialize_normal<NormalT>(&mut self, v: &NormalT) -> Result<(), Self::Error>
    where
        NormalT: Vec3Like,
        NormalT::Scalar: PrimitiveSerialize
    {
        self.serialize_triple([v.x(), v.y(), v.z()])
    }

    fn serialize_named(
        &mut self,
        _name: &str,
        _v: &impl PrimitiveSerialize,
    ) -> Result<(), Self::Error>
    {
        unimplemented!()
    }
}
