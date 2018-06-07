use std::{
    io::{self, Write},
};

use byteorder::{WriteBytesExt, BigEndian, LittleEndian};

use crate::{
    TriMesh, Pos3Like, Vec3Like,
    handle::{DefaultIndex, DefaultIndexExt, FaceHandle, Handle, VertexHandle},
    map::{PropMap, FaceMap, VertexMap},
    io::{
        IntoMeshWriter, LabeledPropSet, MeshWriter, PropSerialize, PropSerializer,
        PrimitiveType, PropLabel, PropSetSerializer, PropType,
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
        // Add the properties stored inside the mesh to our property list. We
        // already dispatch over the format here. This means that we don't have
        // to branch over the format later.
        macro_rules! prop_set_with {
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
            PlyFormat::Ascii => prop_set_with!(PlyAsciiLabeledPropSetSerializer),
            PlyFormat::BinaryBigEndian => prop_set_with!(PlyBinaryBeLabeledPropSetSerializer),
            PlyFormat::BinaryLittleEndian => prop_set_with!(PlyBinaryLeLabeledPropSetSerializer),
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
    #[fail(display =
        "unsupported index type: attempt to use a {} byte integer as vertex index (PLY only \
         supports 1, 2 and 4 byte integers)",
         num_bytes,
    )]
    IndexTypeNotSupported {
        num_bytes: u8,
    },

    #[fail(display = "type '{}' is not supported by PLY", _0)]
    PrimitiveTypeNotSupported(PrimitiveType),

    #[fail(display =
        "attempt to serialize a list-like property with a fixed length of {} (maximum is 2^32)",
        _0,
    )]
    FixedLenListTooLong(u64),

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
        // Write header (this part is always ASCII)
        // ===================================================================

        // Magic signature
        w.write_all(b"ply\n")?;

        // The line defining the format of the file
        let format_line = match self.format {
            PlyFormat::Ascii => b"format ascii 1.0\n" as &[_],
            PlyFormat::BinaryBigEndian => b"format binary_big_endian 1.0\n",
            PlyFormat::BinaryLittleEndian => b"format binary_little_endian 1.0\n",
        };
        w.write_all(format_line)?;


        // TODO: would be nice to write some meta data, such as date, into the
        // file

        // TODO: it would be nice to let the user add comments to the file if
        // they want to.


        // Determine the correct PLY integer for the integer type used to store
        // vertex indices. (Currently, `DefaultIndex` is a fixed type, `u32`,
        // but the idea it to make this library generic over this type.
        // Eventually...)
        let idx_type = match DefaultIndex::NUM_BYTES {
            1 => "uchar",
            2 => "ushort",
            4 => "uint",
            num_bytes => return Err(PlyError::IndexTypeNotSupported { num_bytes }),
        };

        // Define `vertex` element with all properties
        writeln!(w, "element vertex {}", self.mesh.num_vertices())?;
        for prop_set in &self.vertex_props {
            for label in prop_set.labels {
                match label {
                    // Positions are stored as properties 'x', 'y' and 'z' by
                    // convention.
                    PropLabel::Position { scalar_ty } => {
                        let ty_name = primitive_ply_type_name(scalar_ty)?;

                        writeln!(w, "property {} x", ty_name)?;
                        writeln!(w, "property {} y", ty_name)?;
                        writeln!(w, "property {} z", ty_name)?;
                    }

                    // Normals are stored as properties 'nx', 'ny' and 'nz' by
                    // convention.
                    PropLabel::Normal { scalar_ty } => {
                        let ty_name = primitive_ply_type_name(scalar_ty)?;

                        writeln!(w, "property {} nx", ty_name)?;
                        writeln!(w, "property {} ny", ty_name)?;
                        writeln!(w, "property {} nz", ty_name)?;
                    }

                    PropLabel::Named { name, ty: PropType::Single(ty) } => {
                        let ty_name = primitive_ply_type_name(ty)?;
                        writeln!(w, "property {} {}", ty_name, name)?;
                    }

                    PropLabel::Named { name, ty: PropType::VariableLen(ty) } => {
                        // Since we don't know the length before, we
                        // have to use `uint` to specify the length.
                        let ty_name = primitive_ply_type_name(ty)?;
                        writeln!(w, "property list uint {} {}", ty_name, name)?;
                    }

                    PropLabel::Named { name, ty: PropType::FixedLen { len, ty } } => {
                        let len = *len;

                        // We know the length of all properties
                        // beforehand, so we can optimize and chose a
                        // small type to specify the length.
                        let len_type = if len <= u8::max_value() as u64 {
                            "uchar"
                        } else if len <= u16::max_value() as u64 {
                            "ushort"
                        } else if len <= u32::max_value() as u64 {
                            "uint"
                        } else {
                            return Err(PlyError::FixedLenListTooLong(len));
                        };

                        let ty_name = primitive_ply_type_name(ty)?;
                        writeln!(w, "property list {} {} {}", len_type, ty_name, name)?;
                    }
                }
            }
        }


        // Define `face` element with all properties
        writeln!(w, "element face {}", self.mesh.num_faces())?;
        writeln!(w, "property list uchar {} vertex_indices", idx_type)?;

        // TODO: user defined properties


        w.write_all(b"end_header\n")?;


        // ===================================================================
        // Write body
        // ===================================================================
        match self.format {
            PlyFormat::Ascii => {
                for vertex_handle in self.mesh.vertices() {
                    for prop in &self.vertex_props {
                        (prop.serialize)(&mut w, vertex_handle)?;
                    }
                    writeln!(w, "")?;
                }

                for face in self.mesh.faces() {
                    // TODO: other face attributes
                    w.write_all(b"3")?;
                    for &v in &self.mesh.vertices_of_face(face) {
                        w.write_all(b" ")?;
                        v.idx().serialize(PlyAsciiPropSerializer::new(&mut w))?;
                    }
                    w.write_all(b"\n")?;
                }
            }
            PlyFormat::BinaryBigEndian => {
                for vertex_handle in self.mesh.vertices() {
                    for prop in &self.vertex_props {
                        (prop.serialize)(&mut w, vertex_handle)?;
                    }
                }

                for face in self.mesh.faces() {
                    // TODO: other face attributes
                    PlyBinaryBePropSerializer::new(&mut w).serialize_u8(3)?;
                    for &v in &self.mesh.vertices_of_face(face) {
                        v.idx().serialize(PlyBinaryBePropSerializer::new(&mut w))?;
                    }
                }
            }
            _ => unimplemented!(),
        }

        Ok(())
    }
}


/// Returns the name of the PLY type corresponding to the given type.
fn primitive_ply_type_name(ty: &PrimitiveType) -> Result<&'static str, PlyError> {
    match ty {
        PrimitiveType::Bool    => Ok("uchar"),  // we store booleans as `u8`
        PrimitiveType::Uint8   => Ok("uchar"),
        PrimitiveType::Uint16  => Ok("ushort"),
        PrimitiveType::Uint32  => Ok("uint"),
        PrimitiveType::Int8    => Ok("char"),
        PrimitiveType::Int16   => Ok("short"),
        PrimitiveType::Int32   => Ok("int"),
        PrimitiveType::Float32 => Ok("float"),
        PrimitiveType::Float64 => Ok("double"),
        t => Err(PlyError::PrimitiveTypeNotSupported(*t)),
    }
}



// ===========================================================================
// ===== PropSerializer for PLY
// ===========================================================================

/// Serializes primitives as PLY ASCII (simply using the `Display` impl).
struct PlyAsciiPropSerializer<'a, W: Write + 'a + ?Sized> {
    writer: &'a mut W,
}

impl<'a, W: Write + 'a + ?Sized> PlyAsciiPropSerializer<'a, W> {
    fn new(w: &'a mut W) -> Self {
        Self {
            writer: w,
        }
    }
}

impl<'a, W: Write + 'a + ?Sized> PropSerializer for PlyAsciiPropSerializer<'a, W> {
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


// The two binary primitive serializer are very similar. The macro avoids some
// duplicate code.
macro_rules! gen_binary_primitive_serializer {
    ($name:ident, $endianess:ident) => {
        struct $name<'a, W: Write + 'a + ?Sized> {
            writer: &'a mut W,
        }

        impl<'a, W: Write + 'a + ?Sized> $name<'a, W> {
            fn new(w: &'a mut W) -> Self {
                Self {
                    writer: w,
                }
            }
        }

        impl<'a, W: Write + 'a + ?Sized> PropSerializer for $name<'a, W> {
            type Error = PlyError;

            fn serialize_bool(self, v: bool) -> Result<(), Self::Error> {
                // We convert the bool to `u8` to serialize it, because PLY
                // doesn't support booleans.
                self.serialize_u8(if v { 1 } else { 0 })
            }
            fn serialize_i8(self, v: i8) -> Result<(), Self::Error> {
                self.writer.write_i8(v).map_err(|e| e.into())
            }
            fn serialize_i16(self, v: i16) -> Result<(), Self::Error> {
                self.writer.write_i16::<$endianess>(v).map_err(|e| e.into())
            }
            fn serialize_i32(self, v: i32) -> Result<(), Self::Error> {
                self.writer.write_i32::<$endianess>(v).map_err(|e| e.into())
            }
            fn serialize_i64(self, v: i64) -> Result<(), Self::Error> {
                self.writer.write_i64::<$endianess>(v).map_err(|e| e.into())
            }
            fn serialize_u8(self, v: u8) -> Result<(), Self::Error> {
                self.writer.write_u8(v).map_err(|e| e.into())
            }
            fn serialize_u16(self, v: u16) -> Result<(), Self::Error> {
                self.writer.write_u16::<$endianess>(v).map_err(|e| e.into())
            }
            fn serialize_u32(self, v: u32) -> Result<(), Self::Error> {
                self.writer.write_u32::<$endianess>(v).map_err(|e| e.into())
            }
            fn serialize_u64(self, v: u64) -> Result<(), Self::Error> {
                self.writer.write_u64::<$endianess>(v).map_err(|e| e.into())
            }
            fn serialize_f32(self, v: f32) -> Result<(), Self::Error> {
                self.writer.write_f32::<$endianess>(v).map_err(|e| e.into())
            }
            fn serialize_f64(self, v: f64) -> Result<(), Self::Error> {
                self.writer.write_f64::<$endianess>(v).map_err(|e| e.into())
            }
        }
    }
}

gen_binary_primitive_serializer!(PlyBinaryBePropSerializer, BigEndian);
gen_binary_primitive_serializer!(PlyBinaryLePropSerializer, LittleEndian);




// ===========================================================================
// ===== PropSetSerializer for PLY
// ===========================================================================

/// LabeledPropSetSerializer for the ASCII version of PLY. Stores a writer and a flag
/// to know where to put ' ' seperators.
struct PlyAsciiLabeledPropSetSerializer<'a, W: Write + 'a + ?Sized> {
    writer: &'a mut W,
    first: bool,
}

impl<'a, W: Write + 'a + ?Sized> PlyAsciiLabeledPropSetSerializer<'a, W> {
    fn new(w: &'a mut W) -> Self {
        Self {
            writer: w,
            first: true,
        }
    }

    /// Returns the associated primitive serializer.
    fn primitive(&mut self) -> PlyAsciiPropSerializer<W> {
        PlyAsciiPropSerializer {
            writer: &mut *self.writer,
        }
    }

    /// Writes a ' ' seperator when necessary. The first time this method is
    /// called on an instance, it does nothing. Afterwards, it will always
    /// write ' '.
    fn insert_sep(&mut self) -> Result<(), PlyError> {
        if self.first {
            self.first = false;
        } else {
            write!(self.writer, " ")?;
        }

        Ok(())
    }

    /// Helper function to serialize three values of the same type. Used for
    /// `position`, `normal`, ...
    fn serialize_triple(&mut self, v: [&impl PropSerialize; 3]) -> Result<(), PlyError> {
        self.insert_sep()?;
        v[0].serialize(self.primitive())?;
        self.writer.write_all(b" ")?;
        v[1].serialize(self.primitive())?;
        self.writer.write_all(b" ")?;
        v[2].serialize(self.primitive())?;

        Ok(())
    }
}

impl<'a, W: Write + 'a + ?Sized> PropSetSerializer for PlyAsciiLabeledPropSetSerializer<'a, W> {
    type Error = PlyError;

    fn serialize_position<PosT>(&mut self, v: &PosT) -> Result<(), Self::Error>
    where
        PosT: Pos3Like,
        PosT::Scalar: PropSerialize
    {
        self.serialize_triple([v.x(), v.y(), v.z()])
    }

    fn serialize_normal<NormalT>(&mut self, v: &NormalT) -> Result<(), Self::Error>
    where
        NormalT: Vec3Like,
        NormalT::Scalar: PropSerialize
    {
        self.serialize_triple([v.x(), v.y(), v.z()])
    }

    fn serialize_named(
        &mut self,
        _name: &str,
        v: &impl PropSerialize,
    ) -> Result<(), Self::Error>
    {
        self.insert_sep()?;
        v.serialize(self.primitive())
    }
}


// The little endian and big endian binary serializer are very similar. This
// macro avoids some duplicate code.
//
// The binary LabeledPropSetSerializer differ from the ASCII one merely by the
// PropSerializer used and in that fact that they don't output ' '
// seperators.
macro_rules! gen_binary_prop_serializer {
    ($name:ident, $primitive_serializer:ident) => {
        // It's just a wrapper for a `Write` reference
        struct $name<'a, W: Write + 'a + ?Sized> {
            writer: &'a mut W,
        }

        impl<'a, W: Write + 'a + ?Sized> $name<'a, W> {
            fn new(w: &'a mut W) -> Self {
                Self { writer: w }
            }

            /// Returns the associated primitive serializer.
            fn primitive(&mut self) -> $primitive_serializer<W> {
                $primitive_serializer {
                    writer: &mut *self.writer,
                }
            }

            /// Helper function to serialize three values of the same type. Used
            /// for `position`, `normal`, ...
            fn serialize_triple(&mut self, v: [&impl PropSerialize; 3]) -> Result<(), PlyError> {
                v[0].serialize(self.primitive())?;
                v[1].serialize(self.primitive())?;
                v[2].serialize(self.primitive())?;

                Ok(())
            }
        }

        impl<'a, W: Write + 'a + ?Sized> PropSetSerializer for $name<'a, W> {
            type Error = PlyError;

            fn serialize_position<PosT>(&mut self, v: &PosT) -> Result<(), Self::Error>
            where
                PosT: Pos3Like,
                PosT::Scalar: PropSerialize,
            {
                self.serialize_triple([v.x(), v.y(), v.z()])
            }

            fn serialize_normal<NormalT>(&mut self, v: &NormalT) -> Result<(), Self::Error>
            where
                NormalT: Vec3Like,
                NormalT::Scalar: PropSerialize,
            {
                self.serialize_triple([v.x(), v.y(), v.z()])
            }

            fn serialize_named(
                &mut self,
                _name: &str,
                v: &impl PropSerialize,
            ) -> Result<(), Self::Error>
            {
                // We don't need the name here. We just need to dump the value.
                v.serialize(self.primitive())
            }
        }
    }
}

gen_binary_prop_serializer!(PlyBinaryBeLabeledPropSetSerializer, PlyBinaryBePropSerializer);
gen_binary_prop_serializer!(PlyBinaryLeLabeledPropSetSerializer, PlyBinaryLePropSerializer);
