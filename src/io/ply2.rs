use std::{
    collections::HashSet,
    fmt,
    io::{self, Write},
};

use byteorder::{WriteBytesExt, BigEndian, LittleEndian};

use crate::{
    TriMesh, Pos3Like, Vec3Like,
    handle::{DefaultIndex, FaceHandle, VertexHandle},
    map::{FaceMap, VertexMap},
    io::{
        IntoMeshWriter, PropSetSerialize, MeshWriter, PropSerialize, PropSerializer,
        PrimitiveType, PropLabel, PropSetSerializer, PropType, LabeledPropSet,
        NameLabel, PropLabeler,
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

/// This is helper macro to quickly generate an `PropertySet` instance.
///
/// The code generated by this macro already checks the PLY format and
/// stores a function specialized for this format. This means that the format
/// dispatch is only done once per property set and not everytime a property
/// is serialized. But it also means that a lot of code is generated. I really
/// need to check if this is worth it.
macro_rules! pack_prop_set {
    ($format:expr, $labels:expr, |$handle:ident| $val:expr) => {{
        macro_rules! prop_set_with {
            ($serializer:ident) => {
                PropertySet {
                    labels: $labels,
                    serialize: Box::new(move |w, is_first, $handle| {
                        $val.serialize($serializer::new(w, is_first))
                    }),
                }
            }
        }

        match $format {
            PlyFormat::Ascii => prop_set_with!(PlyAsciiPropSetSerializer),
            PlyFormat::BinaryBigEndian => prop_set_with!(PlyBinaryBePropSetSerializer),
            PlyFormat::BinaryLittleEndian => prop_set_with!(PlyBinaryLePropSetSerializer),
        }
    }}
}


impl<'a, MeshT> IntoMeshWriter<'a, MeshT> for Ply
where
    MeshT: TriMesh + 'a,
    MeshT::VertexProp: LabeledPropSet + PropSetSerialize,
{
    type Error = PlyError;
    type Writer = PlyWriter<'a, MeshT>;

    fn serialize(self, mesh: &'a MeshT) -> Result<Self::Writer, Self::Error> {
        // Populate our property lists with the properties stored inside the
        // mesh (that is: properties of face and vertex + the property
        // listing all vertices of a face).
        // TODO: We can probably use the `.add` methods here to avoid dupe code,
        //       but first, `Fn` need to implement `PropMap`
        let vertex_prop_set = pack_prop_set!(
            self.format,
            MeshT::VertexProp::labels(),
            |handle| mesh.vertex_prop(handle).unwrap() // TODO
        );

        let vertex_indices = {
            // Prepare stuff to move into the `PropertySet`
            let labeler = NameLabel("vertex_indices");

            pack_prop_set!(
                self.format,
                PropLabeler::<[DefaultIndex; 3]>::labels(&labeler),
                |handle| labeler.wrap(mesh.vertices_of_face(handle))
            )
        };

        Ok(PlyWriter {
            format: self.format,
            mesh,
            vertex_prop_sets: vec![vertex_prop_set],
            face_prop_sets: vec![vertex_indices], // TODO
            vertex_prop_names: HashSet::new(),
            face_prop_names: HashSet::new(),
        })
    }
}

pub struct PlyWriter<'a, MeshT: 'a> {
    format: PlyFormat,
    mesh: &'a MeshT,
    vertex_prop_sets: Vec<PropertySet<'a, VertexHandle>>,
    face_prop_sets: Vec<PropertySet<'a, FaceHandle>>,
    vertex_prop_names: HashSet<String>,
    face_prop_names: HashSet<String>,
}

struct PropertySet<'a, HandleT> {
    labels: Vec<PropLabel>,
    serialize: Box<'a + Fn(&mut io::Write, bool, HandleT) -> Result<(), PlyError>>,
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

    #[fail(display =
        "attempt to add {} property '{}' to PLY file, but a property with that name has already been \
        added",
        element,
        label,
    )]
    LabelAlreadyInUse {
        label: String,
        element: ElementKind,
    },

    #[fail(display = "something :(")]
    Something,
}

#[derive(Copy, Clone, Debug)]
pub enum ElementKind {
    Vertex,
    Face,
}

impl fmt::Display for ElementKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ElementKind::Vertex => "vertex",
            ElementKind::Face => "face"
        }.fmt(f)
    }
}

impl From<io::Error> for PlyError {
    fn from(_src: io::Error) -> Self {
        PlyError::Something
    }
}

impl<'a, MeshT> PlyWriter<'a, MeshT> {
    pub fn add_face_prop<MapT>(&mut self, map: &'a MapT) -> Result<&mut Self, PlyError>
    where
        MapT: FaceMap,
        MapT::Output: PropSetSerialize + LabeledPropSet,
    {
        let labels = MapT::Output::labels();
        self.check_new_prop_set(ElementKind::Face, &labels)?;

        {
            let new_names = labels.iter()
                .flat_map(label_names)
                .map(|s| s.to_owned());
            self.face_prop_names.extend(new_names);
        }

        self.face_prop_sets.push(pack_prop_set!(
            self.format,
            labels,
            |handle| map.get(handle).unwrap() // TODO
        ));

        Ok(self)
    }

    pub fn add_face_prop_with<MapT>(
        &mut self,
        map: &'a MapT,
        labeler: impl PropLabeler<&'a MapT::Output> + 'a,
    ) -> Result<&mut Self, PlyError>
    where
        MapT: FaceMap,
        MapT::Output: Sized,
    {
        let labels = labeler.labels();
        self.check_new_prop_set(ElementKind::Face, &labels)?;

        {
            let new_names = labels.iter()
                .flat_map(label_names)
                .map(|s| s.to_owned());
            self.face_prop_names.extend(new_names);
        }

        self.face_prop_sets.push(pack_prop_set!(
            self.format,
            labels,
            |handle| labeler.wrap(map.get(handle).unwrap()) // TODO
        ));

        Ok(self)
    }

    fn check_new_prop_set(
        &self,
        element: ElementKind,
        new_labels: &[PropLabel],
    ) -> Result<(), PlyError> {
        let names = match element {
            ElementKind::Vertex => &self.vertex_prop_names,
            ElementKind::Face => &self.face_prop_names,
        };

        for new_label in new_labels {
            for name in label_names(new_label) {
                if names.contains(name) {
                    return Err(PlyError::LabelAlreadyInUse {
                        label: name.into(),
                        element,
                    });
                }
            };
        }

        Ok(())
    }
}

fn label_names(label: &PropLabel) -> Vec<&str> {
    match label {
        PropLabel::Position { .. } => vec!["x", "y", "z"],
        PropLabel::Normal { .. } => vec!["nx", "ny", "nz"],
        PropLabel::Named { name, .. } => vec![name.as_str()],
    }
}

impl<'a, MeshT> MeshWriter<'a> for PlyWriter<'a, MeshT>
where
    MeshT: TriMesh + 'a,
    MeshT::VertexProp: PropSetSerialize,
{
    type Error = PlyError;

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


        // Define `vertex` element with all properties
        writeln!(w, "element vertex {}", self.mesh.num_vertices())?;
        for prop_set in &self.vertex_prop_sets {
            for label in &prop_set.labels {
                write_header_property(&mut w, label)?;
            }
        }

        // Define `face` element with all properties
        writeln!(w, "element face {}", self.mesh.num_faces())?;

        for prop_set in &self.face_prop_sets {
            for label in &prop_set.labels {
                write_header_property(&mut w, label)?;
            }
        }

        w.write_all(b"end_header\n")?;


        // ===================================================================
        // Write body
        // ===================================================================
        let finish_block = match self.format {
            PlyFormat::Ascii => PlyAsciiPropSetSerializer::finish_block,
            PlyFormat::BinaryBigEndian => PlyBinaryBePropSetSerializer::finish_block,
            PlyFormat::BinaryLittleEndian => PlyBinaryLePropSetSerializer::finish_block,
        };

        for vertex_handle in self.mesh.vertices() {
            for (i, prop) in self.vertex_prop_sets.iter().enumerate() {
                (prop.serialize)(&mut w, i == 0, vertex_handle)?;
            }

            finish_block(&mut w)?;
        }

        for face_handle in self.mesh.faces() {
            for (i, prop) in self.face_prop_sets.iter().enumerate() {
                (prop.serialize)(&mut w, i == 0, face_handle)?;
            }

            finish_block(&mut w)?;
        }

        Ok(())
    }
}

/// Writes the header entry for one property to the given writer.
fn write_header_property(w: &mut impl Write, label: &PropLabel) -> Result<(), PlyError> {
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
            // Since we don't know the length, we have to use the largest
            // integer type, `uint`, to specify the length.
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

    Ok(())
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

    fn serialize_fixed_len_seq<T: PropSerialize>(self, seq: &[T]) -> Result<(), Self::Error> {
        write!(self.writer, "{}", seq.len())?;

        for v in seq {
            write!(self.writer, " ")?;
            v.serialize(Self::new(self.writer))?;
        }

        Ok(())
    }

    fn serialize_variable_len_seq<T: PropSerialize>(self, v: &[T]) -> Result<(), Self::Error> {
        self.serialize_fixed_len_seq(v)
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

            fn serialize_fixed_len_seq<T: PropSerialize>(self, seq: &[T]) -> Result<(), Self::Error> {
                let len = seq.len() as u64;
                if len <= u8::max_value() as u64 {
                    self.writer.write_u8(len as u8)?;
                } else if len <= u16::max_value() as u64 {
                    self.writer.write_u16::<$endianess>(len as u16)?;
                } else if len <= u32::max_value() as u64 {
                    self.writer.write_u32::<$endianess>(len as u32)?;
                } else {
                    return Err(PlyError::FixedLenListTooLong(len));
                };

                for v in seq {
                    v.serialize(Self::new(self.writer))?;
                }

                Ok(())
            }

            fn serialize_variable_len_seq<T: PropSerialize>(self, seq: &[T]) -> Result<(), Self::Error> {
                self.writer.write_u32::<$endianess>(seq.len() as u32)?;

                for v in seq {
                    v.serialize(Self::new(self.writer))?;
                }

                Ok(())
            }
        }
    }
}

gen_binary_primitive_serializer!(PlyBinaryBePropSerializer, BigEndian);
gen_binary_primitive_serializer!(PlyBinaryLePropSerializer, LittleEndian);




// ===========================================================================
// ===== PropSetSerializer for PLY
// ===========================================================================

/// PropSetSerializer for the ASCII version of PLY. Stores a writer and a flag
/// to know where to put ' ' seperators.
struct PlyAsciiPropSetSerializer<'a, W: Write + 'a + ?Sized> {
    writer: &'a mut W,
    first: bool,
}

impl<'a, W: Write + 'a + ?Sized> PlyAsciiPropSetSerializer<'a, W> {
    fn new(w: &'a mut W, first: bool) -> Self {
        Self {
            writer: w,
            first,
        }
    }

    fn finish_block(w: &mut W) -> Result<(), PlyError> {
        w.write_all(b"\n").map_err(|e| e.into())
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

impl<'a, W: Write + 'a + ?Sized> PropSetSerializer for PlyAsciiPropSetSerializer<'a, W> {
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
// The binary PropSetSerializer differ from the ASCII one merely by the
// PropSerializer used and in that fact that they don't output ' '
// seperators.
macro_rules! gen_binary_prop_serializer {
    ($name:ident, $primitive_serializer:ident) => {
        // It's just a wrapper for a `Write` reference
        struct $name<'a, W: Write + 'a + ?Sized> {
            writer: &'a mut W,
        }

        impl<'a, W: Write + 'a + ?Sized> $name<'a, W> {
            fn new(w: &'a mut W, _: bool) -> Self {
                Self { writer: w }
            }

            fn finish_block(_: &mut W) -> Result<(), PlyError> {
                // In binary format, the block end implictly after the correct
                // number of bytes
                Ok(())
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

gen_binary_prop_serializer!(PlyBinaryBePropSetSerializer, PlyBinaryBePropSerializer);
gen_binary_prop_serializer!(PlyBinaryLePropSetSerializer, PlyBinaryLePropSerializer);
