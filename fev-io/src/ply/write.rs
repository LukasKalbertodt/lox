use std::{
    // collections::HashSet,
    io::Write,
};

use fev_core::{
    ExplicitVertex, ExplicitFace,
    prop::{LabeledPropList, PropLabel},
};

use crate::{
    MeshWriter,
    ser::{DataType, PrimitiveType, PropListSerialize},
};
use super::{PlyError, PlyFormat};



pub struct PlyWriter<'a, MeshT: 'a> {
    pub format: PlyFormat,
    pub mesh: &'a MeshT,
    // vertex_prop_sets: Vec<PropertySet<'a, VertexHandle>>,
    // face_prop_sets: Vec<PropertySet<'a, FaceHandle>>,
    // vertex_prop_names: HashSet<String>,
    // face_prop_names: HashSet<String>,
}



impl<'a, MeshT> MeshWriter for PlyWriter<'a, MeshT>
where
    MeshT: ExplicitVertex + ExplicitFace,
    MeshT::VertexProp: LabeledPropList + PropListSerialize,
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
        for i in 0..MeshT::VertexProp::num_props() {
            let label = MeshT::VertexProp::label_of(i);
            let ty = MeshT::VertexProp::data_type_of(i);
            write_header_property(&mut w, &label, ty)?;
        }
        // for prop_set in &self.vertex_prop_sets {
        //     for label in &prop_set.labels {
        //         write_header_property(&mut w, label)?;
        //     }
        // }

        // Define `face` element with all properties
        writeln!(w, "element face {}", self.mesh.num_faces())?;
        // for prop_set in &self.face_prop_sets {
        //     for label in &prop_set.labels {
        //         write_header_property(&mut w, label)?;
        //     }
        // }

        w.write_all(b"end_header\n")?;


        // ===================================================================
        // Write body
        // ===================================================================

        // let finish_block = match self.format {
        //     PlyFormat::Ascii => PlyAsciiPropSetSerializer::finish_block,
        //     PlyFormat::BinaryBigEndian => PlyBinaryBePropSetSerializer::finish_block,
        //     PlyFormat::BinaryLittleEndian => PlyBinaryLePropSetSerializer::finish_block,
        // };

        // for vertex_handle in self.mesh.vertices() {
        //     for (i, prop) in self.vertex_prop_sets.iter().enumerate() {
        //         (prop.serialize)(&mut w, i == 0, vertex_handle)?;
        //     }

        //     finish_block(&mut w)?;
        // }

        // for face_handle in self.mesh.faces() {
        //     for (i, prop) in self.face_prop_sets.iter().enumerate() {
        //         (prop.serialize)(&mut w, i == 0, face_handle)?;
        //     }

        //     finish_block(&mut w)?;
        // }

        Ok(())
    }
}

/// Writes the header entry for one property to the given writer.
fn write_header_property(
    w: &mut impl Write,
    label: &PropLabel,
    data_type: DataType,
) -> Result<(), PlyError> {
    match (label, data_type) {
        // Positions are stored as properties 'x', 'y' and 'z' by
        // convention.
        (PropLabel::Position, ty) => {
            // TODO: make sure it's the correct data type
            let ty_name = primitive_ply_type_name(ty.primitive_type())?;

            writeln!(w, "property {} x", ty_name)?;
            writeln!(w, "property {} y", ty_name)?;
            writeln!(w, "property {} z", ty_name)?;
        }

        // Normals are stored as properties 'nx', 'ny' and 'nz' by
        // convention.
        (PropLabel::Normal, ty) => {
            // TODO: make sure it's the correct data type
            let ty_name = primitive_ply_type_name(ty.primitive_type())?;

            writeln!(w, "property {} nx", ty_name)?;
            writeln!(w, "property {} ny", ty_name)?;
            writeln!(w, "property {} nz", ty_name)?;
        }

        (PropLabel::Named(name), DataType::Single(ty)) => {
            let ty_name = primitive_ply_type_name(ty)?;
            writeln!(w, "property {} {}", ty_name, name)?;
        }

        (PropLabel::Named(name), DataType::VariableLen(ty)) => {
            // Since we don't know the length, we have to use the largest
            // integer type, `uint`, to specify the length.
            let ty_name = primitive_ply_type_name(ty)?;
            writeln!(w, "property list uint {} {}", ty_name, name)?;
        }

        (PropLabel::Named(_name), DataType::FixedLen { .. }) => {
            // // We know the length of all properties
            // // beforehand, so we can optimize and chose a
            // // small type to specify the length.
            // let len_type = if len <= u8::max_value() as u64 {
            //     "uchar"
            // } else if len <= u16::max_value() as u64 {
            //     "ushort"
            // } else if len <= u32::max_value() as u64 {
            //     "uint"
            // } else {
            //     return Err(PlyError::FixedLenListTooLong(len));
            // };

            // let ty_name = primitive_ply_type_name(ty)?;
            // writeln!(w, "property list {} {} {}", len_type, ty_name, name)?;
            unimplemented!()
        }
    }

    Ok(())
}

/// Returns the name of the PLY type corresponding to the given type.
fn primitive_ply_type_name(ty: PrimitiveType) -> Result<&'static str, PlyError> {
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
        t => Err(PlyError::PrimitiveTypeNotSupported(t)),
    }
}

// impl<'a, MeshT> PlyWriter<'a, MeshT> {
//     pub fn add_face_prop<MapT>(&mut self, map: &'a MapT) -> Result<&mut Self, PlyError>
//     where
//         MapT: FaceMap,
//         MapT::Output: PropSetSerialize + LabeledPropSet + Sized,
//     {
//         self.add_face_prop_with(map, StandardLabel::new())
//     }

//     pub fn add_face_prop_with<MapT>(
//         &mut self,
//         map: &'a MapT,
//         labeler: impl PropLabeler<&'a MapT::Output> + 'a,
//     ) -> Result<&mut Self, PlyError>
//     where
//         MapT: FaceMap,
//         MapT::Output: Sized,
//     {
//         let labels = labeler.labels();
//         self.check_new_prop_set(ElementKind::Face, &labels)?;

//         {
//             let new_names = labels.iter()
//                 .flat_map(label_names)
//                 .map(|s| s.to_owned());
//             self.face_prop_names.extend(new_names);
//         }

//         self.face_prop_sets.push(pack_prop_set!(
//             self.format,
//             labels,
//             |handle| labeler.wrap(map.get(handle).unwrap()) // TODO
//         ));

//         Ok(self)
//     }

//     fn check_new_prop_set(
//         &self,
//         element: ElementKind,
//         new_labels: &[PropLabel],
//     ) -> Result<(), PlyError> {
//         let names = match element {
//             ElementKind::Vertex => &self.vertex_prop_names,
//             ElementKind::Face => &self.face_prop_names,
//         };

//         for new_label in new_labels {
//             for name in label_names(new_label) {
//                 if names.contains(name) {
//                     return Err(PlyError::LabelAlreadyInUse {
//                         label: name.into(),
//                         element,
//                     });
//                 }
//             };
//         }

//         Ok(())
//     }
// }





// // ===========================================================================
// // ===== PropSerializer for PLY
// // ===========================================================================

// /// Serializes primitives as PLY ASCII (simply using the `Display` impl).
// struct PlyAsciiPropSerializer<'a, W: Write + 'a + ?Sized> {
//     writer: &'a mut W,
// }

// impl<'a, W: Write + 'a + ?Sized> PlyAsciiPropSerializer<'a, W> {
//     fn new(w: &'a mut W) -> Self {
//         Self {
//             writer: w,
//         }
//     }
// }

// impl<'a, W: Write + 'a + ?Sized> PropSerializer for PlyAsciiPropSerializer<'a, W> {
//     type Error = PlyError;

//     fn serialize_bool(self, v: bool) -> Result<(), Self::Error> {
//         write!(self.writer, "{}", v).map_err(|e| e.into())
//     }
//     fn serialize_i8(self, v: i8) -> Result<(), Self::Error> {
//         write!(self.writer, "{}", v).map_err(|e| e.into())
//     }
//     fn serialize_i16(self, v: i16) -> Result<(), Self::Error> {
//         write!(self.writer, "{}", v).map_err(|e| e.into())
//     }
//     fn serialize_i32(self, v: i32) -> Result<(), Self::Error> {
//         write!(self.writer, "{}", v).map_err(|e| e.into())
//     }
//     fn serialize_i64(self, v: i64) -> Result<(), Self::Error> {
//         write!(self.writer, "{}", v).map_err(|e| e.into())
//     }
//     fn serialize_u8(self, v: u8) -> Result<(), Self::Error> {
//         write!(self.writer, "{}", v).map_err(|e| e.into())
//     }
//     fn serialize_u16(self, v: u16) -> Result<(), Self::Error> {
//         write!(self.writer, "{}", v).map_err(|e| e.into())
//     }
//     fn serialize_u32(self, v: u32) -> Result<(), Self::Error> {
//         write!(self.writer, "{}", v).map_err(|e| e.into())
//     }
//     fn serialize_u64(self, v: u64) -> Result<(), Self::Error> {
//         write!(self.writer, "{}", v).map_err(|e| e.into())
//     }
//     fn serialize_f32(self, v: f32) -> Result<(), Self::Error> {
//         write!(self.writer, "{}", v).map_err(|e| e.into())
//     }
//     fn serialize_f64(self, v: f64) -> Result<(), Self::Error> {
//         write!(self.writer, "{}", v).map_err(|e| e.into())
//     }

//     fn serialize_fixed_len_seq<T: PropSerialize>(self, seq: &[T]) -> Result<(), Self::Error> {
//         write!(self.writer, "{}", seq.len())?;

//         for v in seq {
//             write!(self.writer, " ")?;
//             v.serialize(Self::new(self.writer))?;
//         }

//         Ok(())
//     }

//     fn serialize_variable_len_seq<T: PropSerialize>(self, v: &[T]) -> Result<(), Self::Error> {
//         self.serialize_fixed_len_seq(v)
//     }

// }


// // The two binary primitive serializer are very similar. The macro avoids some
// // duplicate code.
// macro_rules! gen_binary_primitive_serializer {
//     ($name:ident, $endianess:ident) => {
//         struct $name<'a, W: Write + 'a + ?Sized> {
//             writer: &'a mut W,
//         }

//         impl<'a, W: Write + 'a + ?Sized> $name<'a, W> {
//             fn new(w: &'a mut W) -> Self {
//                 Self {
//                     writer: w,
//                 }
//             }
//         }

//         impl<'a, W: Write + 'a + ?Sized> PropSerializer for $name<'a, W> {
//             type Error = PlyError;

//             fn serialize_bool(self, v: bool) -> Result<(), Self::Error> {
//                 // We convert the bool to `u8` to serialize it, because PLY
//                 // doesn't support booleans.
//                 self.serialize_u8(if v { 1 } else { 0 })
//             }
//             fn serialize_i8(self, v: i8) -> Result<(), Self::Error> {
//                 self.writer.write_i8(v).map_err(|e| e.into())
//             }
//             fn serialize_i16(self, v: i16) -> Result<(), Self::Error> {
//                 self.writer.write_i16::<$endianess>(v).map_err(|e| e.into())
//             }
//             fn serialize_i32(self, v: i32) -> Result<(), Self::Error> {
//                 self.writer.write_i32::<$endianess>(v).map_err(|e| e.into())
//             }
//             fn serialize_i64(self, v: i64) -> Result<(), Self::Error> {
//                 self.writer.write_i64::<$endianess>(v).map_err(|e| e.into())
//             }
//             fn serialize_u8(self, v: u8) -> Result<(), Self::Error> {
//                 self.writer.write_u8(v).map_err(|e| e.into())
//             }
//             fn serialize_u16(self, v: u16) -> Result<(), Self::Error> {
//                 self.writer.write_u16::<$endianess>(v).map_err(|e| e.into())
//             }
//             fn serialize_u32(self, v: u32) -> Result<(), Self::Error> {
//                 self.writer.write_u32::<$endianess>(v).map_err(|e| e.into())
//             }
//             fn serialize_u64(self, v: u64) -> Result<(), Self::Error> {
//                 self.writer.write_u64::<$endianess>(v).map_err(|e| e.into())
//             }
//             fn serialize_f32(self, v: f32) -> Result<(), Self::Error> {
//                 self.writer.write_f32::<$endianess>(v).map_err(|e| e.into())
//             }
//             fn serialize_f64(self, v: f64) -> Result<(), Self::Error> {
//                 self.writer.write_f64::<$endianess>(v).map_err(|e| e.into())
//             }

//             fn serialize_fixed_len_seq<T: PropSerialize>(self, seq: &[T]) -> Result<(), Self::Error> {
//                 let len = seq.len() as u64;
//                 if len <= u8::max_value() as u64 {
//                     self.writer.write_u8(len as u8)?;
//                 } else if len <= u16::max_value() as u64 {
//                     self.writer.write_u16::<$endianess>(len as u16)?;
//                 } else if len <= u32::max_value() as u64 {
//                     self.writer.write_u32::<$endianess>(len as u32)?;
//                 } else {
//                     return Err(PlyError::FixedLenListTooLong(len));
//                 };

//                 for v in seq {
//                     v.serialize(Self::new(self.writer))?;
//                 }

//                 Ok(())
//             }

//             fn serialize_variable_len_seq<T: PropSerialize>(self, seq: &[T]) -> Result<(), Self::Error> {
//                 self.writer.write_u32::<$endianess>(seq.len() as u32)?;

//                 for v in seq {
//                     v.serialize(Self::new(self.writer))?;
//                 }

//                 Ok(())
//             }
//         }
//     }
// }

// gen_binary_primitive_serializer!(PlyBinaryBePropSerializer, BigEndian);
// gen_binary_primitive_serializer!(PlyBinaryLePropSerializer, LittleEndian);




// // ===========================================================================
// // ===== PropSetSerializer for PLY
// // ===========================================================================

// /// PropSetSerializer for the ASCII version of PLY. Stores a writer and a flag
// /// to know where to put ' ' seperators.
// struct PlyAsciiPropSetSerializer<'a, W: Write + 'a + ?Sized> {
//     writer: &'a mut W,
//     first: bool,
// }

// impl<'a, W: Write + 'a + ?Sized> PlyAsciiPropSetSerializer<'a, W> {
//     fn new(w: &'a mut W, first: bool) -> Self {
//         Self {
//             writer: w,
//             first,
//         }
//     }

//     fn finish_block(w: &mut W) -> Result<(), PlyError> {
//         w.write_all(b"\n").map_err(|e| e.into())
//     }


//     /// Returns the associated primitive serializer.
//     fn primitive(&mut self) -> PlyAsciiPropSerializer<W> {
//         PlyAsciiPropSerializer {
//             writer: &mut *self.writer,
//         }
//     }

//     /// Writes a ' ' seperator when necessary. The first time this method is
//     /// called on an instance, it does nothing. Afterwards, it will always
//     /// write ' '.
//     fn insert_sep(&mut self) -> Result<(), PlyError> {
//         if self.first {
//             self.first = false;
//         } else {
//             write!(self.writer, " ")?;
//         }

//         Ok(())
//     }

//     /// Helper function to serialize three values of the same type. Used for
//     /// `position`, `normal`, ...
//     fn serialize_triple(&mut self, v: [&impl PropSerialize; 3]) -> Result<(), PlyError> {
//         self.insert_sep()?;
//         v[0].serialize(self.primitive())?;
//         self.writer.write_all(b" ")?;
//         v[1].serialize(self.primitive())?;
//         self.writer.write_all(b" ")?;
//         v[2].serialize(self.primitive())?;

//         Ok(())
//     }
// }

// impl<'a, W: Write + 'a + ?Sized> PropSetSerializer for PlyAsciiPropSetSerializer<'a, W> {
//     type Error = PlyError;

//     fn serialize_position<PosT>(&mut self, v: &PosT) -> Result<(), Self::Error>
//     where
//         PosT: Pos3Like,
//         PosT::Scalar: PropSerialize
//     {
//         self.serialize_triple([v.x(), v.y(), v.z()])
//     }

//     fn serialize_normal<NormalT>(&mut self, v: &NormalT) -> Result<(), Self::Error>
//     where
//         NormalT: Vec3Like,
//         NormalT::Scalar: PropSerialize
//     {
//         self.serialize_triple([v.x(), v.y(), v.z()])
//     }

//     fn serialize_named(
//         &mut self,
//         _name: &str,
//         v: &impl PropSerialize,
//     ) -> Result<(), Self::Error>
//     {
//         self.insert_sep()?;
//         v.serialize(self.primitive())
//     }
// }


// // The little endian and big endian binary serializer are very similar. This
// // macro avoids some duplicate code.
// //
// // The binary PropSetSerializer differ from the ASCII one merely by the
// // PropSerializer used and in that fact that they don't output ' '
// // seperators.
// macro_rules! gen_binary_prop_serializer {
//     ($name:ident, $primitive_serializer:ident) => {
//         // It's just a wrapper for a `Write` reference
//         struct $name<'a, W: Write + 'a + ?Sized> {
//             writer: &'a mut W,
//         }

//         impl<'a, W: Write + 'a + ?Sized> $name<'a, W> {
//             fn new(w: &'a mut W, _: bool) -> Self {
//                 Self { writer: w }
//             }

//             fn finish_block(_: &mut W) -> Result<(), PlyError> {
//                 // In binary format, the block end implictly after the correct
//                 // number of bytes
//                 Ok(())
//             }

//             /// Returns the associated primitive serializer.
//             fn primitive(&mut self) -> $primitive_serializer<W> {
//                 $primitive_serializer {
//                     writer: &mut *self.writer,
//                 }
//             }

//             /// Helper function to serialize three values of the same type. Used
//             /// for `position`, `normal`, ...
//             fn serialize_triple(&mut self, v: [&impl PropSerialize; 3]) -> Result<(), PlyError> {
//                 v[0].serialize(self.primitive())?;
//                 v[1].serialize(self.primitive())?;
//                 v[2].serialize(self.primitive())?;

//                 Ok(())
//             }
//         }

//         impl<'a, W: Write + 'a + ?Sized> PropSetSerializer for $name<'a, W> {
//             type Error = PlyError;

//             fn serialize_position<PosT>(&mut self, v: &PosT) -> Result<(), Self::Error>
//             where
//                 PosT: Pos3Like,
//                 PosT::Scalar: PropSerialize,
//             {
//                 self.serialize_triple([v.x(), v.y(), v.z()])
//             }

//             fn serialize_normal<NormalT>(&mut self, v: &NormalT) -> Result<(), Self::Error>
//             where
//                 NormalT: Vec3Like,
//                 NormalT::Scalar: PropSerialize,
//             {
//                 self.serialize_triple([v.x(), v.y(), v.z()])
//             }

//             fn serialize_named(
//                 &mut self,
//                 _name: &str,
//                 v: &impl PropSerialize,
//             ) -> Result<(), Self::Error>
//             {
//                 // We don't need the name here. We just need to dump the value.
//                 v.serialize(self.primitive())
//             }
//         }
//     }
// }

// gen_binary_prop_serializer!(PlyBinaryBePropSetSerializer, PlyBinaryBePropSerializer);
// gen_binary_prop_serializer!(PlyBinaryLePropSetSerializer, PlyBinaryLePropSerializer);
