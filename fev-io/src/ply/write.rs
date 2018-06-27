use std::{
    collections::HashMap,
    io::Write,
};
use frunk::{
    HCons, HNil,
};

use fev_core::{
    ExplicitVertex, ExplicitFace, MeshElement,
    handle::{VertexHandle},
    prop::{LabeledPropList, PropLabel},
};
use fev_map::{
    PropMap,
    fn_map::FnMap,
};

use crate::{
    MeshWriter,
    ser::{PropListSerialize, TypedLabel},
};
use super::{PlyError, PlyFormat};
use self::internal::*;



pub struct PlyWriter<'a, MeshT: 'a, VertexL: PlyPropTopList<VertexHandle>> {
    format: PlyFormat,
    mesh: &'a MeshT,
    vertex_props: VertexL,
    vertex_prop_names: HashMap<String, TypedLabel>,
}

impl<'a, MeshT> PlyWriter<'a, MeshT, HNil>
where
    MeshT: ExplicitVertex + ExplicitFace,
    MeshT::VertexProp: LabeledPropList + PropListSerialize,
{
    pub fn tmp_new(format: PlyFormat, mesh: &'a MeshT) -> Result<Self, PlyError> {
        let mut vertex_prop_names = HashMap::new();
        let typed_labels = MeshT::VertexProp::typed_labels();
        Self::add_names(&mut vertex_prop_names, &typed_labels)?;

        Ok(Self {
            format,
            mesh,
            vertex_props: HNil,
            vertex_prop_names,
        })
    }
}

impl<'a, MeshT, VertexL: PlyPropTopList<VertexHandle>> PlyWriter<'a, MeshT, VertexL> {
    pub fn add_vertex_prop<MapT>(
        mut self,
        map: &'a MapT,
    ) -> Result<PlyWriter<'a, MeshT, VertexL::Out>, PlyError>
    where
        MapT: PropMap<'a, VertexHandle>,
        MapT::Target: PropListSerialize + LabeledPropList,
        VertexL: PlyPropTopListAdd<VertexHandle, PropListDesc<'a, MapT>>,
    {
        let desc = PropListDesc {
            typed_labels: MapT::Target::typed_labels(),
            data: map,
        };

        Self::add_names(&mut self.vertex_prop_names, &desc.typed_labels)?;

        Ok(PlyWriter {
            format: self.format,
            mesh: self.mesh,
            vertex_props: self.vertex_props.add(desc),
            vertex_prop_names: self.vertex_prop_names,
        })
    }

    pub fn add_vertex_prop_as<MapT>(
        mut self,
        map: &'a MapT,
        labels: &[PropLabel],
    ) -> Result<PlyWriter<'a, MeshT, VertexL::Out>, PlyError>
    where
        MapT: PropMap<'a, VertexHandle>,
        MapT::Target: PropListSerialize,
        VertexL: PlyPropTopListAdd<VertexHandle, PropListDesc<'a, MapT>>,
    {
        // Obtain typed labels from `labels` array and from the `MapT::Target`
        // type information.
        let typed_labels = labels.iter()
            .cloned()
            .enumerate()
            .map(|(i, label)| {
                TypedLabel {
                    label: label,
                    data_type: MapT::Target::data_type_of(i),
                }
            })
            .collect();


        let desc = PropListDesc {
            typed_labels,
            data: map,
        };

        Self::add_names(&mut self.vertex_prop_names, &desc.typed_labels)?;

        Ok(PlyWriter {
            format: self.format,
            mesh: self.mesh,
            vertex_props: self.vertex_props.add(desc),
            vertex_prop_names: self.vertex_prop_names,
        })
    }

    /// Adds the PLY property names of all typed labels to the given hash map
    /// and checks for duplicate names. If a duplicate name is found,
    /// `PlyError::NameAlreadyInUse` is returned.
    fn add_names(
        names: &mut HashMap<String, TypedLabel>,
        typed_labels: &[TypedLabel],
    ) -> Result<(), PlyError> {
        for tl in typed_labels {
            let new_names = names_of_prop(&tl);

            // Check for duplicates
            if let Some(name) = new_names.iter().find(|s| names.contains_key(*s)) {
                return Err(
                    PlyError::NameAlreadyInUse {
                        name: name.clone(),
                        element: MeshElement::Vertex, // TODO
                        old_label: names.get(name).unwrap().clone(),
                        new_label: tl.clone(),
                    }
                );
            }

            // Add new names to map
            names.extend(new_names.into_iter().map(|s| (s, tl.clone())));
        }

        Ok(())
    }
}


impl<'a, MeshT, VertexL> MeshWriter for PlyWriter<'a, MeshT, VertexL>
where
    VertexL: PlyPropTopList<VertexHandle>,// + HList + for<'x> ToRef<'x>,
    //for<'x> <VertexL as ToRef<'x>>::Output: HList + PlyPropTopList<VertexHandle>,
    // <Hlist!(&PropListDesc<'a, impl PropMap<'_, VertexHandle>>) as Add<VertexL::Output>>::Output: PropLists<VertexHandle>,
    // for<'x> &'x VertexL: PropLists<VertexHandle> + HList,
    MeshT: ExplicitVertex + ExplicitFace,
    MeshT::VertexProp: LabeledPropList + PropListSerialize,
{
    type Error = PlyError;

    fn write(&self, mut w: impl Write) -> Result<(), Self::Error> {
        let mesh_vertex_props = {
            let typed_labels = (0..MeshT::VertexProp::num_props())
                .map(|i| {
                    TypedLabel {
                        label: MeshT::VertexProp::label_of(i),
                        data_type: MeshT::VertexProp::data_type_of(i)
                    }
                })
                .collect();

            PropListDesc {
                typed_labels,
                data: &FnMap(|handle| self.mesh.vertex_prop(handle)),
            }
        };

        // let test = &FnMap(|handle| self.mesh.vertex_prop(handle));
        // PropMap::get(&test, VertexHandle::from_usize(0));

        // let vertex_props = hlist!(&mesh_vertex_props) + self.vertex_props.to_ref();
        let vertex_props = HCons {
            head: mesh_vertex_props,
            tail: &self.vertex_props,
            // tail: HNil,
        };




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
        // for i in 0..MeshT::VertexProp::num_props() {
        //     let label = MeshT::VertexProp::label_of(i);
        //     let ty = MeshT::VertexProp::data_type_of(i);
        //     write_header_property(&mut w, &label, ty)?;
        // }

        vertex_props.write_header(&mut w)?;
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



        for v in self.mesh.vertices() {
            let block = AsciiBlock::new(&mut w);
            // (hlist!(mesh_vertex_props.clone()) + self.vertex_props)
            // hlist!(mesh_vertex_props.clone())
            vertex_props
                .serialize_block(block, v.handle())?;


            // for (i, prop) in self.vertex_prop_sets.iter().enumerate() {
            //     (prop.serialize)(&mut w, i == 0, vertex_handle)?;
            // }

            // finish_block(&mut w)?;
        }

        // for face_handle in self.mesh.faces() {
        //     for (i, prop) in self.face_prop_sets.iter().enumerate() {
        //         (prop.serialize)(&mut w, i == 0, face_handle)?;
        //     }

        //     finish_block(&mut w)?;
        // }

        Ok(())
    }
}

// Most types and traits live in this private module. That's because we are
// forced to use some of those traits in the signature of `PlyWriter`. Using
// private items in public signatures is not allowed.
//
// However, these traits are really only used internally and shouldn't concern
// the user. Therefore the "`pub` in private module" trick is used.
mod internal {
    use std::{
        io::Write,
    };
    use byteorder::{BigEndian, LittleEndian, WriteBytesExt};
    use frunk::{
        HCons, HNil,
    };

    use fev_core::{
        handle::Handle,
        prop::PropLabel,
    };
    use fev_map::{
        PropMap,
    };

    use crate::{
        ser::{DataType, PrimitiveType, Serialize, Serializer, PropListSerialize, TypedLabel},
    };
    use super::PlyError;


    /// The internal representation of a single property list.
    pub struct PropListDesc<'a, M: 'a> {
        /// The labels and types of the properties in this list.
        pub(in ply) typed_labels: Vec<TypedLabel>,

        /// The map holding the properties.
        pub(in ply) data: &'a M,
    }


    // =======================================================================
    // ===== Traits for dealing with Hlists
    // =======================================================================

    /// A heterogeneous list of property lists that can be serialized into a
    /// PLY file. The elements of the list are instances of `PropListDesc`.
    pub trait PlyPropTopList<H: Handle> {
        /// Writes the header descriptions of all properties in this list into
        /// the given writer.
        fn write_header(&self, w: &mut impl Write) -> Result<(), PlyError>;

        /// Serializes all properties in this list associated with the given
        /// handle into the given block.
        fn serialize_block(&self, block: impl PlyBlock, handle: H) -> Result<(), PlyError>;
    }

    // Impl for empty list
    impl<H: Handle> PlyPropTopList<H> for HNil {
        fn write_header(&self, _: &mut impl Write) -> Result<(), PlyError> {
            Ok(())
        }

        fn serialize_block(&self, block: impl PlyBlock, _: H) -> Result<(), PlyError> {
            block.finish()
        }
    }

    // Impl for references to lists
    impl<'a, H: Handle, L: PlyPropTopList<H>> PlyPropTopList<H> for &'a L {
        fn write_header(&self, w: &mut impl Write) -> Result<(), PlyError> {
            (*self).write_header(w)
        }

        fn serialize_block(&self, block: impl PlyBlock, handle: H) -> Result<(), PlyError> {
            (*self).serialize_block(block, handle)
        }
    }

    // The main impl.
    //
    // We require the list to contain instances of `PropListDesc` with property
    // maps whose properties can be serialized.
    impl<'a, H: Handle, MapT, Tail> PlyPropTopList<H> for HCons<PropListDesc<'a, MapT>, Tail>
    where
        MapT: PropMap<'a, H>,
        MapT::Target: PropListSerialize,
        Tail: PlyPropTopList<H>,
    {
        fn write_header(&self, w: &mut impl Write) -> Result<(), PlyError> {
            // Write header for all properties in this property list
            for tl in &self.head.typed_labels {
                write_header_property({w}, &tl.label, tl.data_type)?;
            }

            // Proceed to next item in top list.
            self.tail.write_header(w)
        }

        fn serialize_block(&self, mut block: impl PlyBlock, handle: H) -> Result<(), PlyError> {
            for (i, tl) in self.head.typed_labels.iter().enumerate() {
                match self.head.data.get(handle) {
                    Some(props) => block.add(props, i)?,
                    None => {
                        panic!(
                            "PropMap incomplete: no value for handle {:?} (property {:?}: {:?})",
                            handle,
                            tl.label,
                            tl.data_type,
                        );
                    }
                }
            }
            self.tail.serialize_block(block, handle)
        }
    }

    pub trait PlyPropTopListAdd<H: Handle, N> {
        type Out: PlyPropTopList<H>;
        fn add(self, item: N) -> Self::Out;
    }

    impl<'a, H, MapT> PlyPropTopListAdd<H, PropListDesc<'a, MapT>> for HNil
    where
        H: Handle,
        MapT: PropMap<'a, H>,
        MapT::Target: PropListSerialize,
    {
        type Out = Hlist!(PropListDesc<'a, MapT>);
        fn add(self, item: PropListDesc<'a, MapT>) -> Self::Out {
            hlist!(item)
        }
    }

    impl<H, T, MapT, Tail> PlyPropTopListAdd<H, T> for HCons<PropListDesc<'a, MapT>, Tail>
    where
        H: Handle,
        MapT: PropMap<'a, H>,
        MapT::Target: PropListSerialize,
        Tail: PlyPropTopListAdd<H, T>,
    {
        type Out = HCons<PropListDesc<'a, MapT>, Tail::Out>;
        fn add(self, item: T) -> Self::Out {
            hlist!(self.head, ...self.tail.add(item))
        }
    }


    // =======================================================================
    // ===== Functions with PLY format logic
    // =======================================================================

    /// Writes the header entry for one property to the given writer.
    fn write_header_property(
        w: &mut impl Write,
        // TODO: Replace with TypedLabel
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

            (PropLabel::Named(name), DataType::FixedLen { len, ty }) => {
                let ty_name = primitive_ply_type_name(ty)?;
                for i in 0..len {
                    writeln!(w, "property {} {}[{}]", ty_name, name, i)?;
                }
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

    pub (in ply) fn names_of_prop(tl: &TypedLabel) -> Vec<String> {
        match (&tl.label, tl.data_type) {
            (PropLabel::Position, _) => vec!["x".into(), "y".into(), "z".into()],
            (PropLabel::Normal, _) => vec!["nx".into(), "ny".into(), "nz".into()],

            (PropLabel::Named(name), DataType::Single(_)) |
            (PropLabel::Named(name), DataType::VariableLen(_)) => vec![name.clone()],

            (PropLabel::Named(name), DataType::FixedLen { len, .. }) => {
                (0..len).map(|i| format!("{}[{}]", name, i)).collect()
            }
        }
    }


    // =======================================================================
    // ===== Definition of blocks (properties for one specific element)
    // =======================================================================

    /// A ply block holds all properties for one specific element. In the
    /// ASCII format this equivalent to "one line". This trait generalizes over
    /// property seperators and block terminators (' ' and '\n' for ASCII,
    /// nothing for binary formats).
    pub trait PlyBlock {
        // TODO: This function could return a `Serialize` instead with GATs
        /// Adds the property from the property list at the given index to the
        /// block. This function is in charge of inserting seperators when
        /// necessary.
        fn add(
            &mut self,
            props: impl PropListSerialize,
            index: usize,
        ) -> Result<(), PlyError>;

        /// Finishes the block. Writes '\n' for ASCII format, does nothing for
        /// binary formats.
        fn finish(self) -> Result<(), PlyError>;
    }


    pub(in ply) struct AsciiBlock<'a, W: 'a + Write> {
        writer: &'a mut W,
        at_start: bool,
    }

    impl<'a, W: 'a + Write> AsciiBlock<'a, W> {
        pub(in ply) fn new(w: &'a mut W) -> Self {
            Self {
                writer: w,
                at_start: true,
            }
        }
    }

    impl<'a, W: 'a + Write> PlyBlock for AsciiBlock<'a, W> {
        fn add(
            &mut self,
            props: impl PropListSerialize,
            index: usize,
        ) -> Result<(), PlyError> {
            if self.at_start {
                self.at_start = false;
            } else {
                self.writer.write_all(b" ")?;
            }

            let ser = AsciiSerializer::new(self.writer);
            props.serialize_at(index, ser)
        }

        fn finish(self) -> Result<(), PlyError> {
            self.writer.write_all(b"\n")?;
            Ok(())
        }
    }


    // ===========================================================================
    // ===== Serializer for PLY
    // ===========================================================================

    /// Serializes primitives as PLY ASCII (simply using the `Display` impl).
    ///
    /// Unfortunately, the PLY format isn't too detailed. So we can't know for
    /// sure how to serialize numbers. E.g. it's not specified how many decimal
    /// digits are allowed/required and if floats should be printed in
    /// scientific notation or not. So yes, we just use `Display` which seems
    /// to work.
    struct AsciiSerializer<'a, W: Write + 'a + ?Sized> {
        writer: &'a mut W,
    }

    impl<'a, W: Write + 'a + ?Sized> AsciiSerializer<'a, W> {
        fn new(w: &'a mut W) -> Self {
            Self {
                writer: w,
            }
        }
    }

    impl<'a, W: Write + 'a + ?Sized> Serializer for AsciiSerializer<'a, W> {
        type Error = PlyError;

        fn serialize_bool(self, v: bool) -> Result<(), Self::Error> {
            // Serialize bool as small integer.
            self.serialize_u8(v as u8)
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

        fn serialize_fixed_len_seq<T: Serialize>(self, seq: &[T]) -> Result<(), Self::Error> {
            let mut first = true;
            for v in seq {
                if first {
                    first = false;
                } else {
                    write!(self.writer, " ")?;
                }
                v.serialize(Self::new(self.writer))?;
            }

            Ok(())
        }

        fn serialize_variable_len_seq<T: Serialize>(self, seq: &[T]) -> Result<(), Self::Error> {
            // Serialize length of sequence first, then all elements.
            write!(self.writer, "{}", seq.len())?;

            for v in seq {
                write!(self.writer, " ")?;
                v.serialize(Self::new(self.writer))?;
            }

            Ok(())
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

            impl<'a, W: Write + 'a + ?Sized> Serializer for $name<'a, W> {
                type Error = PlyError;

                fn serialize_bool(self, v: bool) -> Result<(), Self::Error> {
                    // We convert the bool to `u8` to serialize it, because PLY
                    // doesn't support booleans.
                    self.serialize_u8(v as u8)
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

                fn serialize_fixed_len_seq<T: Serialize>(self, seq: &[T]) -> Result<(), Self::Error> {
                    for v in seq {
                        v.serialize(Self::new(self.writer))?;
                    }

                    Ok(())
                }

                fn serialize_variable_len_seq<T: Serialize>(self, seq: &[T]) -> Result<(), Self::Error> {
                    self.writer.write_u32::<$endianess>(seq.len() as u32)?;

                    for v in seq {
                        v.serialize(Self::new(self.writer))?;
                    }

                    Ok(())
                }
            }
        }
    }

    gen_binary_primitive_serializer!(BinaryBeSerializer, BigEndian);
    gen_binary_primitive_serializer!(BinaryLeSerializer, LittleEndian);
}
