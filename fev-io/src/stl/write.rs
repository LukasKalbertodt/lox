use std::{
    io::Write,
};

// use byteorder::{BigEndian, LittleEndian, WriteBytesExt};
// use splop::SkipFirst;

use fev_core::{
    ExplicitVertex, ExplicitFace, MeshUnsorted,
    handle::{Handle, FaceHandle, VertexHandle},
    // prop::{LabeledPropList, PropLabel},
    prop::{HasPosition, Pos3Like},
};
use fev_map::{PropMap, MeshVertexMap};

use crate::{
    MeshWriter,
    ser::{SinglePrimitive, SinglePrimitiveSerializer},
};
use super::{StlError, StlFormat};


const DEFAULT_SOLID_NAME: &str = "mesh";


// pub struct NoneType;
// pub struct SomeType<T>(T);

pub struct StlWriter<'a, MeshT: 'a, PosMapT> {
    solid_name: String,
    format: StlFormat,
    mesh: &'a MeshT,
    vertex_positions: PosMapT,
}




impl<'a, MeshT: 'a> StlWriter<'a, MeshT, MeshVertexMap<'a, MeshT>>
where
    MeshT: ExplicitVertex + ExplicitFace + MeshUnsorted,
{
    pub fn tmp_new(format: StlFormat, mesh: &'a MeshT) -> Result<Self, StlError> {
        // TODO: verify mesh properties

        Ok(Self {
            solid_name: DEFAULT_SOLID_NAME.into(),
            format,
            mesh,
            vertex_positions: MeshVertexMap::new(&mesh),
        })
    }
}



impl<'a, MeshT, PosMapT, VertexPropT> MeshWriter for StlWriter<'a, MeshT, PosMapT>
where
    // TODO: maybe this is too much
    MeshT: ExplicitVertex + ExplicitFace + MeshUnsorted,
    PosMapT: for<'s> PropMap<'s, VertexHandle, Target = VertexPropT>,
    VertexPropT: HasPosition,
    <VertexPropT::Position as Pos3Like>::Scalar: SinglePrimitive,
{
    type Error = StlError;

    fn write(&self, mut w: impl Write) -> Result<(), Self::Error> {
        if self.format == StlFormat::Ascii {
            // ===============================================================
            // ===== STL ASCII
            // ===============================================================

            writeln!(w, "solid {}", self.solid_name)?;

            for face in self.mesh.faces() {
                // TODO: normal
                writeln!(w, "  facet normal 1.0 0.0 0.0")?;
                writeln!(w, "    outer loop")?;

                for vertex_handle in &self.mesh.vertices_of_face(face.handle()) {
                    let prop = self.vertex_positions
                        .get(*vertex_handle)
                        .unwrap();
                    let pos = prop.position();

                    write!(w, "      vertex ")?;
                    pos.x().serialize_single(StlSerializer::new(&mut w))?;
                    write!(w, " ")?;
                    pos.y().serialize_single(StlSerializer::new(&mut w))?;
                    write!(w, " ")?;
                    pos.z().serialize_single(StlSerializer::new(&mut w))?;
                    writeln!(w, "")?;
                }

                writeln!(w, "    endloop")?;
                writeln!(w, "  endfacet")?;
            }

            writeln!(w, "endsolid {}", self.solid_name)?;
        } else {
            // ===============================================================
            // ===== STL binary
            // ===============================================================
            unimplemented!()
        }

        Ok(())
    }
}


struct StlSerializer<'a, W: 'a + Write> {
    writer: &'a mut W,
}

impl<'a, W: Write> StlSerializer<'a, W> {
    fn new(writer: &'a mut W) -> Self {
        Self { writer }
    }
}

// As STL only supports 32 bit floats, we cast all other types into `f32`. This
// can lead to loss of precision! We might want to change the behavior later
// and return an error instead.
impl<'a, W: Write> SinglePrimitiveSerializer for StlSerializer<'a, W> {
    type Error = StlError;

    fn serialize_bool(self, v: bool) -> Result<(), Self::Error> {
        // Serialize bool as small integer.
        self.serialize_f32(v as u8 as f32)
    }
    fn serialize_i8(self, v: i8) -> Result<(), Self::Error> {
        self.serialize_f32(v.into())
    }
    fn serialize_i16(self, v: i16) -> Result<(), Self::Error> {
        self.serialize_f32(v.into())
    }
    fn serialize_i32(self, v: i32) -> Result<(), Self::Error> {
        self.serialize_f32(v as f32)
    }
    fn serialize_i64(self, v: i64) -> Result<(), Self::Error> {
        self.serialize_f32(v as f32)
    }
    fn serialize_u8(self, v: u8) -> Result<(), Self::Error> {
        self.serialize_f32(v.into())
    }
    fn serialize_u16(self, v: u16) -> Result<(), Self::Error> {
        self.serialize_f32(v.into())
    }
    fn serialize_u32(self, v: u32) -> Result<(), Self::Error> {
        self.serialize_f32(v as f32)
    }
    fn serialize_u64(self, v: u64) -> Result<(), Self::Error> {
        self.serialize_f32(v as f32)
    }
    fn serialize_f32(self, v: f32) -> Result<(), Self::Error> {
        // The STL specification is terribly underspecified. The only
        // information about how to encode floats in ASCII is this:
        //
        // > The numerical data in the facet normal and vertex lines are single
        // > precision floats, for example, 1.23456E+789. A facet normal
        // > coordinate may have a leading minus sign; a vertex coordinate may
        // > not.
        //
        // I don't think the last sentence makes any sense: why forbid negative
        // coordinates? In any case, no one in the real world cares about that:
        // there are plenty of STL files out there with negative vertex
        // coordinates.
        //
        // About the actual format: clearly unhelpful. In real world STL files
        // floats are encoded all over the place. I've seen `1`, `1.2`, `10.2`,
        // `1.02e1`, `1.020000E+001` and more. We just stick to the exact
        // format mentioned in the "specification". This does not necessarily
        // make any sense and wastes memory, but so does ASCII STL. Just don't
        // use the ASCII STL format!
        let exponent = v.log10().floor();
        let mantissa = v / 10f32.powf(exponent);
        write!(self.writer, "{}E{:+}", mantissa, exponent)
            .map_err(|e| e.into())
    }
    fn serialize_f64(self, v: f64) -> Result<(), Self::Error> {
        self.serialize_f32(v as f32)
    }
}

// trait StlVertexPositions {

// }

// impl<B: HasPosition> StlVertexPositions for (NoneType, B) {

// }

// impl<'a, A, B> StlVertexPositions for (SomeType<A>, B)
// where
//     A: PropMap<'a, VertexHandle>,
//     A::Target: HasPosition,
// {

// }
