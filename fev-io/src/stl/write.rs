use std::{
    io::Write,
};

// use byteorder::{BigEndian, LittleEndian, WriteBytesExt};
// use splop::SkipFirst;

use cgmath::prelude::*;
use fev_core::{
    ExplicitVertex, ExplicitFace, MeshUnsorted,
    handle::{FaceHandle, VertexHandle},
    prop::{HasNormal, HasPosition, Pos3Like, PrimitiveNum, Vec3Like},
};
use fev_map::{PropMap, MeshFaceMap, MeshVertexMap};

use crate::{
    MeshWriter,
    ser::{SinglePrimitive, SinglePrimitiveSerializer},
};
use super::{StlError, StlFormat};


const DEFAULT_SOLID_NAME: &str = "mesh";



pub struct StlWriter<'a, MeshT: 'a, PosMapT, FaceNormalsT> {
    solid_name: String,
    format: StlFormat,
    mesh: &'a MeshT,
    vertex_positions: PosMapT,
    face_normals: FaceNormalsT,
}

pub trait FaceNormals {
    fn get<MeshT, PosMapT, VertexPropT>(
        &self,
        handle: FaceHandle,
        mesh: &MeshT,
        vertex_positions: &PosMapT,
    ) -> [f32; 3]
    where
        MeshT: MeshUnsorted,
        PosMapT: for<'s> PropMap<'s, VertexHandle, Target = VertexPropT>,
        VertexPropT: HasPosition;
}

pub struct CalculateFaceNormals;

impl FaceNormals for CalculateFaceNormals {
    fn get<MeshT, PosMapT, VertexPropT>(
        &self,
        handle: FaceHandle,
        mesh: &MeshT,
        vertex_positions: &PosMapT,
    ) -> [f32; 3]
    where
        MeshT: MeshUnsorted,
        PosMapT: for<'s> PropMap<'s, VertexHandle, Target = VertexPropT>,
        VertexPropT: HasPosition,
    {
        let [va, vb, vc] = mesh.vertices_of_face(handle);
        let pa = vertex_positions.get(va).unwrap().position().to_point3();
        let pb = vertex_positions.get(vb).unwrap().position().to_point3();
        let pc = vertex_positions.get(vc).unwrap().position().to_point3();

        let normal = (pb - pa).cross(pc - pa).cast::<f32>().unwrap().normalize();
        [normal.x, normal.y, normal.z]
    }
}


impl<M, FacePropT> FaceNormals for M
where
    M: for<'s> PropMap<'s, FaceHandle, Target = FacePropT>,
    FacePropT: HasNormal,
{
    fn get<MeshT, PosMapT, VertexPropT>(
        &self,
        handle: FaceHandle,
        _: &MeshT,
        _: &PosMapT,
    ) -> [f32; 3]
    where
        MeshT: MeshUnsorted,
        PosMapT: for<'s> PropMap<'s, VertexHandle, Target = VertexPropT>,
        VertexPropT: HasPosition,
    {
        let prop = PropMap::get(self, handle).unwrap();
        let normal = prop.normal();

        [
            normal.x().cast(),
            normal.y().cast(),
            normal.z().cast(),
        ]
    }
}



impl<'a, MeshT: 'a> StlWriter<'a, MeshT, MeshVertexMap<'a, MeshT>, MeshFaceMap<'a, MeshT>>
where
    MeshT: ExplicitVertex + ExplicitFace + MeshUnsorted,
{
    pub fn tmp_new(format: StlFormat, mesh: &'a MeshT) -> Result<Self, StlError> {
        // TODO: verify mesh properties

        Ok(Self {
            solid_name: DEFAULT_SOLID_NAME.into(),
            format,
            mesh,
            vertex_positions: MeshVertexMap::new(mesh),
            face_normals: MeshFaceMap::new(mesh),
        })
    }
}

impl<'a, MeshT, PosMapT, FaceNormalsT> StlWriter<'a, MeshT, PosMapT, FaceNormalsT> {
    pub fn calculate_normals(
        self
    ) -> StlWriter<'a, MeshT, PosMapT, CalculateFaceNormals> {
        StlWriter {
            solid_name: self.solid_name,
            format: self.format,
            mesh: self.mesh,
            vertex_positions: self.vertex_positions,
            face_normals: CalculateFaceNormals,
        }
    }

    pub fn with_normals<M, PropT>(
        self,
        normals: &'a M,
    ) -> StlWriter<'a, MeshT, PosMapT, &'a M>
    where
        M: for<'s> PropMap<'s, FaceHandle, Target = PropT>,
        PropT: HasNormal,
    {
        StlWriter {
            solid_name: self.solid_name,
            format: self.format,
            mesh: self.mesh,
            vertex_positions: self.vertex_positions,
            face_normals: normals,
        }
    }

    pub fn with_vertex_positions<M, PropT>(
        self,
        positions: &'a M,
    ) -> StlWriter<'a, MeshT, &'a M, FaceNormalsT>
    where
        M: for<'s> PropMap<'s, VertexHandle, Target = PropT>,
        PropT: HasPosition,
    {
        StlWriter {
            solid_name: self.solid_name,
            format: self.format,
            mesh: self.mesh,
            vertex_positions: positions,
            face_normals: self.face_normals,
        }
    }
}



impl<'a, MeshT, PosMapT, VertexPropT, FaceNormalsT> MeshWriter
    for StlWriter<'a, MeshT, PosMapT, FaceNormalsT>
where
    // TODO: maybe this is too much
    MeshT: ExplicitVertex + ExplicitFace + MeshUnsorted,
    PosMapT: for<'s> PropMap<'s, VertexHandle, Target = VertexPropT>,
    VertexPropT: HasPosition,
    <VertexPropT::Position as Pos3Like>::Scalar: SinglePrimitive,
    FaceNormalsT: FaceNormals,
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
                let [nx, ny, nz] = self.face_normals.get(
                    face.handle(),
                    self.mesh,
                    &self.vertex_positions
                );
                write!(w, "  facet normal ")?;
                nx.serialize_single(StlSerializer::new(&mut w))?;
                write!(w, " ")?;
                ny.serialize_single(StlSerializer::new(&mut w))?;
                write!(w, " ")?;
                nz.serialize_single(StlSerializer::new(&mut w))?;
                writeln!(w, "")?;

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
        use std::num::FpCategory;

        match v.classify() {
            FpCategory::Normal | FpCategory::Subnormal => {
                let exponent = v.abs().log10().floor();
                let mantissa = v / 10f32.powf(exponent);
                write!(self.writer, "{}E{:+}", mantissa, exponent)
                    .map_err(|e| e.into())
            }
            _ => {
                // `v` is either infinite, `NaN` or zero. We want to serialize
                // the zeroes as `0.0`.
                write!(self.writer, "{:.1}", v)
                    .map_err(|e| e.into())
            }
        }
    }
    fn serialize_f64(self, v: f64) -> Result<(), Self::Error> {
        self.serialize_f32(v as f32)
    }
}



#[cfg(test)]
mod test {
    use crate::{
        ser::SinglePrimitiveSerializer,
    };
    use super::{
        StlSerializer,
    };

    // Helper to serialize into a `Vec<u8>`. The vector is returned.
    macro_rules! ser_into_vec {
        (|$s:ident| $call:expr) => {{
            let mut v = Vec::new();
            {
                let $s = StlSerializer::new(&mut v);
                $call.unwrap();
            }
            v
        }}
    }

    #[test]
    fn ascii_serialize_zeroes() {
        // f32
        assert_eq!(ser_into_vec!(|s| s.serialize_f32(0.0)), b"0.0");
        assert_eq!(ser_into_vec!(|s| s.serialize_f32(-0.0)), b"0.0");

        // f64
        assert_eq!(ser_into_vec!(|s| s.serialize_f64(0.0)), b"0.0");
        assert_eq!(ser_into_vec!(|s| s.serialize_f64(-0.0)), b"0.0");

        // integers
        assert_eq!(ser_into_vec!(|s| s.serialize_u8(0)), b"0.0");
        assert_eq!(ser_into_vec!(|s| s.serialize_i8(0)), b"0.0");
        assert_eq!(ser_into_vec!(|s| s.serialize_u16(0)), b"0.0");
        assert_eq!(ser_into_vec!(|s| s.serialize_i16(0)), b"0.0");
        assert_eq!(ser_into_vec!(|s| s.serialize_u32(0)), b"0.0");
        assert_eq!(ser_into_vec!(|s| s.serialize_i32(0)), b"0.0");
        assert_eq!(ser_into_vec!(|s| s.serialize_u64(0)), b"0.0");
        assert_eq!(ser_into_vec!(|s| s.serialize_i64(0)), b"0.0");
    }

    #[test]
    fn ascii_serialize_subnormals() {
        assert_eq!(ser_into_vec!(|s| s.serialize_f32(1.1754942e-38)), b"1.1754943E-38");
        assert_eq!(ser_into_vec!(|s| s.serialize_f64(1.1754942e-38)), b"1.1754943E-38");
    }

    #[test]
    fn ascii_serialize_nan() {
        use std::{f32, f64};

        assert_eq!(ser_into_vec!(|s| s.serialize_f32(f32::NAN)), b"NaN");
        assert_eq!(ser_into_vec!(|s| s.serialize_f64(f64::NAN)), b"NaN");
    }
}
