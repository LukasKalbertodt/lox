use std::{
    io::{self, Write},
};

use byteorder::{LittleEndian, WriteBytesExt};
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
    fn normal_of(&self, handle: FaceHandle, vertex_positions: [[f32; 3]; 3]) -> [f32; 3];
}

pub struct CalculateFaceNormals;

impl FaceNormals for CalculateFaceNormals {
    fn normal_of(&self, _: FaceHandle, [pos_a, pos_b, pos_c]: [[f32; 3]; 3]) -> [f32; 3] {
        let pos_a = pos_a.to_point3();
        let pos_b = pos_b.to_point3();
        let pos_c = pos_c.to_point3();

        let normal = (pos_b - pos_a).cross(pos_c - pos_a).normalize();
        [normal.x, normal.y, normal.z]
    }
}


impl<M, FacePropT> FaceNormals for M
where
    M: for<'s> PropMap<'s, FaceHandle, Target = FacePropT>,
    FacePropT: HasNormal,
{
    fn normal_of(&self, handle: FaceHandle, _: [[f32; 3]; 3]) -> [f32; 3] {
        let prop = self.get(handle).unwrap();
        let normal = prop.normal();

        [normal.x().cast(), normal.y().cast(), normal.z().cast()]
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
    pub fn with_solid_name(self, name: impl Into<String>) -> Self {
        Self {
            solid_name: name.into(),
            .. self
        }
    }

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

    pub fn with_normals<M: FaceNormals>(
        self,
        normals: &'a M,
    ) -> StlWriter<'a, MeshT, PosMapT, &'a M> {
        StlWriter {
            solid_name: self.solid_name,
            format: self.format,
            mesh: self.mesh,
            vertex_positions: self.vertex_positions,
            face_normals: normals,
        }
    }

    pub fn with_vertex_positions<M: VertexPositions>(
        self,
        positions: &'a M,
    ) -> StlWriter<'a, MeshT, &'a M, FaceNormalsT> {
        StlWriter {
            solid_name: self.solid_name,
            format: self.format,
            mesh: self.mesh,
            vertex_positions: positions,
            face_normals: self.face_normals,
        }
    }
}

pub trait VertexPositions {
    fn pos_of(&self, handle: VertexHandle) -> [f32; 3];
}

impl<M, VertexPropT> VertexPositions for M
where
    M: for<'s> PropMap<'s, VertexHandle, Target = VertexPropT>,
    VertexPropT: HasPosition,
{
    fn pos_of(&self, handle: VertexHandle) -> [f32; 3] {
        let prop = self.get(handle).unwrap();
        let pos = prop.position();

        [pos.x().cast(), pos.y().cast(), pos.z().cast()]
    }
}



impl<'a, MeshT, PosMapT, FaceNormalsT> MeshWriter
    for StlWriter<'a, MeshT, PosMapT, FaceNormalsT>
where
    // TODO: maybe this is too much
    MeshT: ExplicitVertex + ExplicitFace + MeshUnsorted,
    PosMapT: VertexPositions,
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
                // Obtain vertex positions
                let [va, vb, vc] = self.mesh.vertices_of_face(face.handle());
                let positions = [
                    self.vertex_positions.pos_of(va),
                    self.vertex_positions.pos_of(vb),
                    self.vertex_positions.pos_of(vc),
                ];

                // Write face normal
                let normal = self.face_normals.normal_of(face.handle(), positions);
                write!(w, "  facet normal ")?;
                write_ascii_vector(&mut w, normal)?;
                writeln!(w, "")?;

                // Write all vertex positions
                writeln!(w, "    outer loop")?;
                for &vertex_pos in &positions {

                    write!(w, "      vertex ")?;
                    write_ascii_vector(&mut w, vertex_pos)?;
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
            // First, a 80 bytes useless header that must not begin with
            // "solid"!
            let signature = b"FEV   ... PLY specs are terrible ...";
            let padding = vec![b' '; 80 - signature.len()];
            w.write_all(signature)?;
            w.write_all(&padding)?;

            // Next, number of triangles
            w.write_u32::<LittleEndian>(self.mesh.num_faces())?;

            for face in self.mesh.faces() {
                // Obtain vertex positions
                let [va, vb, vc] = self.mesh.vertices_of_face(face.handle());
                let positions = [
                    self.vertex_positions.pos_of(va),
                    self.vertex_positions.pos_of(vb),
                    self.vertex_positions.pos_of(vc),
                ];

                // Write face normal
                let [nx, ny, nz] = self.face_normals.normal_of(face.handle(), positions);
                w.write_f32::<LittleEndian>(nx)?;
                w.write_f32::<LittleEndian>(ny)?;
                w.write_f32::<LittleEndian>(nz)?;

                // Write all vertex positions
                for &[x, y, z] in &positions {
                    w.write_f32::<LittleEndian>(x)?;
                    w.write_f32::<LittleEndian>(y)?;
                    w.write_f32::<LittleEndian>(z)?;
                }

                // Write "attribute byte count". As Wikipedia beautifully put
                // it: "this should be zero because most software does not
                // understand anything else." Great. Some people abuse this to
                // store color or other information. This is terrible, we won't
                // use it.
                w.write_u16::<LittleEndian>(0)?;
            }
        }

        Ok(())
    }
}

pub fn write_ascii_vector(w: &mut Write, [x, y, z]: [f32; 3]) -> Result<(), io::Error> {
    write_ascii_f32(w, x)?;
    write!(w, " ")?;
    write_ascii_f32(w, y)?;
    write!(w, " ")?;
    write_ascii_f32(w, z)?;

    Ok(())
}

/// Writes the given `f32` in STL ASCII format into the given writer.
///
/// The STL specification is terribly underspecified. The only information
/// about how to encode floats in ASCII is this:
///
/// > The numerical data in the facet normal and vertex lines are single
/// > precision floats, for example, 1.23456E+789. A facet normal coordinate
/// > may have a leading minus sign; a vertex coordinate may not.
///
/// I don't think the last sentence makes any sense: why forbid negative
/// coordinates? In any case, no one in the real world cares about that: there
/// are plenty of STL files out there with negative vertex coordinates.
///
/// About the actual format: clearly unhelpful. In real world STL files floats
/// are encoded all over the place. I've seen `1`, `1.2`, `10.2`, `1.02e1`,
/// `1.020000E+001` and more. We just stick to the exact format mentioned in
/// the "specification". This does not necessarily make any sense and wastes
/// memory, but so does ASCII STL. Just don't use the ASCII STL format!
fn write_ascii_f32(w: &mut Write, v: f32) -> Result<(), io::Error> {
    use std::num::FpCategory;

    match v.classify() {
        FpCategory::Normal | FpCategory::Subnormal => {
            let exponent = v.abs().log10().floor();
            let mantissa = v / 10f32.powf(exponent);
            write!(w, "{}E{:+}", mantissa, exponent)
        }
        _ => {
            // `v` is either infinite, `NaN` or zero. We want to serialize
            // the zeroes as `0.0`.
            write!(w, "{:.1}", v)
        }
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
