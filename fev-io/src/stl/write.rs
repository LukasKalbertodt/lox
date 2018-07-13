use std::io::{self, Write};

use byteorder::{LittleEndian, WriteBytesExt};
use cgmath::prelude::*;

use fev_core::{
    ExplicitVertex, ExplicitFace, MeshUnsorted,
    handle::{FaceHandle, VertexHandle},
    prop::{HasNormal, HasPosition, Pos3Like, PrimitiveNum, Vec3Like},
};
use fev_map::{PropMap, MeshFaceMap, MeshVertexMap};

use crate::MeshWriter;
use super::{StlError, StlFormat};


const DEFAULT_SOLID_NAME: &str = "mesh";



// ===============================================================================================
// ===== StlWriter
// ===============================================================================================

pub struct StlWriter<'a, MeshT: 'a, PosMapT, FaceNormalsT> {
    solid_name: String,
    format: StlFormat,
    mesh: &'a MeshT,
    vertex_positions: PosMapT,
    face_normals: FaceNormalsT,
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
            face_normals: CalculateFaceNormals(()),
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

    pub fn with_solid_name(self, name: impl Into<String>) -> Self {
        Self {
            solid_name: name.into(),
            .. self
        }
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
        let get_pos_and_normal = |face_handle| {
            let [va, vb, vc] = self.mesh.vertices_of_face(face_handle);
            let positions = [
                self.vertex_positions.pos_of(va),
                self.vertex_positions.pos_of(vb),
                self.vertex_positions.pos_of(vc),
            ];

            (
                positions,
                self.face_normals.normal_of(face_handle, positions),
            )
        };

        if self.format == StlFormat::Ascii {
            // ===============================================================
            // ===== STL ASCII
            // ===============================================================
            writeln!(w, "solid {}", self.solid_name)?;

            for face in self.mesh.faces() {
                let (positions, normal) = get_pos_and_normal(face.handle());

                // Write face normal
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
                let (positions, [nx, ny, nz]) = get_pos_and_normal(face.handle());

                // Write face normal
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


// ===============================================================================================
// ===== Helper functions for number encoding
// ===============================================================================================

/// Writes the three values of the given vector (in STL ASCII format, separated
/// by ' ') into the writer.
fn write_ascii_vector(w: &mut Write, [x, y, z]: [f32; 3]) -> Result<(), io::Error> {
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


// ===============================================================================================
// ===== Helper traits for vertex positions and face normals
// ===============================================================================================

/// Property maps that contain a position.
///
/// This is pretty much just a trait alias for `PropMap<VertexHandle>` where
/// `Target: HasPosition` (see the only impl).
pub trait VertexPositions {
    /// The vertex position with `f32` scalars. STL only supports `f32`
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

/// Types that can provide a normal for a given triangular face.
///
/// There are basically two types implementing this trait:
///
/// - `CalculateFaceNormals`: the face normal is calculated from the three
///   vertex positions. For this to work, all faces must have a non-zero area
///   (two edge vectors need to be linear independent).
/// - Property maps which return something that `HasNormal`.
pub trait FaceNormals {
    #[inline(always)]
    fn normal_of(&self, handle: FaceHandle, vertex_positions: [[f32; 3]; 3]) -> [f32; 3];
}

/// Calculates the face normal from the vertex positions.
pub struct CalculateFaceNormals(());

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


// ===============================================================================================
// ===== Tests
// ===============================================================================================
#[cfg(test)]
mod test {
    use super::{
        write_ascii_f32,
    };

    // Helper to serialize into a `Vec<u8>`. The vector is returned.
    macro_rules! f32_into_vec {
        ($value:expr) => {{
            let mut v = Vec::new();
            write_ascii_f32(&mut v, $value).unwrap();
            v
        }}
    }

    #[test]
    fn ascii_serialize_zeroes() {
        assert_eq!(f32_into_vec!( 0.0), b"0.0");
        assert_eq!(f32_into_vec!(-0.0), b"0.0");
    }

    #[test]
    fn ascii_serialize_subnormals() {
        assert_eq!(f32_into_vec!(1.1754942e-38), b"1.1754943E-38");
    }

    #[test]
    fn ascii_serialize_nan() {
        assert_eq!(f32_into_vec!(::std::f32::NAN), b"NaN");
    }
}
