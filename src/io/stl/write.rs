use std::{
    io::{self, Write},
};

use byteorder::{LittleEndian, WriteBytesExt};
use cgmath::prelude::*;

use crate::{
    Mesh, MeshUnsorted, ExplicitFace,
    map::{EmptyMap, FacePropMap, VertexPropMap},
    math::{Pos3Like, Vec3Like},
    io::{IntoMeshWriter, MeshWriter},
};
use super::{Error, Format};


const DEFAULT_SOLID_NAME: &str = "mesh";
type DummyMap = EmptyMap<[f32; 3]>;


// ===============================================================================================
// ===== STL Serializer
// ===============================================================================================

#[derive(Clone, Debug)]
pub struct Serializer {
    solid_name: String,
    format: Format,
}

impl Serializer {
    fn new(format: Format) -> Self {
        Self {
            solid_name: DEFAULT_SOLID_NAME.into(),
            format,
        }
    }

    pub fn binary() -> Self {
        Self::new(Format::Binary)
    }

    pub fn ascii() -> Self {
        Self::new(Format::Ascii)
    }

    pub fn with_solid_name(self, name: impl Into<String>) -> Self {
        Self {
            solid_name: name.into(),
            .. self
        }
    }
}

impl<'a, MeshT, PosM> IntoMeshWriter<'a, MeshT, PosM> for Serializer
where
    MeshT: 'a + Mesh + MeshUnsorted + ExplicitFace,
    PosM: 'a + VertexPropMap,
    PosM::Target: Pos3Like<Scalar = f32>,
{
    type Writer = Writer<'a, MeshT, PosM, DummyMap>;
    fn into_writer(self, mesh: &'a MeshT, vertex_positions: &'a PosM) -> Self::Writer {
        Writer::new(self, mesh, vertex_positions)
    }
}


// ===============================================================================================
// ===== STL Writer
// ===============================================================================================

#[derive(Debug)]
pub struct Writer<'a, MeshT, PosM, NormalM>
where
    MeshT: Mesh + MeshUnsorted + ExplicitFace,
    PosM: VertexPropMap,
    PosM::Target: Pos3Like<Scalar = f32>,
    NormalM: FacePropMap,
    NormalM::Target: Vec3Like<Scalar = f32>,
{
    ser: Serializer,
    mesh: &'a MeshT,
    vertex_positions: &'a PosM,
    face_normals: Option<&'a NormalM>,
}

impl<'a, MeshT, PosM> Writer<'a, MeshT, PosM, DummyMap>
where // TODO: remove once implied bounds land
    MeshT: Mesh + MeshUnsorted + ExplicitFace,
    PosM: VertexPropMap,
    PosM::Target: Pos3Like<Scalar = f32>,
{
    fn new(ser: Serializer, mesh: &'a MeshT, vertex_positions: &'a PosM) -> Self {
        Self {
            ser,
            mesh,
            vertex_positions,
            face_normals: None,
        }
    }

    pub fn with_face_normals<NormalM>(
        self,
        face_normals: &'a NormalM,
    ) -> Writer<'a, MeshT, PosM, NormalM>
    where
        NormalM: FacePropMap,
        NormalM::Target: Vec3Like<Scalar = f32>,
    {
        Writer {
            ser: self.ser,
            mesh: self.mesh,
            vertex_positions: self.vertex_positions,
            face_normals: Some(face_normals),
        }
    }
}


impl<MeshT, PosM, NormalM> MeshWriter for Writer<'_, MeshT, PosM, NormalM>
where // TODO: remove once implied bounds land
    MeshT: Mesh + MeshUnsorted + ExplicitFace,
    PosM: VertexPropMap,
    PosM::Target: Pos3Like<Scalar = f32>,
    NormalM: FacePropMap,
    NormalM::Target: Vec3Like<Scalar = f32>,
{
    type Error = Error;

    fn write_to(&self, mut w: impl Write) -> Result<(), Self::Error> {
        // Retrieves the positions and normal of the given face. Captures
        // `self`.
        let get_pos_and_normal = |face_handle| -> ([[f32; 3]; 3], [f32; 3]) {
            let [va, vb, vc] = self.mesh.vertices_of_face(face_handle);

            // Get positions from map and convert them to array
            let get = |h| -> [f32; 3] {
                self.vertex_positions
                    .get(h)
                    .unwrap_or_else(|| panic!("no position for {:?} in map while writing STL", h))
                    .convert()
            };
            let positions = [get(va), get(vb), get(vc)];

            // If a normal map was specified, retrieve the normal from there,
            // otherwise calculate it via cross product.
            let normal = if let Some(normals) = self.face_normals {
                normals.get(face_handle)
                    .unwrap_or_else(|| panic!(
                        "no normal for {:?} in map while writing STL",
                        face_handle,
                    ))
                    .convert()
            } else {
                let pos_a = positions[0].to_point3();
                let pos_b = positions[1].to_point3();
                let pos_c = positions[2].to_point3();

                let normal = (pos_b - pos_a).cross(pos_c - pos_a).normalize();
                [normal.x, normal.y, normal.z]
            };

            (positions, normal)
        };

        if self.ser.format == Format::Ascii {
            // ===============================================================
            // ===== STL ASCII
            // ===============================================================
            writeln!(w, "solid {}", self.ser.solid_name)?;

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

            writeln!(w, "endsolid {}", self.ser.solid_name)?;
        } else {
            // ===============================================================
            // ===== STL binary
            // ===============================================================
            // First, a 80 bytes useless header that must not begin with
            // "solid"!
            let signature = b"LOX   ... PLY specs are terrible ...";
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
                // do that.
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
fn write_ascii_vector(w: &mut impl Write, [x, y, z]: [f32; 3]) -> Result<(), io::Error> {
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
/// About the actual format: clearly unhelpful. In real world, STL files floats
/// are encoded all over the place. I've seen `1`, `1.2`, `10.2`, `1.02e1`,
/// `1.020000E+001` and more. We just stick to the exact format mentioned in
/// the "specification". This does not necessarily make any sense and wastes
/// memory, but so does ASCII STL. Just don't use the ASCII STL format!
fn write_ascii_f32(w: &mut impl Write, v: f32) -> Result<(), io::Error> {
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
