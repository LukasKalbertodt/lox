use criterion::black_box;

use lox::{
    prelude::*,
    io::{
        Error,
        ply::{
            raw::{ElementDef, RawElement, RawSink, RawSource, Serializer},
        },
    },
};


/// A raw sink that just puts all data into the `black_box`.
pub struct NullRawSink;

impl RawSink for NullRawSink {
    fn element_group_start(&mut self, def: &ElementDef) -> Result<(), Error> {
        black_box(def);
        Ok(())
    }
    fn element(&mut self, elem: &RawElement) -> Result<(), Error> {
        black_box(elem);
        Ok(())
    }
}

pub struct RawSphereSource {
    vertex_data: Vec<[f32; 3]>,
    face_data: Vec<[u32; 3]>,
}

impl RawSphereSource {
    pub fn new() -> Self {
        let m = super::sphere();
        let vertex_data = m.mesh.vertex_handles()
            .map(|vh| m.vertex_positions[vh].convert())
            .collect();

        let face_data = m.mesh.face_handles()
            .map(|fh| m.mesh.vertices_around_triangle(fh).map(|vh| vh.idx() as u32))
            .collect();

        Self { vertex_data, face_data }
    }

    pub fn vertex_count(&self) -> u64 {
        self.vertex_data.len() as u64
    }

    pub fn face_count(&self) -> u64 {
        self.face_data.len() as u64
    }
}

impl RawSource for &RawSphereSource {
    fn serialize_into<S: Serializer>(self, mut ser: S) -> Result<(), Error> {
        for mut slice in self.vertex_data.iter().copied() {
            ser.add_slice(&mut slice)?;
            ser.end_element()?;
        }

        for mut slice in self.face_data.iter().copied() {
            ser.add::<u8>(3)?;
            ser.add_slice(&mut slice)?;
            ser.end_element()?;
        }

        Ok(())
    }
}

pub struct RawSphereVNormalsSource {
    vertex_data: Vec<[f32; 6]>,
    face_data: Vec<[u32; 3]>,
}

impl RawSphereVNormalsSource {
    pub fn new() -> Self {
        let m = super::sphere_vnormals();
        let vertex_data = m.mesh.vertex_handles()
            .map(|vh| {
                let pos = m.vertex_positions[vh];
                let normal = m.vertex_normals[vh];
                [pos.x, pos.y, pos.z, normal.x, normal.y, normal.z]
            })
            .collect();

        let face_data = m.mesh.face_handles()
            .map(|fh| m.mesh.vertices_around_triangle(fh).map(|vh| vh.idx() as u32))
            .collect();

        Self { vertex_data, face_data }
    }

    pub fn vertex_count(&self) -> u64 {
        self.vertex_data.len() as u64
    }

    pub fn face_count(&self) -> u64 {
        self.face_data.len() as u64
    }
}

impl RawSource for &RawSphereVNormalsSource {
    fn serialize_into<S: Serializer>(self, mut ser: S) -> Result<(), Error> {
        for mut slice in self.vertex_data.iter().copied() {
            ser.add_slice(&mut slice)?;
            ser.end_element()?;
        }

        for mut slice in self.face_data.iter().copied() {
            ser.add::<u8>(3)?;
            ser.add_slice(&mut slice)?;
            ser.end_element()?;
        }

        Ok(())
    }
}
