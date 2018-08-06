use std::{
    collections::HashMap,
    fs::File,
    io,
    path::Path,
};

use ordered_float::OrderedFloat;

// use nom::{
//     named, le_f32, le_u32, do_parse, call, take, alt, tag, error_position,
//     Needed,
// };
use fev_core::{
    TriMeshSource, TriMeshBuilder,
    handle::{VertexHandle},
    prop,
};

use crate::{
    MeshReader,
    parse::{
        self, Input,
        buf::{Buffer},
    },
};

pub struct StlReader<R: io::Read> {
    reader: R,
}

impl StlReader<File> {
    pub fn open(path: impl AsRef<Path>) -> Result<Self, io::Error> {
        // TODO: or wrap it into buf reader?
        Ok(Self::from_reader(File::open(path)?))
    }
}

impl<R: io::Read> StlReader<R> {
    pub fn from_reader(reader: R) -> Self {
        Self { reader }
    }
}

impl<R: io::Read> TriMeshSource for StlReader<R> {
    type Err = parse::Error;
    type VertexInfo = prop::Position<[f32; 3]>;
    type FaceInfo = prop::Normal<[f32; 3]>;

    fn append(
        self,
        builder: &mut impl TriMeshBuilder<Self::VertexInfo, Self::FaceInfo>,
    ) -> Result<(), Self::Err> {
        let mut b = Buffer::new(self.reader)?;

        let is_ascii = is_ascii(&mut b)?;

        if is_ascii {
            // ===== ASCII =======================================================
            println!("ASCII");
            unimplemented!()

        } else {
            // ===== BINARY ======================================================
            skip_header(&mut b)?;
            let num_triangles = read_num_triangles(&mut b)?;
            builder.hint_num_faces(num_triangles as usize);

            let mut vertices = HashMap::new();

            for _ in 0..num_triangles {
                let triangle = read_triangle(&mut b)?;

                let mut handles = [VertexHandle(0); 3];

                for i in 0..3 {
                    let pos = triangle.vertices[i];

                    let key = [
                        OrderedFloat(pos[0]),
                        OrderedFloat(pos[1]),
                        OrderedFloat(pos[2]),
                    ];

                    handles[i] = *vertices.entry(key)
                        .or_insert_with(|| builder.add_vertex(prop::Position(pos)));
                }

                builder.add_face(handles, prop::Normal(triangle.normal));
            }
        }

        Ok(())
    }
}


fn is_ascii(input: &mut impl Input) -> Result<bool, parse::Error> {
    input.with_bytes(5, |b| Ok(b == b"solid"))
}

fn skip_header(input: &mut impl Input) -> Result<(), parse::Error> {
    input.skip(75)
}

fn read_num_triangles(input: &mut impl Input) -> Result<u32, parse::Error> {
    parse::u32_le(input)
}

fn read_vec3(input: &mut impl Input) -> Result<[f32; 3], parse::Error> {
    let x = parse::f32_le(input)?;
    let y = parse::f32_le(input)?;
    let z = parse::f32_le(input)?;

    Ok([x, y, z])
}

fn read_triangle(input: &mut impl Input) -> Result<Triangle, parse::Error> {
    let normal = read_vec3(input)?;
    let a = read_vec3(input)?;
    let b = read_vec3(input)?;
    let c = read_vec3(input)?;
    input.skip(2)?;

    Ok(Triangle {
        normal,
        vertices: [a, b, c],
    })
}

#[derive(Debug)]
struct Triangle {
    normal: [f32; 3],
    vertices: [[f32; 3]; 3],
}

#[cfg(test)]
mod tests {
    #[test]
    fn empty() {
        // TODO
    }
}
