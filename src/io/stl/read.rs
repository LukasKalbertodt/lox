use std::{
    io,
};

use crate::{
    io::{
        parse::{
            self, Input,
            buf::{Buffer},
        },
    },
};


#[derive(Debug)]
pub struct Reader<R: io::Read> {
    reader: R,
}


impl<R: io::Read> Reader<R> {
    pub fn new(reader: R) -> Self {
        Self { reader }
    }

    pub fn read_raw(self, sink: &mut impl Sink) -> Result<(), parse::Error> {
        fn is_ascii(input: &mut impl Input) -> Result<bool, parse::Error> {
            input.with_bytes(5, |b| Ok(b == b"solid"))
        }

        let mut buf = Buffer::new(self.reader)?;

        if is_ascii(&mut buf)? {
            // ===== ASCII =======================================================
            println!("ASCII");
            unimplemented!()

        } else {
            // ===== BINARY ======================================================
            // Skip the rest of the 80 byte header. 5 bytes were already
            // consumed in `is_ascci`.
            buf.skip(75)?;

            // Stored next is the number of triangles.
            let num_triangles = parse::u32_le(&mut buf)?;
            sink.num_triangles(num_triangles);

            // We attempt to read as many triangles as specified. If the
            // specified number was too high and we reach EOF early, we will
            // return an error.
            for _ in 0..num_triangles {
                /// Reads three consecutive `f32`s.
                fn read_vec3(input: &mut impl Input) -> Result<[f32; 3], parse::Error> {
                    let x = parse::f32_le(input)?;
                    let y = parse::f32_le(input)?;
                    let z = parse::f32_le(input)?;

                    Ok([x, y, z])
                }

                // Read the normal and all three vertex positions.
                let normal = read_vec3(&mut buf)?;
                let a = read_vec3(&mut buf)?;
                let b = read_vec3(&mut buf)?;
                let c = read_vec3(&mut buf)?;
                let attribute_byte_count = parse::u16_le(&mut buf)?;


                sink.add_triangle(Triangle {
                    normal,
                    vertices: [a, b, c],
                    attribute_byte_count,
                });
            }

            buf.assert_eof()?;
        }

        Ok(())
    }
}

/// A sink can accept data from an STL file. This is mainly used for
/// [`Reader::read_raw`].
pub trait Sink {
    /// Is called once in the beginning after reading the number of triangles
    /// from the file.
    fn num_triangles(&mut self, num: u32);

    /// Is called for each triangle that is read from the file.
    fn add_triangle(&mut self, triangle: Triangle);
}

/// Convenience struct which holds two closures and implements [`Sink`].
#[derive(Debug)]
pub struct AdhocSink<F, G>
where
    F: FnMut(u32),
    G: FnMut(Triangle),
{
    pub num_triangles: F,
    pub add_triangle: G,
}

impl<F, G> Sink for AdhocSink<F, G>
where
    F: FnMut(u32),
    G: FnMut(Triangle),
{
    fn num_triangles(&mut self, num: u32) {
        (self.num_triangles)(num);
    }
    fn add_triangle(&mut self, triangle: Triangle) {
        (self.add_triangle)(triangle);
    }
}

/// One triangle in an STL file.
#[derive(Clone, Debug)]
pub struct Triangle {
    /// Face normal.
    normal: [f32; 3],

    /// The 3D positions of the vertices in CCW order (that is, when looking at
    /// the face "from the outside").
    vertices: [[f32; 3]; 3],

    /// No one understands what this does. It's usually zero. Sometimes it's
    /// abused to store a 16bit color. You shouldn't do that.
    attribute_byte_count: u16,
}
