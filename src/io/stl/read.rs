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


macro_rules! parser {
    ($name:ident = |$buf:ident| $body:expr) => {
        parser!($name = |$buf| -> () { $body })
    };
    ($name:ident = |$buf:ident| -> $out:ty $body:block) => {
        fn $name($buf: &mut impl Input) -> Result<$out, parse::Error> {
            $body
        }
    };
}

impl<R: io::Read> Reader<R> {
    pub fn new(reader: R) -> Self {
        Self { reader }
    }

    pub fn read_into_raw(self) -> Result<RawResult, parse::Error> {
        let mut out = RawResult::new();
        self.read_raw(&mut out)?;
        Ok(out)
    }

    pub fn read_raw(self, sink: &mut impl Sink) -> Result<(), parse::Error> {
        parser!(is_ascii = |buf| -> bool { buf.with_bytes(5, |b| Ok(b.data == b"solid")) });

        let mut buf = Buffer::new(self.reader)?;

        if is_ascii(&mut buf)? {
            // ===== ASCII =======================================================
            parser!(opt_whitespace = |buf| buf.skip_until(|b| b != b' '));
            parser!(whitespace = |buf| {
                buf.expect_tag(b" ")?;
                opt_whitespace(buf)?;
                Ok(())
            });
            parser!(linebreak = |buf| {
                opt_whitespace(buf)?;
                buf.expect_tag(b"\n")?;
                opt_whitespace(buf)?;
                Ok(())
            });
            parser!(float = |buf| -> f32 {
                buf.take_until(
                    |b| b == b' ' || b == b'\n',
                    |sd| sd.assert_ascii()?
                        .parse::<f32>()
                        .map_err(|e| sd.error(format!("invalid float literal: {}", e)))
                )
            });
            parser!(vec3 = |buf| -> [f32; 3] {
                let x = float(buf)?;
                whitespace(buf)?;
                let y = float(buf)?;
                whitespace(buf)?;
                let z = float(buf)?;
                Ok([x, y, z])
            });
            parser!(vertex = |buf| -> [f32; 3] {
                line(buf, |buf| {
                    buf.expect_tag(b"vertex")?;
                    whitespace(buf)?;
                    vec3(buf)
                })
            });

            fn line<I, F, O>(buf: &mut I, func: F) -> Result<O, parse::Error>
            where
                I: Input,
                F: FnOnce(&mut I) -> Result<O, parse::Error>,
            {
                opt_whitespace(buf)?;
                let out = func(buf)?;
                linebreak(buf)?;
                Ok(out)
            }

            // Parse the header (just specifies the name of the solid)
            whitespace(&mut buf)?;
            let solid_name = buf.take_until(
                |b| b == b' ' || b == b'\n',
                |sd| sd.assert_ascii().map(|name| name.to_string()),
            )?;
            linebreak(&mut buf)?;
            sink.meta(Meta::Ascii(solid_name.clone()));


            // Parse facets
            loop {
                // First line (`facet normal 0.0 1.0 0.0`)
                let normal = line(&mut buf, |buf| {
                    buf.expect_tag(b"facet normal")?;
                    whitespace(buf)?;
                    vec3(buf)
                })?;

                // Parse vertices
                line(&mut buf, |buf| buf.expect_tag(b"outer loop"))?;
                let vertices = [
                    vertex(&mut buf)?,
                    vertex(&mut buf)?,
                    vertex(&mut buf)?,
                ];
                line(&mut buf, |buf| buf.expect_tag(b"endloop"))?;

                // Pass parsed triangle to sink
                sink.add_triangle(Triangle {
                    normal,
                    vertices,
                    attribute_byte_count: 0,
                });

                // Parse last line (`endfacet`)
                line(&mut buf, |buf| buf.expect_tag(b"endfacet"))?;

                // Check if the next line starts with `endsolid` and break loop
                // in that case.
                opt_whitespace(&mut buf)?;
                buf.prepare(b"endsolid".len())?;
                if buf.starts_with(b"endsolid") {
                    // We've seen `endsolid`: we just stop here. There could be
                    // junk afterwards, but we don't care.
                    break;
                }
            }
        } else {
            // ===== BINARY ======================================================
            // Skip the rest of the 80 byte header. 5 bytes were already
            // consumed in `is_ascci`.
            buf.skip(75)?;

            // Stored next is the number of triangles.
            let num_triangles = parse::u32_le(&mut buf)?;
            sink.meta(Meta::Binary(num_triangles));

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
    /// Is called once in the beginning after reading the header from the file.
    fn meta(&mut self, meta: Meta);

    /// Is called for each triangle that is read from the file.
    fn add_triangle(&mut self, triangle: Triangle);
}

/// Convenience struct which holds two closures and implements [`Sink`].
#[derive(Debug)]
pub struct AdhocSink<F, G>
where
    F: FnMut(Meta),
    G: FnMut(Triangle),
{
    pub meta: F,
    pub add_triangle: G,
}

impl<F, G> Sink for AdhocSink<F, G>
where
    F: FnMut(Meta),
    G: FnMut(Triangle),
{
    fn meta(&mut self, meta: Meta) {
        (self.meta)(meta);
    }
    fn add_triangle(&mut self, triangle: Triangle) {
        (self.add_triangle)(triangle);
    }
}

#[derive(Debug)]
pub enum Meta {
    /// For ASCII STL files, a name for the solid is stored in the file.
    Ascii(String),

    /// For binary STL files, the number of triangles is stored inside.
    Binary(u32),
}

/// One triangle in an STL file.
#[derive(Clone, Debug)]
pub struct Triangle {
    /// Face normal.
    normal: [f32; 3],

    /// The 3D positions of the vertices in CCW order (that is, when looking at
    /// the face "from the outside").
    vertices: [[f32; 3]; 3],

    /// No one understands what this does. It's only stored in binary format
    /// and is usually zero. Sometimes it's abused to store a 16bit color. You
    /// shouldn't do that.
    ///
    /// If an ASCII file is parsed, this is just set to 0 (despite it being not
    /// store in the file).
    attribute_byte_count: u16,
}

#[derive(Debug)]
pub struct RawResult {
    pub solid_name: Option<String>,
    pub triangles: Vec<Triangle>,
}

impl RawResult {
    fn new() -> Self {
        Self {
            solid_name: None,
            triangles: Vec::new(),
        }
    }
}

impl Sink for RawResult {
    fn meta(&mut self, meta: Meta) {
        match meta {
            Meta::Binary(num_triangles) => self.triangles.reserve(num_triangles as usize),
            Meta::Ascii(name) => self.solid_name = Some(name),
        }
    }
    fn add_triangle(&mut self, triangle: Triangle) {
        self.triangles.push(triangle);
    }
}
