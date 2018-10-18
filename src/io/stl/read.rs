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

    /// Reads the whole file into a [`RawResult`].
    ///
    /// Usually you either want to use a higher level function (TODO: link) or
    /// [`read_raw_into`]. The latter is the streaming version of this method
    /// which doesn't require a temporary storage ([`RawResult`]).
    pub fn read_raw(self) -> Result<RawResult, parse::Error> {
        let mut out = RawResult::new();
        self.read_raw_into(&mut out)?;
        Ok(out)
    }

    /// Reads the whole file into the given sink.
    ///
    /// This is a low level building block that you usually don't want to use
    /// directly. TODO: mention higher level methods
    pub fn read_raw_into(self, sink: &mut impl Sink) -> Result<(), parse::Error> {
        // Optionally skip whitespace
        parser!(opt_whitespace = |buf| buf.skip_until(|b| b != b' '));

        // Requires at least one whitespace, skips all whitespace that follows
        // it.
        parser!(whitespace = |buf| {
            buf.expect_tag(b" ")?;
            opt_whitespace(buf)?;
            Ok(())
        });

        /// Requires a '\n' linebreak with optional whitespace before and after
        /// it.
        parser!(linebreak = |buf| {
            opt_whitespace(buf)?;
            buf.expect_tag(b"\n")?;
            opt_whitespace(buf)?;
            Ok(())
        });

        /// Eats optional whitespace, calls the passed parser and requires a
        /// linebreak at the end.
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

        let mut buf = Buffer::new(self.reader)?;

        // Before reading the body, we need to figure out if this file is
        // binary or ASCII. This is actually harder than it sounds because
        // there is no clear metric.
        //
        // An ASCII file starts with `solid <name>` where `<name>` can be an
        // arbitrary string, even arbitrarily long. The name is delimited by a
        // newline.
        //
        // A binary file starts with a 80 byte header that has no significance
        // and may contain arbitrary ASCII data. Afterwards, a big binary blob
        // follows.
        //
        // We use the following metric:
        // - If there are some non-ASCII (>127) bytes at the beginning of the
        //   file, the file is binary.
        // - If the file doesn't start with `solid`, it's binary.
        //
        // One good test would be to read the number of triangles at offset 80
        // and check if the file size corresponds to that. But right now we
        // can't do that because all we have is a `Read`. (TODO)

        // Load the first 1K bytes (or the whole file, if the file is shorter
        // than that). We want to inspect those bytes.
        buf.saturating_prepare(1024)?;
        let is_binary = !buf.starts_with(b"solid") || buf.iter().any(|b| !b.is_ascii());

        // Check if the file starts with `solid`. If yes, a string (the solid
        // name) is stored next. It's also a strong indicator, that the file is
        // ASCII.
        if buf.is_next(b"solid")? {
            // Consume `solid`
            buf.consume(5);

            // Read the solid name (until line break in ASCII case, 80 chars in
            // binary case).
            whitespace(&mut buf)?;
            let solid_name = if is_binary {
                buf.with_bytes(
                    80 - buf.offset(),
                    |sd| sd.assert_ascii().map(|name| name.trim().to_string()),
                )?
            } else {
                let name = buf.take_until(
                    1024, // We won't allow names longer than 1KB
                    |b| b == b'\n',
                    |sd| sd.assert_ascii().map(|name| name.trim().to_string()),
                )?;
                linebreak(&mut buf)?;
                name
            };

            sink.solid_name(solid_name);
        }

        if !is_binary {
            // ===== ASCII =======================================================
            /// Parses one ASCII float via `<f32 as FromStr>::parse`. There must
            /// not be leading whitespace and the float literal has to end with ' '
            /// or '\n'.
            parser!(float = |buf| -> f32 {
                buf.take_until(
                    100, // every float as ASCII fits in 100 bytes
                    |b| b == b' ' || b == b'\n',
                    |sd| sd.assert_ascii()?
                        .parse::<f32>()
                        .map_err(|e| sd.error(format!("invalid float literal: {}", e)))
                )
            });

            /// Parses three floats separated by whitespace. No leading or trailing
            /// whitespace is handled.
            parser!(vec3 = |buf| -> [f32; 3] {
                let x = float(buf)?;
                whitespace(buf)?;
                let y = float(buf)?;
                whitespace(buf)?;
                let z = float(buf)?;
                Ok([x, y, z])
            });

            /// Parses one ASCII line with a vertex (e.g. `vertex 2.0 0.1  1`)
            parser!(vertex = |buf| -> [f32; 3] {
                line(buf, |buf| {
                    buf.expect_tag(b"vertex")?;
                    whitespace(buf)?;
                    vec3(buf)
                })
            });

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
                sink.triangle(Triangle {
                    normal,
                    vertices,
                    attribute_byte_count: 0,
                });

                // Parse last line (`endfacet`)
                line(&mut buf, |buf| buf.expect_tag(b"endfacet"))?;

                // Check if the next line starts with `endsolid` and break loop
                // in that case.
                opt_whitespace(&mut buf)?;
                if buf.is_next(b"endsolid")? {
                    // We've seen `endsolid`: we just stop here. There could be
                    // junk afterwards, but we don't care.
                    break;
                }
            }
        } else {
            // ===== BINARY ======================================================
            // Skip the rest of the 80 byte header. 5 bytes were already
            // consumed in `is_ascci`.
            buf.skip(80 - buf.offset())?;

            // Stored next is the number of triangles.
            let num_triangles = parse::u32_le(&mut buf)?;
            sink.num_triangles(num_triangles);

            // We attempt to read as many triangles as specified. If the
            // specified number was too high and we reach EOF early, we will
            // return an error.
            for _ in 0..num_triangles {
                let triangle = buf.with_bytes(4 * 3 * 4 + 2, |sd| {
                    use byteorder::{ByteOrder, LittleEndian};

                    /// Reads three consecutive `f32`s.
                    fn vec3(data: &[u8]) -> [f32; 3] {
                        [
                            LittleEndian::read_f32(&data[0..]),
                            LittleEndian::read_f32(&data[4..]),
                            LittleEndian::read_f32(&data[8..]),
                        ]
                    }

                    Ok(Triangle {
                        normal: vec3(&sd.data[0..]),
                        vertices: [
                            vec3(&sd.data[12..]),
                            vec3(&sd.data[24..]),
                            vec3(&sd.data[36..]),
                        ],
                        attribute_byte_count: LittleEndian::read_u16(&sd.data[48..]),
                    })
                })?;

                sink.triangle(triangle);
            }

            // If the specified number of triangles was too small and there is
            // still data left, we also error.
            buf.assert_eof()?;
        }

        Ok(())
    }
}

/// A sink can accept data from an STL file. This is mainly used for
/// [`Reader::read_raw_into`].
pub trait Sink {
    /// Is called once in the beginning if the file starts with `solid`.
    ///
    /// If the file is guessed to be binary, the `name` is the 75 character
    /// header after `solid` with whitespace trimmed from both sites. If the
    /// file is ASCII, `name` is the first line (without `solid`). If the file
    /// doesn't start with `solid`, this method is not called.
    fn solid_name(&mut self, name: String);

    /// If the file is binary, this method is called once in the beginning. The
    /// number of triangles as stored in the file is passed into this method.
    fn num_triangles(&mut self, num: u32);

    /// Is called for each triangle that is read from the file.
    fn triangle(&mut self, triangle: Triangle);
}

/// One triangle in an STL file.
#[derive(Clone, Debug)]
pub struct Triangle {
    /// Face normal.
    pub normal: [f32; 3],

    /// The 3D positions of the vertices in CCW order (that is, when looking at
    /// the face "from the outside").
    pub vertices: [[f32; 3]; 3],

    /// No one understands what this does. It's only stored in binary format
    /// and is usually zero. Sometimes it's abused to store a 16bit color. You
    /// shouldn't do that.
    ///
    /// If an ASCII file is parsed, this is just set to 0 (despite it being not
    /// stored in the file).
    pub attribute_byte_count: u16,
}

/// Holds the raw data from a STL file.
///
/// This implements [`Sink`], but instead of using [`Reader::read_raw_into`]
/// with this type, you can use [`Reader::read_raw`] which already does it for
/// you.
#[derive(Debug)]
pub struct RawResult {
    /// The solid name if it's specified in the file.
    pub solid_name: Option<String>,

    /// All triangles from the file.
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
    fn solid_name(&mut self, name: String) {
        self.solid_name = Some(name);
    }

    fn num_triangles(&mut self, num: u32) {
        self.triangles.reserve(num as usize);
    }

    fn triangle(&mut self, triangle: Triangle) {
        self.triangles.push(triangle);
    }
}

#[derive(Debug)]
pub struct CounterSink {
    /// The solid name if it's specified in the file.
    pub solid_name: Option<String>,

    /// The number of triangles in that file.
    pub triangle_count: u32,
}

impl CounterSink {
    /// Returns an instance with no name and 0 triangles.
    pub fn new() -> Self {
        Self {
            solid_name: None,
            triangle_count: 0,
        }
    }
}

impl Sink for CounterSink {
    fn solid_name(&mut self, name: String) {
        self.solid_name = Some(name);
    }

    fn num_triangles(&mut self, _: u32) {}

    fn triangle(&mut self, _: Triangle) {
        self.triangle_count += 1;
    }
}
