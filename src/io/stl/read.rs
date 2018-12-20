use std::{
    fmt,
    fs::File,
    hash::{Hash, Hasher},
    marker::PhantomData,
    io,
    path::Path,
};

use byteorder::{LittleEndian, ReadBytesExt};
use hashbrown::{HashMap, hash_map::Entry};

use crate::{
    prelude::*,
    io::{
        StreamingSource, MemSink,
        parse::{
            self, Input,
            buf::{Buffer},
        },
    },
};
use super::{Encoding, Error};



// ===========================================================================
// ===== Parsing functions
// ===========================================================================
macro_rules! parser {
    ($name:ident = |$buf:ident| $body:expr) => {
        parser!($name = |$buf| -> () { $body });
    };
    ($name:ident = |$buf:ident| -> $out:ty $body:block) => {
        fn $name($buf: &mut impl Input) -> Result<$out, parse::Error> {
            $body
        }
    };
}

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

/// Parses one ASCII float via `<f32 as FromStr>::parse`. There must
/// not be leading whitespace and the float literal has to end with ' '
/// or '\n'.
parser!(float = |buf| -> f32 {
    buf.take_until(
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


// ===========================================================================
// ===== Definition of `Reader`
// ===========================================================================

/// A reader able to read binary and ASCII STL files.
///
/// You can create a reader with [`Reader::open`] or [`Reader::new`]. TODO: how
/// to actually read data.
///
/// The type parameter `U` is just used to configure the vertex-unifying
/// behavior at compile time. See TODO add method
pub struct Reader<R: io::Read + io::Seek, U: UnifyingMarker = UnifyVertices> {
    buf: Buffer<R>,
    solid_name: Option<String>,
    triangle_count: Option<u32>,
    _dummy: PhantomData<U>,
}

impl Reader<File, UnifyVertices> {
    /// Tries to open the file specified by the given path and creates a new
    /// `Reader` from that file.
    pub fn open(path: impl AsRef<Path>) -> Result<Self, Error> {
        // We don't need a `BufReader` here, because we will use our internal
        // parse buffer anyway.
        Self::new(File::open(path)?)
    }
}

impl<R: io::Read + io::Seek> Reader<R, UnifyVertices> {
    /// Creates a new `Reader` from the given `io::Read` instance and parses
    /// the header of the given input.
    ///
    /// If you want to open a file, rather use [`Reader::open`].
    pub fn new(mut reader: R) -> Result<Self, Error> {
        // Determine length of input.
        let input_len = reader.seek(io::SeekFrom::End(0))?;

        // Pretend the file is binary and read the number of triangles at
        // offset 80 (if the file is even long enough).
        let num_tris_if_binary = if input_len >= 84 {
            reader.seek(io::SeekFrom::Start(80))?;
            Some(reader.read_u32::<LittleEndian>()?)
        } else {
            None
        };

        // Jump back to the start of the stream/file
        reader.seek(io::SeekFrom::Start(0))?;

        // Wrap reader into parse buffer.
        let mut buf = Buffer::new(reader)?;

        // Load the first 1K bytes (or the whole file, if the file is shorter
        // than that). We want to inspect those bytes.
        buf.saturating_prepare(1024)?;

        // We need to figure out if this file is binary or ASCII. This is
        // actually harder than it sounds because there is no clear metric.
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
        // - If the file doesn't start with `solid`, it's binary.
        // - If there are some non-ASCII (>127) bytes at the beginning of the
        //   file, the file is binary.
        // - If the expected file length calculated from the triangle count at
        //   offset 80 (only stored if the file is binary) matches the real
        //   file length, we assume it's a binary file. But if there is no such
        //   count (because the file is shorter than 84 bytes), the file is not
        //   binary.
        let is_binary = !buf.starts_with(b"solid")
            || buf.iter().take(1024).any(|b| !b.is_ascii())
            || num_tris_if_binary.map(|num_tris_if_binary| {
                // In binary format, each triangle is stored with 50 bytes
                // (3 * 3 = 9 position floats => 36 bytes, 3 normal floats
                // => 12 bytes, 2 bytes "attribute byte count"). The binary
                // header is 84 bytes long.
                let expected_len_if_binary = num_tris_if_binary as u64 * 50 + 84;
                expected_len_if_binary == input_len
            }).unwrap_or(false);

        // Check if the file starts with `solid`. If yes, a string (the solid
        // name) is stored next.
        let solid_name = if buf.is_next(b"solid")? {
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
                let name = buf.take_until(b'\n', |sd| {
                    sd.assert_ascii().map(|name| name.trim().to_string())
                })?;
                linebreak(&mut buf)?;
                name
            };

            Some(solid_name)
        } else {
            None
        };

        // In the binary case, we still need to skip the remaining header.
        if is_binary {
            buf.skip(84 - buf.offset())?;
        }

        Ok(Self {
            buf,
            solid_name,
            triangle_count: num_tris_if_binary.filter(|_| is_binary),
            _dummy: PhantomData,
        })
    }


    /// Configures the reader to not unify vertices with the exact same
    /// position into one.
    ///
    /// An STL file is a simple list of triangles. Each triangle specifies the
    /// position of its three vertices. This means that vertices of adjacent
    /// triangles are stored once per triangle. When reading the file, we only
    /// know the vertex positions and have no idea which vertices are actually
    /// the same one and which are two different vertices that have the same
    /// position.
    ///
    /// It's common to unify vertices when reading an STL file to get a real
    /// mesh and not just a collection of unconnected triangles. You only need
    /// to disable unification in very special cases, mainly because:
    /// - Your mesh has vertices that have the exact same position but should
    ///   be treated as separate vertices (this is very rare)
    /// - Unifying the vertices is too slow for you (unifying makes the whole
    ///   read process around 2.5 times slower)
    ///
    /// But if any of this is a problem for you, you should rather use a better
    /// file format instead of STL.
    ///
    /// When vertices are unified, `NaN` values in vertices are not allowed. So
    /// in that case, if your file contains `NaN` values, the reading method
    /// will panic.
    pub fn without_vertex_unification(self) -> Reader<R, VerbatimVertices> {
        Reader {
            buf: self.buf,
            solid_name: self.solid_name,
            triangle_count: self.triangle_count,
            _dummy: PhantomData,
        }
    }
}

impl<R: io::Read + io::Seek, U: UnifyingMarker> Reader<R, U> {
    /// Returns the name of the solid. If no solid name was stored in the file,
    /// `None` is returned.
    pub fn solid_name(&self) -> Option<&str> {
        self.solid_name.as_ref().map(|s| s.as_str())
    }

    /// Returns whether or not the file is a binary STL file (as opposed to
    /// ASCII).
    pub fn is_binary(&self) -> bool {
        self.triangle_count.is_some()
    }

    /// Returns the encoding of this STL file.
    pub fn encoding(&self) -> Encoding {
        if self.is_binary() {
            Encoding::Binary
        } else {
            Encoding::Ascii
        }
    }

    /// Returns the triangle count stored in the file. That number is stored if
    /// and only if the file is binary.
    pub fn triangle_count(&self) -> Option<u32> {
        self.triangle_count
    }

    /// Reads the whole file into a [`RawResult`].
    ///
    /// Usually you either want to use a higher level function (like TODO) or
    /// [`Reader::read_raw_into`]. The latter is the streaming version of this
    /// method which doesn't require a temporary storage ([`RawResult`]).
    pub fn into_raw_result(self) -> Result<RawResult, Error> {
        let mut out = RawResult::new();
        self.read_raw_into(&mut out)?;
        Ok(out)
    }

    /// Reads the whole file into the given raw sink.
    ///
    /// This is a low level building block that you usually don't want to use
    /// directly. TODO: High level? In particular, this method itself never
    /// performs any vertex unification (regardless of the type parameter `U`).
    pub fn read_raw_into<S: RawSink>(
        self,
        sink: &mut S,
    ) -> Result<(), Error> {
        let mut buf = self.buf;

        // Forward metadata to the sink
        if let Some(solid_name) = self.solid_name {
            sink.solid_name(solid_name);
        }

        // ===== Parse body ==================================================
        if let Some(triangle_count) = self.triangle_count {
            // ===== BINARY ==================================================
            sink.triangle_count(triangle_count);

            // We attempt to read as many triangles as specified. If the
            // specified number was too high and we reach EOF early, we will
            // return an error.
            for _ in 0..triangle_count {
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

                    Ok(RawTriangle {
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
        } else {
            // ===== ASCII ===================================================
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
                sink.triangle(RawTriangle {
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
        }

        Ok(())
    }
}

impl<R: io::Read + io::Seek, U: UnifyingMarker> fmt::Debug for Reader<R, U> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Reader")
            .field("buf", &self.buf)
            .field("solid_name", &self.solid_name)
            .field("is_binary", &self.triangle_count.is_some())
            .field("triangle_count", &self.triangle_count)
            .finish()
    }
}


// ===========================================================================
// ===== Definition of `RawSink` and some sinks
// ===========================================================================

/// A sink can accept raw data from an STL file. This is mainly used for
/// [`Reader::read_raw_into`].
pub trait RawSink {
    /// Is called once in the beginning if the file starts with `solid`.
    ///
    /// If the file is guessed to be binary, the `name` is the 75 character
    /// header after `solid` with whitespace trimmed from both sites. If the
    /// file is ASCII, `name` is the first line (without `solid`). If the file
    /// doesn't start with `solid`, this method is not called.
    fn solid_name(&mut self, name: String);

    /// If the file is binary, this method is called once in the beginning. The
    /// number of triangles as stored in the file is passed into this method.
    fn triangle_count(&mut self, num: u32);

    /// Is called for each triangle that is read from the file.
    fn triangle(&mut self, triangle: RawTriangle);
}

/// One raw triangle in an STL file.
///
/// This type is used in [`RawResult`] and [`RawSink`]. If you don't use the low
/// level `raw` methods, you probably don't care about this type.
#[derive(Debug, Clone, Copy)]
pub struct RawTriangle {
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

/// Holds the raw data from an STL file.
///
/// To obtain a `RawResult`, call [`Reader::into_raw_result`]. See its
/// documentation for more information.
#[derive(Debug, Clone)]
pub struct RawResult {
    /// The solid name if it's specified in the file.
    pub solid_name: Option<String>,

    /// All triangles from the file.
    pub triangles: Vec<RawTriangle>,
}

impl RawResult {
    /// Creates an instance with no name and no triangles.
    pub fn new() -> Self {
        Self {
            solid_name: None,
            triangles: Vec::new(),
        }
    }
}

/// For convenience, you can use [`Reader::into_raw_result`] instead of
/// [`Reader::read_raw_into`] with `RawResult`.
impl RawSink for RawResult {
    fn solid_name(&mut self, name: String) {
        self.solid_name = Some(name);
    }

    fn triangle_count(&mut self, num: u32) {
        self.triangles.reserve(num as usize);
    }

    fn triangle(&mut self, triangle: RawTriangle) {
        self.triangles.push(triangle);
    }
}

/// A simple wrapper around a closure which implements [`RawSink`].
///
/// The closure is used to implement the `triangle` method of the `RawSink` trait.
/// The methods `solid_name` and `triangle_count` just don't do anything. This
/// is just a quick way to create a sink.
///
/// Since the closure can return an error, you usually have to annotate the
/// error type `!` manually if you are not actually returning an error.
#[derive(Debug)]
pub struct FnRawSink<F>(pub F);

impl<F: FnMut(RawTriangle)> RawSink for FnRawSink<F> {
    fn solid_name(&mut self, _: String) {}
    fn triangle_count(&mut self, _: u32) {}

    fn triangle(&mut self, triangle: RawTriangle) {
        (self.0)(triangle)
    }
}


// ===========================================================================
// ===== Definition of unifying dummy types. Not actually public.
// ===========================================================================
pub trait UnifyingMarker {
    type Adder: VertexAdder;
}

#[derive(Debug)]
pub enum UnifyVertices {}

impl UnifyingMarker for UnifyVertices {
    type Adder = UnifyingAdder;
}

#[derive(Debug)]
pub enum VerbatimVertices {}

impl UnifyingMarker for VerbatimVertices {
    type Adder = NonUnifyingAdder;
}

// ===========================================================================
// ===== VertexAdders: unify vertices or not. Not actually public.
// ===========================================================================
pub trait VertexAdder {
    fn new() -> Self;
    fn size_hint(&mut self, _num_vertices: u32) {}
    fn add_vertex<S: MemSink>(
        &mut self,
        sink: &mut S,
        pos: [f32; 3],
    ) -> VertexHandle;
}

/// Adds every incoming vertex as new unique vertex. No unifying is done.
#[derive(Debug)]
pub struct NonUnifyingAdder;
impl VertexAdder for NonUnifyingAdder {
    fn new() -> Self {
        NonUnifyingAdder
    }

    fn add_vertex<S: MemSink>(
        &mut self,
        sink: &mut S,
        pos: [f32; 3],
    ) -> VertexHandle {
        let handle = sink.add_vertex();
        sink.set_vertex_position(handle, pos.to_point3());
        handle
    }
}

/// The key of the hashmap: three `f32`. Implementing this like this is
/// faster than using `[OrderedFloat<f32>; 3]`. The values inside must
/// not be NaN, because those values won't be normalized (i.e. there
/// are 2^23 different NaN values) which will confuse the hash map.
#[derive(Debug, PartialEq)]
struct PosKey([f32; 3]);

impl Eq for PosKey  {}
impl Hash for PosKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0[0].to_bits().hash(state);
        self.0[1].to_bits().hash(state);
        self.0[2].to_bits().hash(state);
    }
}

/// Unifies incoming vertices with the exact same position into a single one.
#[derive(Debug)]
pub struct UnifyingAdder(HashMap<PosKey, VertexHandle>);
impl VertexAdder for UnifyingAdder {
    fn new() -> Self {
        UnifyingAdder(HashMap::default())
    }

    fn size_hint(&mut self, num_vertices: u32) {
        self.0.reserve(num_vertices as usize);
    }

    fn add_vertex<S: MemSink>(
        &mut self,
        sink: &mut S,
        pos: [f32; 3],
    ) -> VertexHandle {
        // Make sure the positions are not `NaN`. This assert apparently
        // doesn't have a measurable effect on execution speed, so it's
        // `assert` and not `debug_assert!`.
        assert!(
            !(pos[0].is_nan() || pos[1].is_nan() || pos[2].is_nan()),
            "attempt to read STL file with vertex position containing NaN value",
        );

        let handle = match self.0.entry(PosKey(pos)) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let handle = sink.add_vertex();
                sink.set_vertex_position(handle, pos.to_point3());
                entry.insert(handle);
                handle
            }
        };

        handle
    }
}


impl<R, U> StreamingSource for Reader<R, U>
where
    R: io::Read + io::Seek,
    U: UnifyingMarker,
{
    type Error = Error;

    fn transfer_to<S: MemSink>(self, sink: &mut S) -> Result<(), Self::Error> {
        struct HelperSink<'a, S: MemSink, A: VertexAdder> {
            sink: &'a mut S,
            vertex_adder: A,
        };

        impl<S: MemSink, A: VertexAdder> RawSink for HelperSink<'_, S, A> {
            fn solid_name(&mut self, _: String) {}
            fn triangle_count(&mut self, _num: u32) {} // TODO

            fn triangle(&mut self, triangle: RawTriangle) {
                let [pa, pb, pc] = triangle.vertices;
                let a = self.vertex_adder.add_vertex(self.sink, pa);
                let b = self.vertex_adder.add_vertex(self.sink, pb);
                let c = self.vertex_adder.add_vertex(self.sink, pc);

                self.sink.add_face([a, b, c]);
            }
        }

        // Read into helper sink
        self.read_raw_into(&mut HelperSink {
            sink,
            vertex_adder: U::Adder::new(),
        })
    }
}
