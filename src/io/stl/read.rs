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
        StreamSource, MemSink, Error,
        parse::{self, ParseBuf, Buffer},
    },
    util::MeshSizeHint,
};
use super::{Encoding, RawTriangle, RawResult};



// ===========================================================================
// ===== Definition of `Reader`
// ===========================================================================

/// A reader able to read binary and ASCII STL files. Implements
/// [`StreamSource`].
///
/// You can create a reader with [`Reader::open`] or [`Reader::new`]. Then, to
/// read mesh data,  you can either use the high level [`StreamSource`]
/// interface or a `raw_*` method.
///
/// The type parameter `U` is just used to configure the vertex-unifying
/// behavior at compile time (which defaults to unify vertices). See
/// [`without_vertex_unification`][Reader::without_vertex_unification] for more
/// information.
///
/// # Example
///
/// ```no_run
/// use lox::{
///     prelude::*,
///     ds::FaceDelegateMesh,
///     fat::MiniMesh,
///     io::stl::Reader,
/// };
///
/// // We configure the reader to not unify the vertices. This means that the
/// // resulting mesh only contains unconnected triangles.
/// let reader = Reader::open("foo.stl")?.without_vertex_unification();
/// let m = MiniMesh::<FaceDelegateMesh>::create_from(reader)?;
/// # Ok::<_, lox::io::Error>(())
/// ```
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
        let is_binary = !buf.raw_buf().starts_with(b"solid")
            || buf.raw_buf().iter().take(1024).any(|b| !b.is_ascii())
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
            parse::whitespace(&mut buf)?;
            let solid_name = if is_binary {
                buf.with_bytes(
                    80 - buf.offset(),
                    |sd| {
                        sd.assert_ascii()
                            .map(|name| name.trim().to_string())
                            .map_err(|e| e.into())
                    },
                )?
            } else {
                let name = buf.take_until(b'\n', |sd| {
                    sd.assert_ascii()
                        .map(|name| name.trim().to_string())
                        .map_err(|e| e.into())
                })?;
                parse::linebreak(&mut buf)?;
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
    ///   read process a couple of times slower)
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
    /// Usually you either want to use a higher level interface (via
    /// [`StreamSource`]) or the method [`Reader::read_raw`]. The latter is the
    /// streaming version of this method which doesn't require a temporary
    /// storage ([`RawResult`]).
    pub fn into_raw_result(self) -> Result<RawResult, Error> {
        // Prepare the raw result with metadata and memory allocations.
        let mut out = RawResult::empty();
        out.solid_name = self.solid_name.clone();
        if let Some(tri_count) = self.triangle_count {
            out.triangles.reserve(tri_count as usize);
        }

        // Read the all triangles into the raw result
        self.read_raw(|tri| out.triangles.push(tri))?;

        Ok(out)
    }

    /// Reads the whole file, passing each triangle to the `add_triangle`
    /// callback.
    ///
    /// This is a low level building block that you usually don't want to use
    /// directly. In particular, **this method itself never performs any vertex
    /// unification** (regardless of the type parameter `U`). You usually want
    /// to use the [`StreamSource`]) interface to actually read meshes from
    /// this reader.
    pub fn read_raw(self, mut add_triangle: impl FnMut(RawTriangle)) -> Result<(), Error> {
        /// Parses three floats separated by whitespace. No leading or trailing
        /// whitespace is handled.
        fn vec3(buf: &mut impl ParseBuf) -> Result<[f32; 3], Error> {
            let x = parse::ascii_f32(buf)?;
            parse::whitespace(buf)?;
            let y = parse::ascii_f32(buf)?;
            parse::whitespace(buf)?;
            let z = parse::ascii_f32(buf)?;
            Ok([x, y, z])
        }

        /// Parses one ASCII line with a vertex (e.g. `vertex 2.0 0.1  1`)
        fn vertex(buf: &mut impl ParseBuf) -> Result<[f32; 3], Error> {
            parse::line(buf, |buf| {
                buf.expect_tag(b"vertex")?;
                parse::whitespace(buf)?;
                vec3(buf)
            })
        }

        let mut buf = self.buf;

        // ===== Parse body ==================================================
        if let Some(triangle_count) = self.triangle_count {
            // ===== BINARY ==================================================
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

                add_triangle(triangle);
            }

            // If the specified number of triangles was too small and there is
            // still data left, we also error.
            buf.assert_eof()?;
        } else {
            // ===== ASCII ===================================================
            // Parse facets
            loop {
                // First line (`facet normal 0.0 1.0 0.0`)
                let normal = parse::line(&mut buf, |buf| {
                    buf.expect_tag(b"facet normal")?;
                    parse::whitespace(buf)?;
                    vec3(buf)
                })?;

                // Parse vertices
                parse::line(&mut buf, |buf| buf.expect_tag(b"outer loop"))?;
                let vertices = [
                    vertex(&mut buf)?,
                    vertex(&mut buf)?,
                    vertex(&mut buf)?,
                ];
                parse::line(&mut buf, |buf| buf.expect_tag(b"endloop"))?;

                // Pass parsed triangle to callback
                add_triangle(RawTriangle {
                    normal,
                    vertices,
                    attribute_byte_count: 0,
                });

                // Parse last line (`endfacet`)
                parse::line(&mut buf, |buf| buf.expect_tag(b"endfacet"))?;

                // Check if the next line starts with `endsolid` and break loop
                // in that case.
                parse::opt_whitespace(&mut buf)?;
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
// ===== Definition of unifying dummy types. Not actually public.
// ===========================================================================
pub trait UnifyingMarker {
    type Adder: VertexAdder;
    const UNIFY: bool;
}

#[derive(Debug)]
pub enum UnifyVertices {}

impl UnifyingMarker for UnifyVertices {
    type Adder = UnifyingAdder;
    const UNIFY: bool = true;
}

#[derive(Debug)]
pub enum VerbatimVertices {}

impl UnifyingMarker for VerbatimVertices {
    type Adder = NonUnifyingAdder;
    const UNIFY: bool = false;
}

// ===========================================================================
// ===== VertexAdders: unify vertices or not. Not actually public.
// ===========================================================================
pub trait VertexAdder {
    fn new() -> Self;
    fn size_hint(&mut self, _vertex_count: u32) {}
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

    fn size_hint(&mut self, vertex_count: u32) {
        self.0.reserve(vertex_count as usize);
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


impl<R, U> StreamSource for Reader<R, U>
where
    R: io::Read + io::Seek,
    U: UnifyingMarker,
{
    fn transfer_to<S: MemSink>(self, sink: &mut S) -> Result<(), Error> {
        let mut vertex_adder = U::Adder::new();

        // Prepare the size hint. If we do not unify, we know the number of
        // vertices exactly.
        let face_count = self.triangle_count();
        let vertex_count = face_count
            .map(|tris| tris / 2)
            .filter(|_| !U::UNIFY);
        let hint = MeshSizeHint { vertex_count, face_count };

        // Give hints to the sink and our vertex adder.
        sink.size_hint(hint);
        sink.prepare_vertex_positions::<f32>(hint.guess_vertex_count())?;
        sink.prepare_face_normals::<f32>(hint.guess_face_count())?;
        vertex_adder.size_hint(hint.guess_vertex_count());

        // Read the body data
        self.read_raw(|triangle| {
            let [pa, pb, pc] = triangle.vertices;
            let a = vertex_adder.add_vertex(sink, pa);
            let b = vertex_adder.add_vertex(sink, pb);
            let c = vertex_adder.add_vertex(sink, pc);

            let f = sink.add_face([a, b, c]);
            sink.set_face_normal(f, triangle.normal.to_vector3());
        })
    }
}
