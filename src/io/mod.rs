//! Reading and writing meshes in different formats.
//!
//! This module contains everything to serialize and deserialize meshes in
//! different file formats from files or other sources of data. The full module
//! is quite large and a bit complex, but most of the time, you don't need to
//! fully understand everything. See the next "Quick Start" section for a brief
//! introduction.
//!
//! # Quick start
//!
//! To read or write meshes, you need a type that can store your mesh data and
//! implements [`MemSink`][io::MemSink] or [`MemSource`][io::MemSource],
//! respectively. You want to either use a type from the [`fat`] module or
//! write your own type and then `#[derive(MemSink, MemSource)]` for it. Most
//! examples in this modules use types from the `fat` module. (See the next
//! section for more details about the sink/source traits.)
//!
//! There are multiple ways to actually read or write. **You most certainly are
//! looking for [`read_file`][io::read_file] or
//! [`write_file`][io::write_file].** If you don't read from/write to a file,
//! there are four other convenience functions: [`read_from`][io::read_from],
//! [`read_from_mem`][io::read_from_mem], [`write_to`][io::write_to] and
//! [`write_to_mem`][io::write_to_mem].
//!
//! Here is a simple example of your basic small program using IO:
//!
//! ```no_run
//! use lox::{
//!     ds::FaceDelegateMesh,
//!     fat::MiniMesh,
//!     io,
//! };
//!
//!
//! // Using a predefined mesh type here. Alternatively, you can very easily
//! // create your own type!
//! type MyMesh = MiniMesh<FaceDelegateMesh>;
//!
//! // Read a mesh file (a PLY file in this case)
//! let mut m: MyMesh = io::read_file("input.ply")?;
//!
//! // ... do something with the mesh here
//!
//! // Write the resulting mesh
//! io::write_file(&m, "output.ply")?;
//!
//! # Ok::<_, io::Error>(())
//! ```
//!
//!
//! # Sources and Sinks
//!
//! There are four traits at the core of this module:
//! [`MemSource`][io::MemSource], [`StreamSource`][io::StreamSource],
//! [`MemSink`][io::MemSink] and [`StreamSink`][io::StreamSink]. They abstract
//! over all types that can provide or receive mesh data (connectivity *and*
//! property data). The `Mem*` variants can provide or receive data in
//! arbitrary order (random access), while the `Stream*` variants cannot and
//! are restricted to one particular access pattern. This has the following
//! consequences:
//!
//! - We cannot transfer data from a `StreamSource` to a `StreamSink`, because
//!   the source generally provides data in a different order than the sink
//!   expects. Thus, at least one `Mem*` is required to transfer data.
//! - When transferring mesh data from a source to a sink, the `Stream*`
//!   variant has complete control over the order of data.
//!
//! As an example for different access orders, consider the mesh formats PLY
//! and OBJ. Say we want to store vertices with a position and a normal. PLY
//! stores a list of vertices where each vertex contains all its properties. On
//! the other hand, OBJ stores all vertex positions, and *then* all vertex
//! normals. In short:
//!
//! - PLY: `pos0 normal0 pos1 normal1 ... posn normaln`
//! - OBJ: `pos0 pos1 ... posn normal0 normal1 ... normaln`
//!
//! You can't convert a PLY file to an OBJ file without having a temporary
//! buffer containing all mesh data. Of course, a PLY to PLY conversion would
//! work without temporary buffer, but this is a special case; if you want to
//! avoid the temporary buffer there, you can't use the `Source`/`Sink` API.
//!
//! Apart from files, there are some other implementors of the `Stream*`
//! traits. For example, there are many algorithms creating meshes
//! algorithmically, e.g. simple shapes (see [`shape` module][crate::shape]) or
//! triangulating iso-surfaces (compare: marching cubes and similar
//! algorithms). These algorithms also create data in a specific order and
//! cannot provide random access without having all data in memory already.
//!
//!
//!
//! # File Formats
//!
//! This section tries to answer the question "which file format should I
//! choose?" by comparing the different formats. Quick answer: **the PLY format
//! is a good default choice for most situations.** And whatever format you
//! choose, do not use ASCII encoding unless you have a good reason to (this
//! encoding is very space inefficient and slow).
//!
//! The following table shows a comparison. Symbol explanation for properties:
//! - `✘`: property *not* supported (cannot be stored in the file)
//! - `✔`: property supported (can be stored in the file)
//! - `✔*`: non-optional property (always has to be stored in the file)
//!
//! | Format | Connectivity | Memory Efficiency | V-Normal | V-Color | F-Normal | F-Color |
//! | ------ | ------------ | ----------------- | -------- | ------- | -------- | ------- |
//! | PLY   | shared vertex | good              | ✔        | ✔      | ✔        | ✔      |
//! | STL   | triangle soup | bad               | ✘        | ✘      | ✔*       | ✘      |
//!
//! More formats will be added in the future.
//!
//! All available file formats are listed by the enum
//! [`FileFormat`][io::FileFormat]. It also defines a few very useful methods.
//!
//!
//! # Three levels of IO: Convenience vs. Control
//!
//! This module offers three ways how to do IO. At the high level, there are
//! the six `read_*` and `write_*` functions directly in this module. These
//! were already mentioned in the *Quick Start* section.
//!
//! But sometimes, you might need more control than that. This is where the
//! `Reader` and `Writer` types come into play. There is a module for each file
//! format lox supports, each of which contains a `Reader` and `Writer`. These
//! are the types implementing `StreamSource` and `StreamSink`, respectively.
//! These `Writer` types are often configurable via a `Config` type, which is
//! also defined in the file format module.
//!
//! In very rare cases, you might need even more control. That's why the
//! `Reader` and `Writer` objects also offer a *raw* API. This is a very low
//! level, not-very-nice interface which allows you to receive the raw data
//! coming from the file. The idea is that even if the IO abstraction in this
//! library is not fitting someone's need, they still don't have to parse the
//! file themselves.
//!

// ----- Informal interface of format submodules ------------------------------
//
// All submodules that represent a file format (e.g. `ply`) have a similar
// interface: they export many symbols with the same name. This interface is
// not checked by the compiler (no traits are involved), but it's useful for
// users and library authors to make all those modules look about the same.
//
// Here is an informal description of said interface:
// - **`Config`**: a type with `into_writer` method. Should be public.
// - **`Reader`**: a type that implements [`StreamSource`][io::StreamSource].
//   Should be public.
// - **`Writer`**: a type that implements [`StreamSink`][io::StreamSink].
//   Should be public.
// - **`const FILE_EXTENSIONS: &[&str]`**: a list of file name extensions used
//   by that format (usually, it's only one extension, thus one element in the
//   slice). The slice must contain at least one element. The first element is
//   the most commonly used/preferred extension. Should be `pub(super)`.
// - **`is_file_start`**: checks if the given data is a valid start of a file
//   in the specific format. This is used to guess the file format of a given
//   file. If the file is ≤ 1024 bytes large, the full file is given to this
//   function, otherwise the first 1024 bytes are passed. This function is
//   only supposed to do quick checks: it shouldn't attempt to parse the
//   beginning of the file, but instead only look for magic numbers or similar
//   things. Should be `pub(super)`.

use std::{
    convert::TryInto,
    fmt,
    fs::File,
    io::{self, Read, Seek, SeekFrom},
    path::Path,
};

use failure::Backtrace;
use cgmath::{Point3, Vector3};
use failure::Fail;

use crate::{
    handle::{VertexHandle, FaceHandle, hsize},
    math::PrimitiveNum,
    prop::{ColorLike, PrimitiveColorChannel},
    sealed::Sealed,
    traits::{Empty, Mesh, TriVerticesOfFace},
    util::MeshSizeHint,
};
use self::{
    parse::ParseError,
    util::IsFormat,
};

pub mod parse;
pub mod ply;
pub mod stl;
pub mod util;

#[cfg(test)]
mod tests;


// ----------------------------------------------------------------------------

// ===========================================================================
// ===== Convenience `read_*` and `write_*` functions
// ===========================================================================

/// Reads from the given reader into an empty instance of type `SinkT` and
/// returns that instance.
///
/// If you want to read from files, [`read_file`] is the more convenient
/// function. If you still want to use this function to read from a file, you
/// don't need to wrap the file into a `BufReader` as the reading will be
/// buffered internally anyway.
///
/// ```no_run
/// use std::io::stdin;
/// use lox::{
///     ds::SharedVertexMesh,
///     fat::MiniMesh,
///     io::{self, FileFormat},
/// };
///
/// // Reading from stdin
/// let mesh: MiniMesh<SharedVertexMesh> = io::read_from(FileFormat::Ply, stdin())?;
/// # Ok::<_, io::Error>(())
/// ```
pub fn read_from<SinkT, R>(format: FileFormat, reader: R) -> Result<SinkT, Error>
where
    SinkT: Empty + MemSink,
    R: io::Read,
{
    let mut out = SinkT::empty();
    format.reader(reader)?.transfer_to(&mut out)?;
    out.finish()?;
    Ok(out)
}


/// Reads the file with the given filename into an empty instance of type
/// `SinkT` and returns that instance.
///
/// This function tries to automatically determine the file format from the
/// filename extension and the first few bytes of the file. If the format
/// couldn't be determined because it's unknown or ambiguous,
/// `ErrorKind::FormatUnknown` is returned. To explicitly specify the file format,
/// use the [`read_from`] function.
///
/// ```no_run
/// use lox::{
///     ds::FaceDelegateMesh,
///     fat::MiniMesh,
///     io,
/// };
///
/// let mesh: MiniMesh<FaceDelegateMesh> = io::read_file("foo.ply")?;
/// # Ok::<_, io::Error>(())
/// ```
pub fn read_file<SinkT, P>(path: P) -> Result<SinkT, Error>
where
    SinkT: Empty + MemSink,
    P: AsRef<Path>,
{
    // We have this inner method which takes a `&Path` directly to reduce the
    // number of instantiations of the outer function. These "convenience"
    // generics can actually often result in bloated binaries.
    fn inner<T: Empty + MemSink>(path: &Path) -> Result<T, Error> {
        // We don't need to use a `BufReader` here, because our internal parse
        // buffer already buffers.
        let mut file = File::open(path)?;

        // Guess the file format
        let format = match FileFormat::from_extension(path) {
            Some(f) => f,
            None => {
                // Read the first 1024 bytes
                let mut buf = Vec::new();
                Read::by_ref(&mut file).take(1024).read_to_end(&mut buf)?;
                file.seek(SeekFrom::Start(0))?; // back to the beginning

                // Guess from the data or just error that we couldn't find the
                // format.
                FileFormat::from_file_start(&buf)
                    .ok_or(Error::new(|| ErrorKind::FormatUnknown))?
            }
        };

        read_from(format, file)
    }

    inner(path.as_ref())
}

/// Reads from the given bytes into an empty instance of type `SinkT` and
/// returns that instance.
///
/// This is just a convenience wrapper for [`read_from`].
pub fn read_from_mem<SinkT>(format: FileFormat, data: &[u8]) -> Result<SinkT, Error>
where
    SinkT: Empty + MemSink,
{
    read_from(format, io::Cursor::new(data))
}


/// Writes the mesh defined by `src` with the given format to the given writer.
///
/// If possible, (native) binary encoding is used. If you really need to write
/// a file in ASCII encoding, use the corresponding `Writer` type in the file
/// format module.
///
/// If you want to write to files, [`write_file`] is the more convenient
/// function. If you still want to use this function to write to a file, you
/// should wrap the file into a `BufWriter` as unbuffered write operations are
/// fairly slow.
///
/// ```
/// use std::io::stdout;
/// use lox::{
///     prelude::*,
///     ds::SharedVertexMesh,
///     fat::MiniMesh,
///     io::{self, FileFormat},
/// };
///
/// // Writing to stdout
/// let dummy = MiniMesh::<SharedVertexMesh>::empty();
/// io::write_to(FileFormat::Ply, &dummy, stdout())?;
/// # Ok::<_, io::Error>(())
/// ```
pub fn write_to<SrcT, W>(format: FileFormat, src: &SrcT, writer: W) -> Result<(), Error>
where
    SrcT: MemSource,
    W: io::Write,
{
    format.writer(writer).transfer_from(src)
}


/// Writes the mesh defined by `src` to the file with the given filename (the
/// file is created/overwritten).
///
/// This function tries to automatically determine the file format from the
/// filename extension. If the format couldn't be determined because it's
/// unknown or ambiguous, `ErrorKind::FormatUnknown` is returned. To explicitly
/// specify the file format, use the [`write_to`] function.
///
/// If possible, (native) binary encoding is used. If you really need to write
/// a file in ASCII encoding, use the corresponding `Writer` type in the file
/// format module.
///
/// ```no_run
/// use lox::{
///     prelude::*,
///     ds::FaceDelegateMesh,
///     fat::MiniMesh,
///     io,
/// };
///
/// let dummy = MiniMesh::<FaceDelegateMesh>::empty();
/// io::write_file(&dummy, "foo.ply")?;
/// # Ok::<_, io::Error>(())
/// ```
pub fn write_file<SrcT, P>(src: &SrcT, path: P) -> Result<(), Error>
where
    SrcT: MemSource,
    P: AsRef<Path>,
{
    // We have this inner method which takes a `&Path` directly to reduce the
    // number of instantiations of the outer function. These "convenience"
    // generics can actually often result in bloated binaries.
    fn inner<T: MemSource>(path: &Path, src: &T) -> Result<(), Error> {
        // Guess the file format from extension
        let format = FileFormat::from_extension(path)
            .ok_or(Error::new(|| ErrorKind::FormatUnknown))?;

        // Write the file
        let file = io::BufWriter::new(File::create(path)?);
        write_to(format, src, file)
    }

    inner(path.as_ref(), src)
}

/// Writes the mesh defined by `src` with the given format to memory (into a
/// `Vec<u8>`).
///
/// This is just a convenience wrapper for [`read_from`].
pub fn write_to_mem<SrcT>(format: FileFormat, src: &SrcT) -> Result<Vec<u8>, Error>
where
    SrcT: MemSource,
{
    let mut v = Vec::new();
    write_to(format, src, &mut v)?;
    Ok(v)
}


// ===========================================================================
// ===== `FileFormat`, `Error` and other types
// ===========================================================================

/// Represents one of the supported file formats.
///
/// New file formats may be added with only minor version bumps, so you cannot
/// match this enum exhaustively.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum FileFormat {
    Ply,
    Stl,
}

impl FileFormat {
    /// Tries to guess the file format from the start of a file.
    ///
    /// Returns `None` if the format is ambiguous or if no format (known to the
    /// library) is detected. Note that if `Some` is returned, it doesn't mean
    /// that it's guaranteed that the file has the returned format; it only
    /// means that it's the most likely candidate.
    ///
    /// The given `data` has to be at least 1024 bytes long if the file is >=
    /// 1024 bytes long; otherwise `data` must contain the full file. It is
    /// passed to the `is_file_start` functions from the format submodules.
    pub fn from_file_start(data: &[u8]) -> Option<Self> {
        macro_rules! results {
            ($($module:ident => $variant:ident,)*) => {
                [$(
                    (FileFormat::$variant, $module::is_file_start(data))
                ,)*]
            }
        }

        let results = results!(
            ply => Ply,
            stl => Stl,
        );

        let probablies = results.iter()
            .filter(|(_, is_format)| *is_format == IsFormat::Probably)
            .map(|(format, _)| format)
            .collect::<Vec<_>>();

        match &*probablies {
            // No "probably" matches, let's try "maybe"s
            [] => {}

            // Exactly one format says "probably" -> perfect
            [one] => return Some(**one),

            // Two or more formats say "probably" -> that's bad
            _ => return None,
        }

        let maybes = results.iter()
            .filter(|(_, is_format)| *is_format == IsFormat::Maybe)
            .map(|(format, _)| format)
            .collect::<Vec<_>>();

        match &*maybes {
            [one] => Some(**one),
            _ => None,
        }
    }

    /// Tries to guess the file format from the file extension.
    ///
    /// It doesn't matter if the extension is uppercase or lowercase (or mixed)
    /// as it's converted to lowercase before matching.
    ///
    /// Returns `None` if:
    /// - the path/file has no extension in its name, or
    /// - the extension contains non-ASCII characters, or
    /// - the file extension is not known.
    pub fn from_extension(path: impl AsRef<Path>) -> Option<Self> {
        let ext = path.as_ref()
            .extension()
            .and_then(|ext| ext.to_str())
            .filter(|ext| ext.is_ascii())
            .map(|ext| ext.to_ascii_lowercase())?;

        match () {
            () if ply::FILE_EXTENSIONS.contains(&&*ext) => Some(FileFormat::Ply),
            () if stl::FILE_EXTENSIONS.contains(&&*ext) => Some(FileFormat::Stl),
            _ => None
        }
    }

    /// Returns the file name extensions used for this file format (e.g.
    /// `["ply"]` for `Ply`).
    ///
    /// Sometimes multiple extensions can be used for a file format. The
    /// recommended or most used one is always the first element in the
    /// returned slice. The returned extension is always lowercase. The
    /// returned slice is never empty.
    pub fn extensions(&self) -> &'static [&'static str] {
        match self {
            FileFormat::Ply => ply::FILE_EXTENSIONS,
            FileFormat::Stl => stl::FILE_EXTENSIONS,
        }
    }

    /// Checks if the given data is a valid start of a file in the specific
    /// format.
    ///
    /// This method only does quick checks and does not attempt to already
    /// parse the header.
    ///
    /// The given `data` has to be at least 1024 bytes long if the file is >=
    /// 1024 bytes long; otherwise `data` must contain the full file. It is
    /// passed to the `is_file_start` functions from the format submodules.
    pub fn is_file_start(&self, data: &[u8]) -> IsFormat {
        match self {
            FileFormat::Ply => ply::is_file_start(data),
            FileFormat::Stl => stl::is_file_start(data),
        }
    }

    /// Returns the writer object of the given format.
    ///
    /// The writer is already monomorphized with the underlying `io::Write`
    /// object plus the source type. This has the disadvantage that you have to
    /// already specify the types on this method. But we do get a significant
    /// speed advantage. See [`DynStreamSink`] for more information.
    ///
    /// The encoding is choosen depending on what the format supports. Native
    /// binary encoding is preferred, followed by swapped-endianess binary,
    /// followed by ASCII encoding. If you need to specify the encoding, take a
    /// look at [`writer_with_encoding`][FileFormat::writer_with_encoding].
    pub fn writer<'a, SrcT, W>(&self, w: W) -> Box<dyn DynStreamSink<SrcT> + 'a>
    where
        SrcT: MemSource,
        W: 'a + io::Write,
    {
        match self {
            FileFormat::Stl => Box::new(stl::Config::binary().into_writer(w)),
            FileFormat::Ply => Box::new(ply::Config::binary().into_writer(w)),
        }
    }

    /// Returns the writer object of the given format and encoding.
    ///
    /// Works like [`writer`][FileFormat::writer], but you can specify the
    /// encoding. If the encoding is not supported by the format,
    /// `ErrorKind::EncodingNotSupported` is returned.
    pub fn writer_with_encoding<'a, SrcT, W>(
        &self,
        encoding: FileEncoding,
        w: W,
    ) -> Result<Box<dyn DynStreamSink<SrcT> + 'a>, Error>
    where
        SrcT: MemSource,
        W: 'a + io::Write,
    {
        let err = Error::new(|| ErrorKind::EncodingNotSupported {
            file_format: *self,
            encoding,
        });

        macro_rules! writer {
            ($module:ident) => {{
                let encoding = encoding.try_into().map_err(|_| err)?;
                let config = $module::Config::new(encoding);
                Ok(Box::new(config.into_writer(w)))
            }}
        }

        match self {
            FileFormat::Ply => writer!(ply),
            FileFormat::Stl => writer!(stl),
        }
    }

    /// Returns the reader object of the given format.
    ///
    /// The reader is already monomorphized with the underlying `io::Read`
    /// object plus the sink type. This has the disadvantage that you have to
    /// already specify the types on this method. But we do get a significant
    /// speed advantage. See [`DynStreamSource`] for more information.
    pub fn reader<'a, SinkT, R>(&self, r: R) -> Result<Box<dyn DynStreamSource<SinkT> + 'a>, Error>
    where
        SinkT: MemSink,
        R: 'a + io::Read,
    {
        let out = match self {
            FileFormat::Stl => Box::new(stl::Reader::new(r)?)
                as Box<dyn DynStreamSource<SinkT> + 'a>,
            FileFormat::Ply => Box::new(ply::Reader::new(r)?),
        };

        Ok(out)
    }
}

impl fmt::Display for FileFormat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FileFormat::Ply => "PLY",
            FileFormat::Stl => "STL",
        }.fmt(f)
    }
}

/// Describes the encoding of the main data of a mesh file.
///
/// Not every format has to support all of these encodings (in fact, many
/// formats only support one encoding). In some formats, the header is always
/// stored in ASCII, but the body data can have different encodings.
///
/// The `fmt::Display` impl results in the strings `ASCII`, `big endian binary`
/// and `little endian binary`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileEncoding {
    /// Everything is stored as an ASCII string. Generally, ASCII encodings are
    /// fairly space-inefficient.
    Ascii,

    /// Binary encoding where all numeric types are stored in big endian
    /// layout.
    BinaryBigEndian,

    /// Binary encoding where all numeric types are stored in little endian
    /// layout.
    BinaryLittleEndian,
}

impl FileEncoding {
    /// Returns the binary encoding with native endianess (e.g.
    /// `BinaryLittleEndian` on x86).
    pub fn binary_native() -> Self {
        #[cfg(target_endian = "big")]
        { FileEncoding::BinaryBigEndian }

        #[cfg(target_endian = "little")]
        { FileEncoding::BinaryLittleEndian }
    }
}

impl fmt::Display for FileEncoding {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FileEncoding::Ascii => "ASCII",
            FileEncoding::BinaryBigEndian => "big endian binary",
            FileEncoding::BinaryLittleEndian => "little endian binary",
        }.fmt(f)
    }
}

/// Enumerates the supported kinds of mesh properties.
///
/// New property kinds may be added with only minor version bumps, so you
/// cannot match this enum exhaustively.
#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
pub enum PropKind {
    VertexPosition,
    VertexNormal,
    VertexColor,
    FaceNormal,
    FaceColor,
}

impl PropKind {
    fn plural_form(&self) -> &'static str {
        match self {
            PropKind::VertexPosition => "vertex positions",
            PropKind::VertexNormal => "vertex normals",
            PropKind::VertexColor => "vertex colors",
            PropKind::FaceNormal => "face normals",
            PropKind::FaceColor => "face colors",
        }
    }
}

/// The error type for all IO operations in this library.
///
/// The actual error description is defined by [`ErrorKind`]. This error stores
/// such an error kind plus a backtrace on the heap. You can inspect the kind
/// via [`Error::kind`]. Everything is stored on the heap in order to make
/// returning this error more efficient (the `ErrorKind` type is pretty large).
/// Allocation cost is not a problem, because this error isn't created very
/// often and if it is, it usually aborts the current operation.
///
/// This error can be created via [`Error::new`] or the `From` implementations,
/// most notably `From<std::io::Error>`. You can display information about this
/// error via the `Display` impl (i.e. `println!("{}", e)` or `e.to_string()`).
pub struct Error(Box<ErrorImpl>);

struct ErrorImpl {
    kind: ErrorKind,
    backtrace: Backtrace,
}

impl Error {
    /// Creates a new error with the error kind produced by the given closure.
    ///
    /// This function is not super cheap as it performs a heap allocation and
    /// potentially collects backtrace information. Therefore you should make
    /// sure that you won't create a lot of errors in your algorithm. Usually,
    /// this is not a problem because once you create (and return) and error,
    /// the operation doesn't continue.
    #[cold]
    #[inline(never)]
    pub fn new(kind: impl FnOnce() -> ErrorKind) -> Self {
        Self(Box::new(ErrorImpl {
            kind: kind(),
            backtrace: Backtrace::new(),
        }))
    }

    /// Returns the kind of this error.
    pub fn kind(&self) -> &ErrorKind {
        &self.0.kind
    }
}


impl From<io::Error> for Error {
    fn from(src: io::Error) -> Self {
        Self::new(|| src.into())
    }
}

impl From<ParseError> for Error {
    fn from(src: ParseError) -> Self {
        Self::new(|| src.into())
    }
}

impl Fail for Error {
    fn name(&self) -> Option<&str> {
        Some("io::Error")
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        Some(&self.0.backtrace)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.kind.fmt(f)
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.kind.fmt(f)?;
        write!(f, "\n\n")?;
        self.0.backtrace.fmt(f)?;
        Ok(())
    }
}


/// All kinds of things that can go wrong when doing IO. Is stored within
/// [`Error`].
///
/// This type shouldn't be returned directly, but always via [`Error`] as it is
/// more efficient and also stores a backtrace.
#[derive(Debug, Fail)]
#[non_exhaustive]
pub enum ErrorKind {
    /// An IO error.
    ///
    /// Can be caused by all kinds of failures. For example, if the underlying
    /// writer or reader returns an error or a file cannot be opened, this
    /// error variant is returned.
    Io(io::Error),

    /// An error while parsing input data.
    ///
    /// Whenever a file (or generally, a stream) is parsed as a specific format
    /// and the file isn't valid, this error is returned. See [`ParseError`]
    /// for more information.
    ///
    /// If you encounter this error, here is what you can do: make sure your
    /// input file is well-formed. If you are sure that your file is fine and
    /// other programs can succesfully parse that file, please consider
    /// reporting this as a parser bug.
    Parse(ParseError),

    /// An error indicating that the input file is not valid.
    ///
    /// This is similar to but different from `Parse`: while a parse error is
    /// something very much related to the low level syntax of the input file,
    /// this `InvalidInput` rather represents logical errors in the file (like
    /// faces not defining their vertices or wrong order of elements).
    /// Furthmore, parse errors can usually point to the exact part of the file
    /// where the error occured. These general input errors are more abstract
    /// and often don't just belong to one specific span.
    ///
    /// If you encounter this error, here is what you can do: make sure your
    /// input file is well-formed. If you are sure that your file is fine and
    /// other programs can succesfully parse that file, please consider
    /// reporting this as a parser bug.
    InvalidInput(String),

    /// This error can be returned by a `MemSink` to signal that it is not able
    /// to handle incoming data.
    ///
    /// This error usually means that you try to transfer mesh data from a
    /// source into a `MemSink` that has strict casting rules. E.g. if the sink
    /// stores vertex positions as `f32`, the source provides `f64` vertex
    /// positions and the sink only allows lossless casts, this error is
    /// returned from [`MemSink::prepare_vertex_positions`].
    ///
    /// If you encounter this error, here is what you can do:
    /// - If you own the sink: either change the type of your properties or use
    ///   a more relaxed casting mode (if you derived `MemSink`, you can add
    ///   `#[lox(vertex_position(cast = "lossy"))])` to your vertex position
    ///   field.
    /// - Otherwise: choose a different sink that supports your source's data
    ///   or choose a different source that only provides data compatible with
    ///   your sink.
    SinkIncompatible {
        prop: PropKind,
        source_type: PrimitiveType,
    },

    /// This error can be returned by a `MemSource` to signal that it is not
    /// able to provide a property in the type requested by the sink.
    ///
    /// This error usually means that you try to transfer mesh data from a
    /// `MemSource` that has strict casting rules. E.g. if the sink wants to
    /// store vertex positions as `f32`, the source provides `f64` vertex
    /// positions and the source only allows lossless casts, this error is
    /// returned from [`MemSource::vertex_position`].
    ///
    /// If you encounter this error, here is what you can do:
    /// - If you own the source: either change the type of your properties or
    ///   use a more relaxed casting mode (if you derived `MemSource`, you can
    ///   add `#[lox(vertex_position(cast = lossy))])` to your vertex position
    ///   field.
    /// - Otherwise: choose a different source that supports your sinks's data
    ///   types or choose a different sink that stores data in types compatible
    ///   with your source.
    SourceIncompatible {
        prop: PropKind,
        requested_type: PrimitiveType,
    },

    /// This error is raised when a sink detects that the source cannot provide
    /// all data required by the sink.
    ///
    /// If you encounter this error, here is what you can do:
    /// - Check why the source does not provide the data the sink requires and
    ///   potentially use a source that provides that data.
    /// - If you own the sink: relax the restrictions of what data is required.
    DataIncomplete {
        prop: PropKind,
        msg: String,
    },

    /// A file couldn't be opened because the file format was no specified and
    /// could not be automatically determined.
    ///
    /// If you encounter this error, here is what you can do:
    /// - Make sure the file you want to open has a valid file extension for
    ///   your file format (e.g. `.ply` for PLY).
    /// - Make sure to use a file format which can identify files based on a
    ///   magic number (e.g. *not* STL).
    /// - Specify the file format explicitly.
    FormatUnknown,

    /// A file format does not support a specific encoding.
    EncodingNotSupported {
        file_format: FileFormat,
        encoding: FileEncoding,
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorKind::Io(e) => write!(f, "IO error: {}", e),
            ErrorKind::Parse(e) => write!(f, "Parsing error: {}", e),
            ErrorKind::InvalidInput(msg) => write!(f, "invalid input: {}", msg),
            ErrorKind::SinkIncompatible { prop, source_type } => {
                write!(
                    f,
                    "sink is not compatible with source: sink cannot handle {} with type `{:?}` \
                        (if you derived `MemSink`, you might want to change the casting mode)",
                    prop.plural_form(),
                    source_type,
                )
            }
            ErrorKind::SourceIncompatible { prop, requested_type } => {
                write!(
                    f,
                    "source is not compatible with sink: source cannot provide {} with type \
                        `{:?}` (if you derived `MemSource`, you might want to change the casting \
                        mode)",
                    prop.plural_form(),
                    requested_type,
                )
            }
            ErrorKind::DataIncomplete { prop, msg } => {
                write!(
                    f,
                    "source data is incomplete (sink requires more data): missing {} ({})",
                    prop.plural_form(),
                    msg,
                )
            }
            ErrorKind::FormatUnknown => write!(f, "unknown or ambiguous file format"),
            ErrorKind::EncodingNotSupported { file_format, encoding } => {
                write!(
                    f,
                    "file format {} does not support {} encoding",
                    file_format,
                    encoding,
                )
            }
        }
    }
}

impl From<io::Error> for ErrorKind {
    fn from(src: io::Error) -> Self {
        ErrorKind::Io(src)
    }
}

impl From<ParseError> for ErrorKind {
    fn from(src: ParseError) -> Self {
        ErrorKind::Parse(src)
    }
}


// ==========================================================================
// ===== Primitives
// ==========================================================================

/// Represents the type of an IO primitive.
///
/// This is closely related to [`PrimitiveValue`] (which represents an IO
/// primitive value) and [`Primitive`] (which is a trait abstracting over the
/// closed set of IO primitive types).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    Uint8,
    Int8,
    Uint16,
    Int16,
    Uint32,
    Int32,
    Float32,
    Float64,
}

/// Represents an IO primitive value.
///
/// This is closely related to [`PrimitiveType`] (which represents only the
/// type of IO primitives) and [`Primitive`] (which is a trait abstracting over
/// the closed set of IO primitive types).
#[derive(Debug, Clone, Copy)]
pub enum PrimitiveValue {
    Uint8(u8),
    Int8(i8),
    Uint16(u16),
    Int16(i16),
    Uint32(u32),
    Int32(i32),
    Float32(f32),
    Float64(f64),
}

macro_rules! make_convert_method {
    ($name:ident, $ty:ident, $variant:ident) => {
        make_convert_method!($name, $ty, $variant, stringify!($ty), stringify!($variant));
    };
    ($name:ident, $ty:ident, $variant:ident, $ty_str:expr, $variant_str:expr) => {
        /// Returns this value as `
        #[doc = $ty_str]
        /// ` if `self` is `
        #[doc = $variant_str]
        /// `. Otherwise, `None` is returned. This function does not cast
        /// between different number types!
        pub fn $name(&self) -> Option<$ty> {
            match self {
                PrimitiveValue::$variant(x) => Some(*x),
                _ => None,
            }
        }
    };
}

impl PrimitiveValue {
    make_convert_method!(as_u8, u8, Uint8);
    make_convert_method!(as_i8, i8, Int8);
    make_convert_method!(as_u16, u16, Uint16);
    make_convert_method!(as_i16, i16, Int16);
    make_convert_method!(as_u32, u32, Uint32);
    make_convert_method!(as_i32, i32, Int32);
    make_convert_method!(as_f32, f32, Float32);
    make_convert_method!(as_f64, f64, Float64);

    /// Returns the type of this value.
    pub fn ty(&self) -> PrimitiveType {
        match self {
            PrimitiveValue::Uint8(_) => PrimitiveType::Uint8,
            PrimitiveValue::Int8(_) => PrimitiveType::Int8,
            PrimitiveValue::Uint16(_) => PrimitiveType::Uint16,
            PrimitiveValue::Int16(_) => PrimitiveType::Int16,
            PrimitiveValue::Uint32(_) => PrimitiveType::Uint32,
            PrimitiveValue::Int32(_) => PrimitiveType::Int32,
            PrimitiveValue::Float32(_) => PrimitiveType::Float32,
            PrimitiveValue::Float64(_) => PrimitiveType::Float64,
        }
    }
}

/// Abstracts over all IO primitive types.
///
/// Note that this trait is exactly implemented for the types that are included
/// in [`PrimitiveType`] and [`PrimitiveValue`]. Thus, this is a closed set of
/// implementing types (unusual for a trait). As a consequence, you are not
/// supposed to implement this trait for your own types! That's why this trait
/// has a supertrait called `Sealed`. Said supertrait is crate-private, so you
/// can't implement it for your types.
pub trait Primitive: PrimitiveNum + Sealed {
    /// The type represented as this [`PrimitiveType`] value.
    const TY: PrimitiveType;

    /// Returns the channel type represented at runtime by
    /// [`PrimitiveColorChannelType`] for `Primitive` types thare are also a
    /// [`PrimitiveColorChannel`].
    fn channel_type() -> PrimitiveColorChannelType
    where
        Self: PrimitiveColorChannel,
    {
        // We can unwrap here: we control all impls for this trait and we know
        // that `TY` always has the correct type. Since this method is bounded
        // by `Self: PrimitiveColorChannel`, we know for sure that `TY` is a
        // valid color channel type.
        PrimitiveColorChannelType::from_primitive_type(Self::TY).unwrap()
    }

    /// Returns the primitive as a [`PrimitiveValue`] (basically dynamic
    /// typing).
    ///
    /// The implementation of this method always returns a value with the same
    /// type as specified in `Self::TY`. In other words: `T::TY ==
    /// t.to_primitive_value().ty()` is always true for all primitives `t` with
    /// type `T`.
    fn to_primitive_value(&self) -> PrimitiveValue;
}

macro_rules! impl_primitive {
    ($ty:ident, $variant:ident) => {
        impl Sealed for $ty {}
        impl Primitive for $ty {
            const TY: PrimitiveType = PrimitiveType::$variant;
            fn to_primitive_value(&self) -> PrimitiveValue {
                PrimitiveValue::$variant(*self)
            }
        }
    }
}

impl_primitive!(u8,  Uint8);
impl_primitive!(i8,  Int8);
impl_primitive!(u16, Uint16);
impl_primitive!(i16, Int16);
impl_primitive!(u32, Uint32);
impl_primitive!(i32, Int32);
impl_primitive!(f32, Float32);
impl_primitive!(f64, Float64);

// ===========================================================================
// ===== Colors
// ===========================================================================

/// Represents the type of an IO color channel (subset of [`PrimitiveType`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveColorChannelType {
    Uint8,
    Uint16,
    Uint32,
    Float32,
    Float64,
}

impl PrimitiveColorChannelType {
    pub fn to_primitive_type(&self) -> PrimitiveType {
        match self {
            PrimitiveColorChannelType::Uint8 => PrimitiveType::Uint8,
            PrimitiveColorChannelType::Uint16 => PrimitiveType::Uint16,
            PrimitiveColorChannelType::Uint32 => PrimitiveType::Uint32,
            PrimitiveColorChannelType::Float32 => PrimitiveType::Float32,
            PrimitiveColorChannelType::Float64 => PrimitiveType::Float64,
        }
    }

    pub fn from_primitive_type(src: PrimitiveType) -> Option<Self> {
        match src {
            PrimitiveType::Uint8 => Some(PrimitiveColorChannelType::Uint8),
            PrimitiveType::Uint16 => Some(PrimitiveColorChannelType::Uint16),
            PrimitiveType::Uint32 => Some(PrimitiveColorChannelType::Uint32),
            PrimitiveType::Float32 => Some(PrimitiveColorChannelType::Float32),
            PrimitiveType::Float64 => Some(PrimitiveColorChannelType::Float64),
            _ => None,
        }
    }
}

/// Specifies the channel type of a color type as well as whether the color
/// type contains an alpha channel.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ColorType {
    /// Whether or not the color type contains an alpha channel.
    pub alpha: bool,

    /// Runtime representation of the channel's type.
    pub channel_type: PrimitiveColorChannelType,
}

impl ColorType {
    /// Takes the type level information of `C` ([`ColorLike::HAS_ALPHA`] and
    /// [`ColorLike::Channel`]) and creates runtime information (a `ColorType`
    /// instance) from it.
    pub fn from_color_like<C>() -> Self
    where
        C: ColorLike,
        C::Channel: Primitive,
    {
        Self {
            alpha: C::HAS_ALPHA,
            channel_type: C::Channel::channel_type(),
        }
    }
}


// ==========================================================================
// ===== {Streaming/Mem}-Sinks and Sources
// ==========================================================================

/// A type that stores mesh data which can only be accessed in the order
/// defined by this type.
///
/// This trait is mostly implemented by `Reader` types for specific file
/// formats or algorithms that create mesh data on the fly (like types in the
/// [`shape` module][crate::shape]).
///
/// The mesh data can be transferred to a [`MemSink`]. The important part is
/// that the `StreamSource` can decide in what order the mesh data is
/// transferred. The `MemSink` has to be able to accept the data in any order.
pub trait StreamSource {
    /// Transfers all of the mesh data in this source to the given sink.
    ///
    /// **For users of this method**: this function does not call
    /// [`MemSink::finish`] to make it possible to chain multiple `transfer_to`
    /// calls. Just remember to call `finish()` after your last `transfer_to`
    /// call!
    ///
    /// **For implementors of this method**: there are several rules about the
    /// interaction with the sink. See the documentation of [`MemSink`] for
    /// more information about how to use its methods.
    fn transfer_to<SinkT: MemSink>(self, sink: &mut SinkT) -> Result<(), Error>;
}

/// An object-safe [`StreamSource`] companion trait. This is only useful for
/// use as trait-object.
///
/// The trait `StreamSource` has a method with generic parameter and thus is
/// not object-safe (i.e. cannot be made into a trait-object). This is OK for
/// most uses, but sometimes a dynamically dispatched source is necessary.
/// That's what this trait is for. It moves the generic `SinkT` parameter from
/// the method to the trait to make it possible ot have a
/// `dyn DynStreamSink<MySource>`.
///
/// For more information, see [`DynStreamSink`] which works exactly like this
/// trait (but for sinks).
///
/// This trait is automatically implemented for all types that implement
/// [`StreamSource`].
pub trait DynStreamSource<SinkT: MemSink> {
    /// Like [`StreamSource::transfer_to`], but the `SinkT` parameter is
    /// already fixed in the trait.
    fn transfer_to(self: Box<Self>, sink: &mut SinkT) -> Result<(), Error>;
}

impl<T, SinkT> DynStreamSource<SinkT> for T
where
    SinkT: MemSink,
    T: StreamSource,
{
    fn transfer_to(self: Box<Self>, sink: &mut SinkT) -> Result<(), Error> {
        StreamSource::transfer_to(*self, sink)
    }
}

/// A type that can receive and store mesh data in any order.
///
/// This trait is mostly used to transfer mesh data from a [`StreamSource`]
/// into another type. In this kind of transfer, the `StreamSource` determines
/// the order of the data, while the `MemSink` has to be able to store the data
/// in any order. There are a few exceptions – those are explained below.
///
/// In general, if the source provides data that the sink cannot store, that
/// data is ignored/discarded and does not lead to errors.
///
///
/// # Kinds of methods on this trait
///
/// There are four kinds of methods:
/// - **Convenience methods for the user**: currently only `create_from`,
///   already provided. Intended to be used by the user of a `MemSink`, not by
///   a source.
/// - **Mesh connectivity**: `add_vertex` and `add_face`. These are the only
///   required methods. Intended to be used by the source when transferring
///   data.
/// - **Mesh properties**: `prepare_*` and `set_*` methods: empty/default
///   implementations provided. Intended to be used by the source when
///   transferring data.
/// - **Various other methods**: `size_hint` and `finish`. Empty implementation
///   provided. Intended to be used by the source when transferring data.
///
/// There are some rules for the methods concerning properties: for each
/// property (e.g. `vertex_position` or `face_normal`), the `prepare_*` method
/// has to be called by the source before the `set_*` method can be called.
/// Additionally, the type parameter must be the same for all calls of
/// `prepare_*` and `set_*` of one property. The sink can rely on these rules.
///
/// The handles passed to `set_` methods have to be handles returned
/// by `add_vertex` or `add_face`.
///
/// The `count` parameter of the `prepare_` methods is just an optimization and
/// represents a lower bound of the number of properties will be added via
/// `set_*`. Therefore, it's always valid for the source to pass 0 as `count`.
///
///
/// # Deriving
///
/// TODO
pub trait MemSink {
    // =======================================================================
    // ===== Mesh connectivity
    // =======================================================================
    fn add_vertex(&mut self) -> VertexHandle;
    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle;


    // =======================================================================
    // ===== Provided convenience method
    // =======================================================================

    /// Creates the sink from the given source.
    ///
    /// This is a convenience method that is already provided. There is
    /// probably no need to overwrite this method.
    fn create_from(source: impl StreamSource) -> Result<Self, Error>
    where
        Self: Sized + Empty,
    {
        let mut out = Self::empty();
        source.transfer_to(&mut out)?;
        out.finish()?;
        Ok(out)
    }

    // =======================================================================
    // ===== Various optional/provided methods
    // =======================================================================

    /// Might be called by the source to indicate how many vertices and faces
    /// are to be expected.
    ///
    /// This is just an optimization as it allows the sink to reserve memory.
    /// The provided implementation simply does nothing, which is absolutely
    /// valid.
    ///
    /// This method might not be called by the source at all.
    fn size_hint(&mut self, _hint: MeshSizeHint) {}

    /// Signals the sink that a transfer operation is finished and that no
    /// additional data will be added to the sink.
    ///
    /// This method allows the sink to raise an error in response to a invalid
    /// end state. This is usually done to make sure a sink has all the data it
    /// was expecting. Due to the arbitrary-order nature of source data, the
    /// sink's data is probably incomplete within a transfer operation. Calling
    /// this method says: "there is no additional data, make sure you are in a
    /// proper state now".
    ///
    /// This shouldn't be called by `StreamSink::transfer_to` directly, as
    /// calling `transfer_to` doesn't necessarily mean that a transfer is
    /// completed afterwards. Instead, `finish` is called by
    /// [`MemSink::create_from`] and thus by `io::read`.
    ///
    /// When deriving this trait, this method is implemented to check that
    /// property maps are complete (meaning: for every element in the mesh, a
    /// property has been added).
    fn finish(&mut self) -> Result<(), Error> {
        Ok(())
    }


    // =======================================================================
    // ===== Mesh properties
    // =======================================================================

    // ----- Vertex positions ------------------------------------------------
    /// Informs the sink that the source will provide at least `count` many
    /// vertex positions with the scalar type `N`.
    fn prepare_vertex_positions<N: Primitive>(&mut self, _count: hsize) -> Result<(), Error> {
        Ok(())
    }

    /// Sets the position (with scalar type `N`) of the vertex `v`.
    fn set_vertex_position<N: Primitive>(
        &mut self,
        _v: VertexHandle,
        _position: Point3<N>,
    ) {}


    // ----- Vertex normals --------------------------------------------------
    /// Informs the sink that the source will provide at least `count` many
    /// vertex normals with the scalar type `N`.
    fn prepare_vertex_normals<N: Primitive>(&mut self, _count: hsize) -> Result<(), Error> {
        Ok(())
    }

    /// Sets the normal (with scalar type `N`) of the vertex `v`.
    fn set_vertex_normal<N: Primitive>(
        &mut self,
        _v: VertexHandle,
        _normal: Vector3<N>,
    ) {}


    // ----- Vertex colors --------------------------------------------------
    /// Informs the sink that the source will provide at least `count` many
    /// vertex colors with the color type `C`.
    fn prepare_vertex_colors<C>(&mut self, _count: hsize) -> Result<(), Error>
    where
        C: ColorLike,
        C::Channel: Primitive,
    {
        Ok(())
    }

    /// Sets the color (with color type `C`) of the vertex `v`.
    fn set_vertex_color<C>(&mut self, _v: VertexHandle, _color: C)
    where
        C: ColorLike,
        C::Channel: Primitive,
    {}


    // ----- Face normals ----------------------------------------------------
    /// Informs the sink that the source will provide at least `count` many
    /// face normals with the scalar type `N`.
    fn prepare_face_normals<N: Primitive>(&mut self, _count: hsize) -> Result<(), Error> {
        Ok(())
    }

    /// Sets the normal (with scalar type `N`) of the face `f`.
    fn set_face_normal<N: Primitive>(
        &mut self,
        _f: FaceHandle,
        _normal: Vector3<N>,
    ) {}


    // ----- Face colors ----------------------------------------------------
    /// Informs the sink that the source will provide at least `count` many
    /// face colors with the color type `C`.
    fn prepare_face_colors<C>(&mut self, _count: hsize) -> Result<(), Error>
    where
        C: ColorLike,
        C::Channel: Primitive,
    {
        Ok(())
    }

    /// Sets the color (with color type `C`) of the face `f`.
    fn set_face_color<C>(&mut self, _f: FaceHandle, _color: C)
    where
        C: ColorLike,
        C::Channel: Primitive,
    {}
}

/// A type that can receive mesh data in an order defined by this type.
///
/// This trait is mostly implemented by `Writer` types for specific file
/// formats.
///
/// The mesh data is transferred from a [`MemSource`]. The important part is
/// that the `StreamSink` can decide in what order the mesh data is
/// transferred. The `MemSource` has to be able to provide the data in any
/// order (random access).
pub trait StreamSink {
    /// Transfers all mesh data from `src` to this sink.
    ///
    /// **For implementors of this method**: take a look at the documentation
    /// of [`MemSource`] for more information on how to interact with the
    /// source. There are some important rules regarding this interaction.
    fn transfer_from<SrcT: MemSource>(self, src: &SrcT) -> Result<(), Error>;
}

/// An object-safe [`StreamSink`] companion trait. This is only useful for use
/// as trait-object.
///
/// The trait `StreamSink` has a method with generic parameter and thus is not
/// object-safe (i.e. cannot be made into a trait-object). This is OK for most
/// uses, but sometimes a dynamically dispatched sink is necessary. That's what
/// this trait is for. It moves the generic `SrcT` parameter from the method to
/// the trait to make it possible ot have a `dyn DynStreamSink<MySource>`.
///
/// Having the source type as a trait parameter does restrict the potential
/// usages of this trait. In other words: you either have to know the type of
/// your writer or the type of your source.
///
/// Why is that? Speed. A typical mesh transfer operation has many interactions
/// between the underlying `io::Write` instance, the actual writing algorithm
/// and the source. Making any of these frequent calls virtual would slow down
/// the operation significantly. The design of this trait is a compromise: you
/// can provide all the type parameters up-front to monomorphize all calls and
/// still have a trait object.
///
/// This trait is automatically implemented for all types that implement
/// [`StreamSink`].
pub trait DynStreamSink<SrcT: MemSource> {
    fn transfer_from(self: Box<Self>, src: &SrcT) -> Result<(), Error>;
}

impl<T, SrcT> DynStreamSink<SrcT> for T
where
    T: StreamSink,
    SrcT: MemSource,
{
    fn transfer_from(self: Box<Self>, src: &SrcT) -> Result<(), Error> {
        StreamSink::transfer_from(*self, src)
    }
}


/// A type that can provide mesh data in any order (random access).
///
/// This trait is mostly used to transfer mesh data to a [`StreamSink`]. In
/// this kind of transfer, the `StreamSink` determines the order of the data,
/// while the `MemSource` has to be able to provide the data in any order.
///
///
/// # Rules regarding property methods
///
/// For each property, there are two methods (e.g. `vertex_position` and
/// `vertex_position_type`). There are some rules for using and implementing
/// these methods. Those rules are covered here instead of repeating the same
/// rules in all method descriptions.
///
/// The `*_type` method gives two pieces of information: (a) does the source
/// provide this property, and (b) what type does that property have. If the
/// source does *not* provide a property "foo", then calling the method `foo()`
/// will always panic. Thus, a sink using this interface should always call the
/// `*_type` method first to check if the property is provided.
///
/// The type returned by the `*_type` method is merely a recommendation for the
/// sink. If the sink can store multiple types, it should choose a type closest
/// to the returned type. But it is legal for the sink to always stick to one
/// type (in fact, many sinks can only store one type).
///
/// The `foo` method is then called by the sink with a specific type. This type
/// must be fixed! Meaning: over the lifetime of a `MemSource`,
/// `vertex_position` must always be called with the same type (the same is
/// true for other property methods). However, the source must be able to
/// handle all primitive types that a property function might be called with.
/// This is usually done via casting or returning `Err::SourceIncompatible`.
///
/// The handles passed to all main property methods must be valid handles
/// obtained from the mesh returned by `core_mesh()`.
///
/// All property methods have a default implemention which returns `None` and
/// panics in `*_type` and `*` respectively.
pub trait MemSource {
    /// The type of the core mesh.
    type CoreMesh: Mesh + TriVerticesOfFace;

    /// Returns the core mesh (storing the connectivity).
    fn core_mesh(&self) -> &Self::CoreMesh;


    // ----- Vertex positions -------------------------------------------------
    /// Returns the scalar type of the vertex positions of this source, or
    /// `None` if this source does not provide vertex positions.
    ///
    /// See [the trait documentation][MemSource] for important information!
    fn vertex_position_type(&self) -> Option<PrimitiveType> {
        None
    }

    /// Returns the vertex position of the vertex with the given handle, or
    /// `None` if there is no position associated with that vertex.
    ///
    /// See [the trait documentation][MemSource] for important information!
    fn vertex_position<T: Primitive>(&self, _v: VertexHandle) -> Result<Option<Point3<T>>, Error> {
        panic!(
            "requested vertex position from `MemSource`, but this source doesn't \
                contain vertex positions"
        );
    }

    // ----- Vertex normals -------------------------------------------------
    /// Returns the scalar type of the vertex normals of this source, or
    /// `None` if this source does not provide vertex normals.
    ///
    /// See [the trait documentation][MemSource] for important information!
    fn vertex_normal_type(&self) -> Option<PrimitiveType> {
        None
    }

    /// Returns the vertex normal of the vertex with the given handle, or
    /// `None` if there is no normal associated with that vertex.
    ///
    /// See [the trait documentation][MemSource] for important information!
    fn vertex_normal<T: Primitive>(&self, _v: VertexHandle) -> Result<Option<Vector3<T>>, Error> {
        panic!(
            "requested vertex normal from `MemSource`, but this source doesn't \
                contain vertex normals"
        );
    }

    // ----- Vertex colors --------------------------------------------------
    /// Returns the color type (including channel type and whether or not an
    /// alpha channel is present) of the vertex colors of this source, or
    /// `None` if this source does not provide vertex colors.
    ///
    /// See [the trait documentation][MemSource] for important information!
    fn vertex_color_type(&self) -> Option<ColorType> {
        None
    }

    /// Returns the vertex color of the vertex with the given handle, or `None`
    /// if there is no color associated with that vertex.
    ///
    /// See [the trait documentation][MemSource] for important information!
    fn vertex_color<C>(&self, _v: VertexHandle) -> Result<Option<C>, Error>
    where
        C: ColorLike,
        C::Channel: Primitive,
    {
        panic!(
            "requested vertex color from `MemSource`, but this source doesn't \
                contain vertex colors"
        );
    }

    // ----- Face normals -------------------------------------------------
    /// Returns the scalar type of the face normals of this source, or
    /// `None` if this source does not provide face normals.
    ///
    /// See [the trait documentation][MemSource] for important information!
    fn face_normal_type(&self) -> Option<PrimitiveType> {
        None
    }

    /// Returns the face normal of the face with the given handle, or
    /// `None` if there is no normal associated with that face.
    ///
    /// See [the trait documentation][MemSource] for important information!
    fn face_normal<T: Primitive>(&self, _f: FaceHandle) -> Result<Option<Vector3<T>>, Error> {
        panic!(
            "requested face normal from `MemSource`, but this source doesn't \
                contain face normals"
        );
    }

    // ----- Face colors --------------------------------------------------
    /// Returns the color type (including channel type and whether or not an
    /// alpha channel is present) of the face colors of this source, or `None`
    /// if this source does not provide face colors.
    ///
    /// See [the trait documentation][MemSource] for important information!
    fn face_color_type(&self) -> Option<ColorType> {
        None
    }

    /// Returns the face color of the face with the given handle, or `None`
    /// if there is no color associated with that face.
    ///
    /// See [the trait documentation][MemSource] for important information!
    fn face_color<C>(&self, _f: FaceHandle) -> Result<Option<C>, Error>
    where
        C: ColorLike,
        C::Channel: Primitive,
    {
        panic!(
            "requested face color from `MemSource`, but this source doesn't \
                contain face colors"
        );
    }
}


// TODO: add the following trait once GATs are available. Implement trait for
// `Config` types. Then we can also add a bunch of useful functions such as
// `write_to_mem`.
//
// trait IntoWriter {
//     type Writer<W: io::Write>;
//     fn into_writer<W: io::Write>(self) -> Self::Writer<W>;

//     // fn into_file_writer()
// }
