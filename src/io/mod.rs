//! TODO
//!
//!
//! # Informal interface of format submodules
//!
//! All submodules that represent a file format (e.g. `ply`) have a similar
//! interface: they export many symbols with the same name. This interface is
//! not checked by the compiler (no traits are involved), but it's useful for
//! users and library authors to make all those modules look about the same.
//!
//! Here is an informal description of said interface:
//! - **`const FILE_EXTENSIONS: &[&str]`**: a list of file name extensions used
//!   by that format (usually, it's only one extension, thus one element in the
//!   slice). The slice must contain at least one element. The first element is
//!   the most commonly used/preferred extension.
//! - **`fn read`**: has the same signature as [`io::read`] and works exactly
//!   the same, except that it reads the file in the specific format and does
//!   not guess the format.
//! - **`is_file_start`**: checks if the given data is a valid start of a file
//!   in the specific format. This is used to guess the file format of a given
//!   file. If the file is <= 1024 bytes large, the full file is given to this
//!   function, otherwise the first 1024 bytes are passed to `is_file_start`.
//!   This function is only supposed to do quick checks: it shouldn't attempt
//!   to parse the beginning of the file, but instead only look for magic
//!   numbers or similar things.
//! - TODO

#![allow(unused_imports)] // TODO

use std::{
    fmt,
    fs::File,
    marker::PhantomData,
    io::{self, BufWriter, Cursor, Read, Write},
    path::Path,
};

use cgmath::{Point3, Vector3};
use failure::Fail;

use crate::{
    Mesh, TriVerticesOfFace,
    handle::{VertexHandle, FaceHandle, hsize},
    map::VertexPropMap,
    math::{PrimitiveFloat, PrimitiveNum},
    prop::{ColorLike, PrimitiveColorChannel, Pos3Like},
    sealed::Sealed,
    traits::Empty,
    util::MeshSizeHint,
};
use self::{
    parse::ParseError,
    util::{TypeWish, DefaultTypeWishes, WishFor},
};

pub mod parse;
pub mod ply;
pub mod stl;
pub mod util;

#[cfg(test)]
mod tests;


/// Reads the file with the given filename into an empty instance of type `T`
/// and returns that instance.
///
/// This function tries to automatically determine the file format from the
/// filename extension and the first few bytes of the file. If the format
/// couldn't be determined because it's unknown or ambiguous,
/// `Error::FormatUnknown` is returned.
///
/// ```no_run
/// use lox::{
///     ds::FaceDelegateMesh,
///     fat::MiniMesh,
///     io
/// };
///
/// # fn main() -> Result<(), failure::Error> {
/// let mesh: MiniMesh<FaceDelegateMesh> = io::read("foo.ply")?;
/// # Ok(())
/// # }
/// ```
pub fn read<T: Empty + MemSink, P: AsRef<Path>>(path: P) -> Result<T, Error> {
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

                // Guess from the data or just error that we couldn't find the
                // format.
                FileFormat::from_file_start(&buf).ok_or(Error::FormatUnknown)?
            }
        };

        // Read the file into the specified sink
        match format {
            FileFormat::Ply => T::create_from(ply::Reader::new(file)?),
            FileFormat::Stl => T::create_from(stl::Reader::new(file)?),
        }
    }

    inner(path.as_ref())
}

// /// Types that can be transformed into a [`MeshWriter`].
// pub trait IntoMeshWriter<'a, MeshT, PosM>
// where
//     MeshT: 'a + Mesh + TriVerticesOfFace,
//     PosM: 'a + VertexPropMap,
//     PosM::Target: Pos3Like,
// {
//     type Writer: MeshWriter;
//     fn into_writer(self, mesh: &'a MeshT, vertex_positions: &'a PosM) -> Self::Writer;
// }

// /// Types that can serialize a mesh with vertex positions and potentially
// /// additional properties. The mesh and properties are already stored within
// /// the type.
// ///
// /// The main method of this trait is `write_to` which writes the mesh to a
// /// given `io::Write` destination. There are some other provided methods for
// /// easily writing to a file, to stdout and to memory.
// pub trait MeshWriter {
//     type Error: From<io::Error>;

//     /// Writes the mesh and all mesh properties into the given `Write`
//     /// instance.
//     fn write_to(&self, writer: impl Write) -> Result<(), Self::Error>;

//     /// Writes the mesh to the file given by the filename. Overwrites the file
//     /// if it already exists.
//     fn write_to_file(&self, path: impl AsRef<Path>) -> Result<(), Self::Error> {
//         self.write_to(BufWriter::new(File::create(path)?))
//     }

//     /// Writes the mesh to stdout. Locks stdout for the time the mesh is being
//     /// written.
//     fn write_to_stdout(&self) -> Result<(), Self::Error> {
//         let stdout = io::stdout();
//         let lock = stdout.lock();
//         self.write_to(lock)
//     }

//     /// Writes the mesh into a `Vec<u8>` which is returned on success.
//     fn write_to_memory(&self) -> Result<Vec<u8>, Self::Error> {
//         let mut w = Cursor::new(Vec::new());
//         self.write_to(&mut w)?;
//         Ok(w.into_inner())
//     }
// }

/// The result of inspecting the start of the file to check if it's a file of a
/// specific format.
///
/// This is returned by the `is_file_start` functions in each file format
/// module.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IsFormat {
    /// The file is very likely a file of the specified format.
    ///
    /// This should be returned when there are strong indicators of the
    /// specified format (e.g. the magic number is found). The `is_file_start`
    /// function is not required to already try parsing the file and properly
    /// check for errors. Instead, quick and easy indicators should be used.
    Probably,

    /// The file could be a file of the specified format, but there is no clear
    /// indicator that it is.
    ///
    /// This should be returned as rarely as possible. It's only necessary when
    /// a file format does not have a magic number or something like that.
    Maybe,

    /// The file is definitely not valid in the specified format (e.g. a magic
    /// number is not found).
    No,
}

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

    /// Returns the file name extension used for this file format (e.g. `"ply"`
    /// for `Ply`).
    ///
    /// If there are multiple extensions that can be used for this file format,
    /// the recommended or most used one is returned. The returned extension is
    /// always lowercase.
    pub fn extension(&self) -> &'static str {
        // We can just return the first element, as our informal module
        // interface requires that to be the recommended extesion (see module
        // documentation).
        match self {
            FileFormat::Ply => ply::FILE_EXTENSIONS[0],
            FileFormat::Stl => stl::FILE_EXTENSIONS[0],
        }
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
/// formats only support one encoding).
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
    pub fn binary_native() -> Self {
        #[cfg(target_endian = "big")]
        { FileEncoding::BinaryBigEndian }

        #[cfg(target_endian = "little")]
        { FileEncoding::BinaryLittleEndian }
    }
}

/// A simple unit-like error type that is used for `TryFrom<FileEncoding>`
/// impls of format specific `Encoding` types.
#[derive(Debug, Clone, Copy)]
pub struct EncodingNotSupported;


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

/// The error type for reading or writing a mesh.
#[derive(Debug, Fail)]
#[non_exhaustive]
pub enum Error {
    /// An IO error.
    ///
    /// Can be caused by all kinds of failures. For example, if the underlying
    /// writer or reader returns an error or a file cannot be opened, this
    /// error variant is returned.
    Io(io::Error),

    /// An error while parsing input data.
    ///
    /// Whenever a file (or generally, a stream) is parsed as a specific format
    /// and the file isn't valid, this error is returned. See [`parse::Error`]
    /// for more information.
    ///
    /// If you encounter this error, here is what you can do: make sure your
    /// input file is well-formed. If you are sure that your file is fine and
    /// other programs can succesfully parse that file, please consider
    /// reporting this as a parser bug.
    Parse(ParseError),

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
    ///   `#[lox(vertex_position(cast = lossy))])` to your vertex position
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
    DataIncomplete(String),

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
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Io(e) => write!(f, "IO error: {}", e),
            Error::Parse(e) => write!(f, "Parsing error: {}", e),
            Error::SinkIncompatible { prop, source_type } => {
                write!(
                    f,
                    "sink is not compatible with source: sink cannot handle {} with type `{:?}` \
                        (if you derived `MemSink`, you might want to change the casting mode)",
                    prop.plural_form(),
                    source_type,
                )
            }
            Error::SourceIncompatible { prop, requested_type } => {
                write!(
                    f,
                    "source is not compatible with sink: source cannot provide {} with type \
                        `{:?}` (if you derived `MemSource`, you might want to change the casting \
                        mode)",
                    prop.plural_form(),
                    requested_type,
                )
            }
            Error::DataIncomplete(details) => {
                write!(
                    f,
                    "source data is incomplete (sink requires more data): {}",
                    details,
                )
            }
            Error::FormatUnknown => write!(f, "unknown or ambiguous file format"),
        }
    }
}

impl From<io::Error> for Error {
    fn from(src: io::Error) -> Self {
        Error::Io(src)
    }
}

impl From<ParseError> for Error {
    fn from(src: ParseError) -> Self {
        Error::Parse(src)
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

    /// Returns the primitive as a [`PrimitiveValue`] (basically dynamic
    /// typing).
    ///
    /// The implementation of this method always returns a value with the same
    /// type as specified in `Self::TY`. In other words: `T::TY ==
    /// t.to_primitive_value().ty()` is always true for all primitives `t` with
    /// type `T`.
    fn to_primitive_value(&self) -> PrimitiveValue;

    /// Returns `Some(self)` if `T == Self`, or `None` otherwise.
    ///
    /// This method is useful to convert a generic value into a value of a
    /// specific type.
    fn downcast_as<T: Primitive>(self) -> Option<T> {
        DowncastAs::downcast_as(self)
    }
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

        impl DowncastAs<$ty> for $ty {
            fn downcast_as(self) -> Option<$ty> {
                Some(self)
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

/// A helper trait to implement [`Primitive::downcast_as`].
trait DowncastAs<Target: Primitive> {
    fn downcast_as(self) -> Option<Target>;
}

// The default impl returns `None`. Specific impls will overwrite that.
impl<Source: Primitive, Target: Primitive> DowncastAs<Target> for Source {
    default fn downcast_as(self) -> Option<Target> {
        None
    }
}

/// Specific color type used in IO traits. Alpha is optional.
#[derive(Clone, Copy, Debug)]
pub struct Color<C: PrimitiveColorChannel> {
    pub r: C,
    pub g: C,
    pub b: C,
    pub a: Option<C>,
}

impl<C: PrimitiveColorChannel> ColorLike for Color<C> {
    type Channel = C;

    fn from_rgb(r: Self::Channel, g: Self::Channel, b: Self::Channel) -> Self {
        Self { r, g, b, a: None }
    }
    fn from_rgba(r: Self::Channel, g: Self::Channel, b: Self::Channel, a: Self::Channel) -> Self {
        Self { r, g, b, a: Some(a) }
    }
    fn red(&self) -> Self::Channel {
        self.r
    }
    fn green(&self) -> Self::Channel {
        self.g
    }
    fn blue(&self) -> Self::Channel {
        self.b
    }
    fn alpha(&self) -> Option<Self::Channel> {
        self.a
    }
}



// ==========================================================================
// ===== {Streaming/Mem}-Sinks and Sources
// ==========================================================================

pub trait StreamSource {
    fn transfer_to<S: MemSink>(self, sink: &mut S) -> Result<(), Error>;
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
/// - **Mesh properties**: `prepare_*` and `set_*` methods plus one associated
///   type per property: empty/default implementations provided. Intended to be
///   used by the source when transferring data.
/// - **Various other methods**: `size_hint` and `finish`. Empty implementation
///   provided. Intended to be used by the source when transferring data.
///
/// There are some rules for the methods concerning properties: for each
/// property (e.g. `vertex_position` or `face_normal`), the `prepare_*` method
/// has to be called by the source before the `set_*` method can be called.
/// Additionally, the `N` type parameter must be the same for all calls of
/// `prepare_*` and `set_*` of one property. The sink can rely on these rules.
///
/// The handles passed to `set_` methods have to be handles returned
/// by `add_vertex` or `add_face`.
///
/// The `count` parameter of the `prepare_` methods is just an optimization and
/// represents a lower bound of the number of properties will be added via
/// `set_*`. Therefore, it's always valid for the source to pass 0 as `count`.
///
/// The associated type for each property (e.g. `VertexPosition`) is a bit
/// special, too. They can be used to signal a preferred property type to the
/// source. The source is not forced to use those type wishes, but it usually
/// does so if it makes sense: for example when parsing ASCII values or
/// generating values. See [`TypeWish`] for more information.
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
    /// Preferred types for the scalar type of vertex positions.
    type VertexPosition: TypeWish = DefaultTypeWishes;

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
    /// Preferred types for the scalar type of vertex normals.
    type VertexNormal: TypeWish = DefaultTypeWishes;

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
    /// vertex colors with the color channel type `C`. `alpha` specified
    /// whether the provided colors will have an alpha channel.
    fn prepare_vertex_colors<C: PrimitiveColorChannel + Primitive>(
        &mut self,
        _count: hsize,
        _alpha: bool,
    ) -> Result<(), Error> {
        Ok(())
    }

    /// Sets the color (with color channel type `C`) of the vertex `v`.
    fn set_vertex_color<C: PrimitiveColorChannel + Primitive>(
        &mut self,
        _v: VertexHandle,
        _color: Color<C>,
    ) {}


    // ----- Face normals ----------------------------------------------------
    /// Preferred types for the scalar type of face normals.
    type FaceNormal: TypeWish = DefaultTypeWishes;

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
    /// face colors with the color channel type `C`. `alpha` specified whether
    /// the provided colors will have an alpha channel.
    fn prepare_face_colors<C: PrimitiveColorChannel + Primitive>(
        &mut self,
        _count: hsize,
        _alpha: bool,
    ) -> Result<(), Error> {
        Ok(())
    }

    /// Sets the color (with color channel type `C`) of the face `f`.
    fn set_face_color<C: PrimitiveColorChannel + Primitive>(
        &mut self,
        _f: FaceHandle,
        _color: Color<C>,
    ) {}
}

pub trait StreamSink {
    fn transfer_from<S: MemSource>(self, src: &S) -> Result<(), Error>;
}


/// ...
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
/// will always panic. Thus, a sink using this interace should always call the
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
    type CoreMesh: Mesh + TriVerticesOfFace;
    fn core_mesh(&self) -> &Self::CoreMesh;


    // ----- Vertex positions -------------------------------------------------
    /// Returns the scalar type of the vertex positions of this source or
    /// `None` if this source does not provide vertex positions.
    ///
    /// See [the trait documentation][MemSource] for important information!
    fn vertex_position_type(&self) -> Option<PrimitiveType> {
        None
    }

    /// Returns the vertex position of the vertex with the given handle, or
    /// `None` if there is no position associated with that vertex.
    ///
    /// See [the trait documentation][MemSource] for important
    /// information!
    fn vertex_position<T: Primitive>(&self, _v: VertexHandle) -> Result<Option<Point3<T>>, Error> {
        panic!("requested non-existent vertex position from `MemSource`");
    }
}
