#![feature(rust_2018_preview)]


use std::{
    fs::{File},
    io::{self, BufWriter, Cursor, Write},
    path::Path,
};


mod parse;
pub mod ply;
pub mod stl;
pub mod ser;

pub use self::{
    stl::StlReader,
};


// TODO: Make better with GATs
pub trait IntoMeshWriter<'a, MeshT>
// where
//     MeshT: TriMesh + 'a,
//     MeshT::VertexProp: LabeledPropSet + PropSetSerialize,
{
    type Error: From<io::Error>;
    type Writer: MeshWriter<Error = Self::Error>;

    fn serialize(self, mesh: &'a MeshT) -> Result<Self::Writer, Self::Error>;
}


/// Types that contain a mesh and can serialize it in some form.
///
/// The main method of this trait is `write` which writes the mesh to a given
/// `io::Write` destination. There are some other provided methods for easily
/// writing to a file, to stdout and to memory.
///
/// Instances of these types are usually obtained by calling
/// [`IntoMeshWriter::serialize`].
pub trait MeshWriter {
    type Error: From<io::Error>;

    fn write(&self, writer: impl Write) -> Result<(), Self::Error>;

    /// Writes the mesh to the file given by the filename. Overwrites the file
    /// if it already exists.
    fn write_to_file(&self, path: impl AsRef<Path>) -> Result<(), Self::Error> {
        self.write(BufWriter::new(File::create(path)?))
    }

    /// Writes the mesh to stdout. Locks stdout for the time the mesh is being
    /// written.
    fn write_to_stdout(&self) -> Result<(), Self::Error> {
        let stdout = io::stdout();
        let lock = stdout.lock();
        self.write(lock)
    }

    /// Writes the mesh into a `Vec<u8>` which is returned on success.
    fn write_to_memory(&self) -> Result<Vec<u8>, Self::Error> {
        let mut w = Cursor::new(Vec::new());
        self.write(&mut w)?;
        Ok(w.into_inner())
    }
}


pub trait MeshReader {

}
