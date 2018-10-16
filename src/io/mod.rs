use std::{
    fs::File,
    io::{self, BufWriter, Cursor, Write},
    path::Path,
};

use crate::{
    Mesh, MeshUnsorted, ExplicitFace,
    map::VertexPropMap,
    math::Pos3Like,
};


pub mod parse;
pub mod ply;
pub mod stl;

/// Types that can be transformed into a [`MeshWriter`].
pub trait IntoMeshWriter<'a, MeshT, PosM>
where
    MeshT: 'a + Mesh + MeshUnsorted + ExplicitFace,
    PosM: 'a + VertexPropMap,
    PosM::Target: Pos3Like,
{
    type Writer: MeshWriter;
    fn into_writer(self, mesh: &'a MeshT, vertex_positions: &'a PosM) -> Self::Writer;
}

/// Types that can serialize a mesh with vertex positions and potentially
/// additional properties. The mesh and properties are already stored within
/// the type.
///
/// The main method of this trait is `write_to` which writes the mesh to a
/// given `io::Write` destination. There are some other provided methods for
/// easily writing to a file, to stdout and to memory.
pub trait MeshWriter {
    type Error: From<io::Error>;

    /// Writes the mesh and all mesh properties into the given `Write`
    /// instance.
    fn write_to(&self, writer: impl Write) -> Result<(), Self::Error>;

    /// Writes the mesh to the file given by the filename. Overwrites the file
    /// if it already exists.
    fn write_to_file(&self, path: impl AsRef<Path>) -> Result<(), Self::Error> {
        self.write_to(BufWriter::new(File::create(path)?))
    }

    /// Writes the mesh to stdout. Locks stdout for the time the mesh is being
    /// written.
    fn write_to_stdout(&self) -> Result<(), Self::Error> {
        let stdout = io::stdout();
        let lock = stdout.lock();
        self.write_to(lock)
    }

    /// Writes the mesh into a `Vec<u8>` which is returned on success.
    fn write_to_memory(&self) -> Result<Vec<u8>, Self::Error> {
        let mut w = Cursor::new(Vec::new());
        self.write_to(&mut w)?;
        Ok(w.into_inner())
    }
}
