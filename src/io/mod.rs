use std::error::Error;
use std::fmt::Display;

use TriMesh;


mod ply;

pub use self::ply::Ply;


pub trait MeshSerialize {
    type Err: Error;


    // TODO: is `Display` really appropriate here?
    fn serialize<M>(self, mesh: &M) -> Result<(), Self::Err>
        where M: TriMesh,
              M::Idx: Display;
}



// fn write_ply<Idx: HandleIndex>(mesh: FvTriMesh<Idx>) {
//     for (vH, v) in mesh.vertices() {
//         for attr in attributes[vH] {
//             write!(file, "{} ", attr);
//         }
//     }
// }
