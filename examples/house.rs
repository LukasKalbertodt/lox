extern crate fev;

use std::fs::File;

use fev::{
    TriMesh,
    impls::FvTriMesh,
    io::{Ply},
    map::{VertexVecMap, PropMapMut},
    shape::{disk, GenPosition},
    shape2::{append_sphere, AdhocBuilder, SpheroidVertexInfo, HasPosition},
};


struct MyVertexInfo {
    position: (f64, f64, f64),
}

impl<'a, T> From<&'a T> for MyVertexInfo
where
    T: HasPosition,
{
    fn from(src: &'a T) -> Self {
        let [x, y, z] = *src.position();
        Self {
            position: (x, y, z),
        }
    }
}

fn main() {
    let mut mesh: FvTriMesh<MyVertexInfo> = FvTriMesh::new();
    // let mut positions = VertexVecMap::new();

    // let va = mesh.add_vertex();
    // positions.insert(va, (0.0, 0.0, 0.0));
    // let vb = mesh.add_vertex();
    // positions.insert(vb, (1.0, 3.0, 0.0));
    // let vc = mesh.add_vertex();
    // positions.insert(vc, (2.0, 0.0, 0.0));

    // let _f = mesh.add_face([va, vb, vc]);

    // append_sphere(&mut AdhocBuilder {
    //     shared: &mut mesh,
    //     add_vertex: |mesh: &mut &mut FvTriMesh, info: SpheroidVertexInfo| {
    //         let v = mesh.add_vertex(());
    //         positions.insert(v, info.position);
    //         v
    //     },
    //     add_face: |mesh: &mut &mut FvTriMesh, vertices, _| mesh.add_face(vertices, ()),
    // });
    append_sphere(&mut mesh);

    // let (mesh, GenPosition(positions)): (FvTriMesh, GenPosition<VertexVecMap<(f64, f64, f64)>>) = disk(300);

    let mut file = File::create("foo.ply").unwrap();
    Ply::ascii()
        // .with_vertex_positions(&positions)
        .write(&mut file, &mesh)
        .unwrap();
}
