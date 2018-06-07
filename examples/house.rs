#![feature(termination_trait_lib)]

extern crate fev;
extern crate failure;

use std::fs::File;

use fev::{
    TriMesh,
    impls::FvTriMesh,
    io::{Ply, IntoMeshWriter, MeshWriter, PropLabel, PrimitiveType, LabeledPropSet, PropSetSerializer},
    map::{VertexVecMap, PropMapMut},
    // shape::{disk, GenPosition},
    shape2::{append_sphere, AdhocBuilder, SpheroidVertexInfo, HasPosition, HasNormal},
};


struct MyVertexInfo {
    position: (f32, f32, f32),
    normal: (f32, f32, f32),
}

impl<'a, T> From<&'a T> for MyVertexInfo
where
    T: HasPosition + HasNormal,
{
    fn from(src: &'a T) -> Self {
        let [px, py, pz] = *src.position();
        let [nx, ny, nz] = *src.normal();
        Self {
            position: (px as f32, py as f32, pz as f32),
            normal: (nx as f32, ny as f32, nz as f32),
        }
    }
}

impl LabeledPropSet for MyVertexInfo {
    const LABELS: &'static [PropLabel] = &[
        PropLabel::Position { scalar_ty: PrimitiveType::Float32 },
        PropLabel::Normal { scalar_ty: PrimitiveType::Float32 },
    ];

    fn serialize<S>(&self, mut serializer: S) -> Result<(), S::Error>
    where
        S: PropSetSerializer
    {
        serializer.serialize_position(&self.position)?;
        serializer.serialize_normal(&self.normal)?;
        Ok(())
    }
}

fn main() -> Result<(), failure::Error> {
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

    // let mut vec = std::io::Cursor::new(Vec::new());
    let mut file = File::create("foo.ply").unwrap();
    Ply::ascii()
    // Ply::binary()
        // .with_vertex_positions(&positions)
        .serialize(&mesh)?
        // .write(&mut vec)?;
        .write(&mut file)?;

    // println!("{:02x?}", vec.get_ref());

    Ok(())
}
