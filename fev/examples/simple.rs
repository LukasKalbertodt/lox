extern crate fev;
extern crate failure;

use failure::Error;


use fev::{
    impls::sv::SharedVertexMesh,
    prop::{LabeledPropList, PropLabel},
    io::{
        MeshWriter,
        ser::{DataType, PropListSerialize, Serializer, Serialize},
        ply::{
            PlyFormat,
            write::PlyWriter,
        },
    },
};


struct MyProp {
    pos: (f64, f64, f64),
}

impl PropListSerialize for MyProp {
    fn data_type_of(prop_index: usize) -> DataType {
        match prop_index {
            0 => <(f64, f64, f64) as Serialize>::DATA_TYPE,
            _ => unreachable!(),
        }
    }

    fn serialize<S: Serializer>(
        &self,
        prop_index: usize,
        serializer: S,
    ) -> Result<(), S::Error> {
        match prop_index {
            0 => self.pos.serialize(serializer),
            _ => unreachable!(),
        }
    }
}

impl LabeledPropList for MyProp {
    fn num_props() -> usize {
        1
    }

    fn label_of(prop_index: usize) -> PropLabel {
        match prop_index {
            0 => PropLabel::Position,
            _ => unreachable!(),
        }
    }
}

fn main() -> Result<(), Error> {
    let mut mesh = SharedVertexMesh::new();
    let a = mesh.add_vertex(MyProp { pos: (1.0, 2.0, 3.0) });
    let b = mesh.add_vertex(MyProp { pos: (3.0, 2.0, 1.0) });
    let c = mesh.add_vertex(MyProp { pos: (4.0, 4.0, 4.0) });
    mesh.add_face([a, b, c], ());

    let mut w = PlyWriter {
        format: PlyFormat::Ascii,
        mesh: &mesh,
    };
    w.write_to_stdout()?;

    // println!("{:#?}", mesh);

    Ok(())
}
