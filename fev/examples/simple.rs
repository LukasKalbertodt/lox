extern crate fev;
extern crate failure;

use failure::Error;


use fev::{
    impls::sv::SharedVertexMesh,
    prop::{LabeledPropList, PropLabel},
    map::{VertexVecMap, PropStoreMut, fn_map::FnMap},
    io::{
        MeshWriter,
        ser::{DataType, PropListSerialize, Serializer, Serialize, SingleProp},
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

    fn serialize_at<S: Serializer>(
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

struct MyNormal {
    normal: [f32; 3],
}

impl PropListSerialize for MyNormal {
    fn data_type_of(prop_index: usize) -> DataType {
        match prop_index {
            0 => <[f32; 3] as Serialize>::DATA_TYPE,
            _ => unreachable!(),
        }
    }

    fn serialize_at<S: Serializer>(
        &self,
        prop_index: usize,
        serializer: S,
    ) -> Result<(), S::Error> {
        match prop_index {
            0 => self.normal.serialize(serializer),
            _ => unreachable!(),
        }
    }
}

impl LabeledPropList for MyNormal {
    fn num_props() -> usize {
        1
    }

    fn label_of(prop_index: usize) -> PropLabel {
        match prop_index {
            0 => PropLabel::Normal,
            _ => unreachable!(),
        }
    }
}

fn main() -> Result<(), Error> {
    let mut vm = VertexVecMap::new();

    let mut mesh = SharedVertexMesh::new();
    let a = mesh.add_vertex(MyProp { pos: (1.0, 2.0, 3.0) });
    vm.insert(a, MyNormal { normal: [1.0, 0.0, 0.0]});
    let b = mesh.add_vertex(MyProp { pos: (3.0, 2.0, 1.0) });
    vm.insert(b, MyNormal { normal: [0.0, 1.0, 0.0]});
    let c = mesh.add_vertex(MyProp { pos: (4.0, 4.0, 4.0) });
    vm.insert(c, MyNormal { normal: [0.0, 0.0, 1.0]});
    mesh.add_face([a, b, c], ());

    PlyWriter::tmp_new(PlyFormat::Ascii, &mesh)
        .add_vertex_prop(&vm)
        .add_vertex_prop_as(&FnMap(|_| Some(SingleProp(7))), &[PropLabel::Named("peter".into())])
        .write_to_stdout()?;

    // println!("{:#?}", mesh);

    Ok(())
}
