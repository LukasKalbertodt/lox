use failure::Error;
use smallvec::SmallVec;

use crate::{
    mesh,
    prelude::*,
    ds::SharedVertexMesh,
    map::{ConstMap, FnMap, VecMap},
};
use super::{
    Serializer, Encoding, Reader, PropertyType, ScalarType, PropIndex, Property,
    ListLenType, RawOffset, RawResult,
};


// ===========================================================================
// ===== Writing
// ===========================================================================

fn triangle_mesh() -> (SharedVertexMesh, VecMap<VertexHandle, [f32; 3]>) {
    mesh! {*
        type: SharedVertexMesh,
        vertices: [
            v0: ([0.0f32, 0.0, 0.0]),
            v1: ([3.0, 5.0, 8.0]),
            v2: ([1.942, 152.99, 0.007]),
        ],
        faces: [
            [v0, v1, v2],
        ],
    }
}

#[test]
fn write_triangle_ascii() -> Result<(), Error> {
    let (mesh, positions) = triangle_mesh();

    let res = Serializer::ascii()
        .into_writer(&mesh, &positions)
        .write_to_memory()?;

    assert_eq_file!(&res, "triangle_ascii.ply");
    Ok(())
}

#[test]
fn write_triangle_with_comments_ascii() -> Result<(), Error> {
    let (mesh, positions) = triangle_mesh();

    let res = Serializer::ascii()
        .add_comment("My name is Tom")
        .add_comment("Yes we can have multiple comments :)")
        .add_comment(
            "Comments appear in the file in the same order as they are added with `add_comment`"
        )
        .into_writer(&mesh, &positions)
        .write_to_memory()?;

    assert_eq_file!(&res, "triangle_with_comments_ascii.ply");
    Ok(())
}

#[test]
fn write_triangle_with_extra_props_ascii() -> Result<(), Error> {
    let (mesh, positions, bar) = mesh! {*
        type: SharedVertexMesh,
        vertices: [
            v0: ([0.0f32, 0.0, 0.0], vec![]),
            v1: ([3.0, 5.0, 8.0], vec![-1i8]),
            v2: ([1.942, 152.99, 0.007], vec![3, 8]),
        ],
        faces: [
            [v0, v1, v2],
        ],
    };

    let res = Serializer::ascii()
        .into_writer(&mesh, &positions)
        .add_vertex_prop("foo", &ConstMap([0.93f64, 0.2, 0.3]))
        .add_vertex_prop("bar", &FnMap(|h| bar.get(h).map(|v| v.into_inner().as_slice())))
        .add_vertex_prop("baz", &FnMap(|h: VertexHandle| Some(3 * h.to_usize() as u16)))
        .add_face_prop("cats", &ConstMap(-99.123f32))
        .write_to_memory()?;

    assert_eq_file!(&res, "triangle_with_extra_props_ascii.ply");
    Ok(())
}

#[test]
fn write_triangle_bbe() -> Result<(), Error> {
    let (mesh, positions) = triangle_mesh();

    let res = Serializer::new(Encoding::BinaryBigEndian)
        .into_writer(&mesh, &positions)
        .write_to_memory()?;

    assert_eq_file!(&res, "triangle_bbe.ply");
    Ok(())
}

#[test]
fn write_triangle_with_comments_bbe() -> Result<(), Error> {
    let (mesh, positions) = triangle_mesh();

    let res = Serializer::new(Encoding::BinaryBigEndian)
        .add_comment("My name is Tom")
        .add_comment("Yes we can have multiple comments :)")
        .add_comment(
            "Comments appear in the file in the same order as they are added with `add_comment`"
        )
        .into_writer(&mesh, &positions)
        .write_to_memory()?;

    assert_eq_file!(&res, "triangle_with_comments_bbe.ply");
    Ok(())
}

#[test]
fn write_triangle_with_extra_props_bbe() -> Result<(), Error> {
    let (mesh, positions, bar) = mesh! {*
        type: SharedVertexMesh,
        vertices: [
            v0: ([0.0f32, 0.0, 0.0], vec![]),
            v1: ([3.0, 5.0, 8.0], vec![-1i8]),
            v2: ([1.942, 152.99, 0.007], vec![3, 8]),
        ],
        faces: [
            [v0, v1, v2],
        ],
    };

    let res = Serializer::new(Encoding::BinaryBigEndian)
        .into_writer(&mesh, &positions)
        .add_vertex_prop("foo", &ConstMap([0.93f64, 0.2, 0.3]))
        .add_vertex_prop("bar", &FnMap(|h| bar.get(h).map(|v| v.into_inner().as_slice())))
        .add_vertex_prop("baz", &FnMap(|h: VertexHandle| Some(3 * h.to_usize() as u16)))
        .add_face_prop("cats", &ConstMap(-99.123f32))
        .write_to_memory()?;

    assert_eq_file!(&res, "triangle_with_extra_props_bbe.ply");
    Ok(())
}

#[test]
fn write_triangle_with_extra_props_ble() -> Result<(), Error> {
    let (mesh, positions, bar) = mesh! {*
        type: SharedVertexMesh,
        vertices: [
            v0: ([0.0f32, 0.0, 0.0], vec![]),
            v1: ([3.0, 5.0, 8.0], vec![-1i8]),
            v2: ([1.942, 152.99, 0.007], vec![3, 8]),
        ],
        faces: [
            [v0, v1, v2],
        ],
    };

    let res = Serializer::new(Encoding::BinaryLittleEndian)
        .into_writer(&mesh, &positions)
        .add_vertex_prop("foo", &ConstMap([0.93f64, 0.2, 0.3]))
        .add_vertex_prop("bar", &FnMap(|h| bar.get(h).map(|v| v.into_inner().as_slice())))
        .add_vertex_prop("baz", &FnMap(|h: VertexHandle| Some(3 * h.to_usize() as u16)))
        .add_face_prop("cats", &ConstMap(-99.123f32))
        .write_to_memory()?;

    assert_eq_file!(&res, "triangle_with_extra_props_ble.ply");
    Ok(())
}


// ===========================================================================
// ===== Reading
// ===========================================================================

fn check_triangle(res: &RawResult) {
    let groups = &res.element_groups;
    assert_eq!(groups.len(), 2);

    let g0 = &groups[0];
    assert_eq!(g0.def.name, "vertex");
    assert_eq!(g0.def.count, 3);
    assert_eq!(g0.def.property_defs[PropIndex(0)].ty, PropertyType::Scalar(ScalarType::Float));
    assert_eq!(g0.def.property_defs[PropIndex(0)].name, "x");
    assert_eq!(g0.def.property_defs[PropIndex(1)].ty, PropertyType::Scalar(ScalarType::Float));
    assert_eq!(g0.def.property_defs[PropIndex(1)].name, "y");
    assert_eq!(g0.def.property_defs[PropIndex(2)].ty, PropertyType::Scalar(ScalarType::Float));
    assert_eq!(g0.def.property_defs[PropIndex(2)].name, "z");

    assert_eq!(g0.elements[0].prop_infos[PropIndex(0)].offset, RawOffset(0));
    assert_eq!(g0.elements[0].prop_infos[PropIndex(1)].offset, RawOffset(4));
    assert_eq!(g0.elements[0].prop_infos[PropIndex(2)].offset, RawOffset(8));
    assert_eq!(g0.elements[0].iter().collect::<Vec<_>>(), &[
        Property::Float(0.0),
        Property::Float(0.0),
        Property::Float(0.0),
    ]);
    println!("{:?}", g0.elements[1].data);
    println!("{:#?}", g0.elements[1].prop_infos);
    assert_eq!(g0.elements[1].iter().collect::<Vec<_>>(), &[
        Property::Float(3.0),
        Property::Float(5.0),
        Property::Float(8.0),
    ]);
    assert_eq!(g0.elements[2].iter().collect::<Vec<_>>(), &[
        Property::Float(1.942),
        Property::Float(152.99),
        Property::Float(0.007),
    ]);

    let g1 = &groups[1];
    assert_eq!(g1.def.name, "face");
    assert_eq!(g1.def.count, 1);
    assert_eq!(g1.def.property_defs[PropIndex(0)].ty, PropertyType::List {
        len_type: ListLenType::UChar,
        scalar_type: ScalarType::UInt,
    });
    assert_eq!(g1.def.property_defs[PropIndex(0)].name, "vertex_indices");

    assert_eq!(
        g1.elements[0].iter().collect::<Vec<_>>(),
        &[Property::UIntList(SmallVec::from_vec(vec![0, 1, 2]))],
    );
}

#[test]
fn read_raw_triangle_bbe() -> Result<(), Error> {
    let input = include_test_file!("triangle_bbe.ply");
    let res = Reader::new(input)?.into_raw_result()?;

    check_triangle(&res);

    Ok(())
}

#[test]
fn read_raw_triangle_ascii() -> Result<(), Error> {
    let input = include_test_file!("triangle_ascii.ply");
    let res = Reader::new(input)?.into_raw_result()?;

    check_triangle(&res);

    Ok(())
}
