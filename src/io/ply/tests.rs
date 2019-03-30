use cgmath::{Point3, Vector3};
use failure::Error;

use crate::{
    self as lox, // for proc-macros
    mesh, MemSource, MemSink,
    prelude::*,
    ds::SharedVertexMesh,
    fat::MiniMesh,
    io::IsFormat,
    map::VecMap,
};
use super::{
    Config, Encoding, Reader, is_file_start,
    raw::{PropertyType, ScalarType, PropIndex, Property, ListLenType, RawOffset, RawStorage},
};

// ===========================================================================
// ===== Utilities
// ===========================================================================
#[test]
fn test_is_file_start() {
    assert_eq!(is_file_start(b""), IsFormat::No);
    assert_eq!(is_file_start(b"plyx"), IsFormat::No);
    assert_eq!(is_file_start(b"plx\n"), IsFormat::No);

    assert_eq!(is_file_start(b"ply\n"), IsFormat::Probably);
    assert_eq!(is_file_start(b"ply\nfoo"), IsFormat::Probably);
    assert_eq!(is_file_start(b"ply\ncomment"), IsFormat::Probably);
    assert_eq!(is_file_start(b"ply\nformat"), IsFormat::Probably);
}

// ===========================================================================
// ===== Writing
// ===========================================================================

fn triangle_mesh() -> MiniMesh<SharedVertexMesh> {
    let (mesh, vertex_positions) = mesh! {
        type: SharedVertexMesh,
        vertices: [
            v0: (Point3::new(0.0f32, 0.0, 0.0)),
            v1: (Point3::new(3.0, 5.0, 8.0)),
            v2: (Point3::new(1.942, 152.99, 0.007)),
        ],
        faces: [
            [v0, v1, v2],
        ],
    };

    MiniMesh { mesh, vertex_positions }
}

fn to_mem(config: Config, mesh: &impl MemSource) -> Result<Vec<u8>, Error> {
    let mut v = Vec::new();
    config.into_writer(&mut v).transfer_from(mesh)?;
    Ok(v)
}

#[test]
fn write_triangle_ascii() -> Result<(), Error> {
    let res = to_mem(Config::ascii(), &triangle_mesh())?;
    assert_eq_file!(&res, "triangle_ascii.ply");
    Ok(())
}

#[test]
fn write_triangle_bbe() -> Result<(), Error> {
    let res = to_mem(Config::new(Encoding::BinaryBigEndian), &triangle_mesh())?;
    assert_eq_file!(&res, "triangle_bbe.ply");
    Ok(())
}

#[test]
fn write_triangle_ble() -> Result<(), Error> {
    let res = to_mem(Config::new(Encoding::BinaryLittleEndian), &triangle_mesh())?;
    assert_eq_file!(&res, "triangle_ble.ply");
    Ok(())
}


#[test]
fn write_triangle_with_comments_ascii() -> Result<(), Error> {
    let config = Config::ascii()
        .add_comment("My name is Tom")
        .add_comment("Yes we can have multiple comments :)")
        .add_comment(
            "Comments appear in the file in the same order as they are added with `add_comment`"
        );
    let res = to_mem(config, &triangle_mesh())?;
    assert_eq_file!(&res, "triangle_with_comments_ascii.ply");
    Ok(())
}

#[test]
fn write_triangle_with_comments_bbe() -> Result<(), Error> {
    let config = Config::new(Encoding::BinaryBigEndian)
        .add_comment("My name is Tom")
        .add_comment("Yes we can have multiple comments :)")
        .add_comment(
            "Comments appear in the file in the same order as they are added with `add_comment`"
        );
    let res = to_mem(config, &triangle_mesh())?;
    assert_eq_file!(&res, "triangle_with_comments_bbe.ply");
    Ok(())
}

#[test]
fn write_triangle_with_comments_ble() -> Result<(), Error> {
    let config = Config::new(Encoding::BinaryLittleEndian)
        .add_comment("My name is Tom")
        .add_comment("Yes we can have multiple comments :)")
        .add_comment(
            "Comments appear in the file in the same order as they are added with `add_comment`"
        );
    let res = to_mem(config, &triangle_mesh())?;
    assert_eq_file!(&res, "triangle_with_comments_ble.ply");
    Ok(())
}

#[derive(Empty, MemSink, MemSource, Debug)]
struct FullMesh {
    #[lox(core_mesh)]
    mesh: SharedVertexMesh,

    #[lox(vertex_position)]
    vertex_positions: VecMap<VertexHandle, Point3<f64>>,

    #[lox(vertex_normal)]
    vertex_normals: VecMap<VertexHandle, Vector3<f32>>,

    #[lox(vertex_color)]
    vertex_colors: VecMap<VertexHandle, [u8; 3]>,

    #[lox(face_normal)]
    face_normals: VecMap<FaceHandle, Vector3<f32>>,

    #[lox(face_color)]
    face_colors: VecMap<FaceHandle, [u8; 4]>,
}

fn three_tris_all_props() -> FullMesh {
    //
    //    (1)       (3)
    //     | \     / |
    //     |  \   /  |
    //     |   (4)   |
    //     |  /   \  |
    //     | /     \ |
    //    (0)-------(2)
    //
    let (
        mesh,
        vertex_positions,
        vertex_normals,
        vertex_colors,
        face_normals,
        face_colors
    ) = mesh! {
        type: SharedVertexMesh,
        vertices: [
            v0: (
                Point3::new(0.011, 0.021, 0.031),
                Vector3::new(0.011, 0.021, 1.031),
                [0, 101, 202],
            ),
            v1: (
                Point3::new(0.012, 1.022, 0.032),
                Vector3::new(0.012, 0.022, 1.032),
                [3, 104, 205]
            ),
            v2: (
                Point3::new(1.013, 0.023, 0.033),
                Vector3::new(0.013, 0.023, 1.033),
                [6, 107, 208]
            ),
            v3: (
                Point3::new(1.014, 1.024, 0.034),
                Vector3::new(0.014, 0.024, 1.034),
                [9, 110, 211]
            ),
            v4: (
                Point3::new(0.515, 0.525, 0.035),
                Vector3::new(0.015, 0.025, 1.035),
                [12, 113, 214]
            ),
        ],
        faces: [
            [v0, v2, v4]: (Vector3::new(0.041, 0.051, 1.061), [15, 116, 217, 224]),
            [v0, v4, v1]: (Vector3::new(0.042, 0.052, 1.062), [18, 119, 220, 225]),
            [v2, v3, v4]: (Vector3::new(0.043, 0.053, 1.063), [21, 122, 223, 226]),
        ],
    };

    FullMesh { mesh, vertex_positions, vertex_normals, vertex_colors, face_normals, face_colors }
}

fn check_three_tris_all_props(m: &FullMesh) {
    let vh = VertexHandle::new;
    let fh = FaceHandle::new;
    let (v0, v1, v2, v3, v4) = (vh(0), vh(1), vh(2), vh(3), vh(4));
    let (f0, f1, f2) = (fh(0), fh(1), fh(2));

    // Mesh and connectivity
    assert_eq!(m.mesh.num_vertices(), 5);
    assert_eq!(m.mesh.num_faces(), 3);

    assert_eq!(m.mesh.vertex_handles().collect::<Vec<_>>(), [v0, v1, v2, v3, v4]);
    assert_eq!(m.mesh.face_handles().collect::<Vec<_>>(), [f0, f1, f2]);

    assert_eq!(m.mesh.vertices_around_triangle(f0), [v0, v2, v4]);
    assert_eq!(m.mesh.vertices_around_triangle(f1), [v0, v4, v1]);
    assert_eq!(m.mesh.vertices_around_triangle(f2), [v2, v3, v4]);

    // Vertex props
    assert_eq!(m.vertex_positions.num_elements(), 5);
    assert_eq!(m.vertex_positions.get_ref(v0), Some(&Point3::new(0.011, 0.021, 0.031)));
    assert_eq!(m.vertex_positions.get_ref(v1), Some(&Point3::new(0.012, 1.022, 0.032)));
    assert_eq!(m.vertex_positions.get_ref(v2), Some(&Point3::new(1.013, 0.023, 0.033)));
    assert_eq!(m.vertex_positions.get_ref(v3), Some(&Point3::new(1.014, 1.024, 0.034)));
    assert_eq!(m.vertex_positions.get_ref(v4), Some(&Point3::new(0.515, 0.525, 0.035)));

    assert_eq!(m.vertex_normals.num_elements(), 5);
    assert_eq!(m.vertex_normals.get_ref(v0), Some(&Vector3::new(0.011, 0.021, 1.031)));
    assert_eq!(m.vertex_normals.get_ref(v1), Some(&Vector3::new(0.012, 0.022, 1.032)));
    assert_eq!(m.vertex_normals.get_ref(v2), Some(&Vector3::new(0.013, 0.023, 1.033)));
    assert_eq!(m.vertex_normals.get_ref(v3), Some(&Vector3::new(0.014, 0.024, 1.034)));
    assert_eq!(m.vertex_normals.get_ref(v4), Some(&Vector3::new(0.015, 0.025, 1.035)));

    assert_eq!(m.vertex_colors.num_elements(), 5);
    assert_eq!(m.vertex_colors.get_ref(v0), Some(&[0, 101, 202]));
    assert_eq!(m.vertex_colors.get_ref(v1), Some(&[3, 104, 205]));
    assert_eq!(m.vertex_colors.get_ref(v2), Some(&[6, 107, 208]));
    assert_eq!(m.vertex_colors.get_ref(v3), Some(&[9, 110, 211]));
    assert_eq!(m.vertex_colors.get_ref(v4), Some(&[12, 113, 214]));

    // Face props
    assert_eq!(m.face_normals.num_elements(), 3);
    assert_eq!(m.face_normals.get_ref(f0), Some(&Vector3::new(0.041, 0.051, 1.061)));
    assert_eq!(m.face_normals.get_ref(f1), Some(&Vector3::new(0.042, 0.052, 1.062)));
    assert_eq!(m.face_normals.get_ref(f2), Some(&Vector3::new(0.043, 0.053, 1.063)));

    assert_eq!(m.face_colors.num_elements(), 3);
    assert_eq!(m.face_colors.get_ref(f0), Some(&[15, 116, 217, 224]));
    assert_eq!(m.face_colors.get_ref(f1), Some(&[18, 119, 220, 225]));
    assert_eq!(m.face_colors.get_ref(f2), Some(&[21, 122, 223, 226]));
}

#[test]
fn write_three_tris_all_props_ascii() -> Result<(), Error> {
    let res = to_mem(Config::ascii(), &three_tris_all_props())?;
    assert_eq_file!(&res, "three_tris_all_props_ascii.ply");
    Ok(())
}

#[test]
fn write_three_tris_all_props_ble() -> Result<(), Error> {
    let res = to_mem(Config::new(Encoding::BinaryLittleEndian), &three_tris_all_props())?;
    assert_eq_file!(&res, "three_tris_all_props_ble.ply");
    Ok(())
}

#[test]
fn write_three_tris_all_props_bbe() -> Result<(), Error> {
    let res = to_mem(Config::new(Encoding::BinaryBigEndian), &three_tris_all_props())?;
    assert_eq_file!(&res, "three_tris_all_props_bbe.ply");
    Ok(())
}


// TODO: add test with mesh with some deleted faces & vertices


// ===========================================================================
// ===== Reading
// ===========================================================================

fn check_triangle(res: &RawStorage) {
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
        &[Property::UIntList(vec![0, 1, 2].into())],
    );
}

#[test]
fn read_raw_triangle_bbe() -> Result<(), Error> {
    let input = include_test_file!("triangle_bbe.ply");
    let res = Reader::new(input)?.into_raw_storage()?;

    check_triangle(&res);

    Ok(())
}

#[test]
fn read_raw_triangle_ble() -> Result<(), Error> {
    let input = include_test_file!("triangle_ble.ply");
    let res = Reader::new(input)?.into_raw_storage()?;

    check_triangle(&res);

    Ok(())
}

#[test]
fn read_raw_triangle_ascii() -> Result<(), Error> {
    let input = include_test_file!("triangle_ascii.ply");
    let res = Reader::new(input)?.into_raw_storage()?;

    check_triangle(&res);

    Ok(())
}

/// We only need one test here (instead of one for ble, bbe and ascii) since
/// the raw data was already checked above (in the `raw` tests).
#[test]
fn read_triangle() -> Result<(), Error> {
    let input = include_test_file!("triangle_ble.ply");
    let m = MiniMesh::<SharedVertexMesh>::create_from(Reader::new(input)?)?;

    // Check the mesh and properties
    let vh = VertexHandle::new;
    let fh = FaceHandle::new;

    assert_eq!(m.mesh.num_vertices(), 3);
    assert_eq!(m.mesh.num_faces(), 1);
    assert_eq!(m.mesh.vertices_around_triangle(fh(0)), [vh(0), vh(1), vh(2)]);

    assert_eq!(m.vertex_positions.get_ref(vh(0)), Some(&Point3::new(0.0, 0.0, 0.0)));
    assert_eq!(m.vertex_positions.get_ref(vh(1)), Some(&Point3::new(3.0, 5.0, 8.0)));
    assert_eq!(m.vertex_positions.get_ref(vh(2)), Some(&Point3::new(1.942, 152.99, 0.007)));

    Ok(())
}

fn check_triangle_extra_props(res: &RawStorage) {
    let groups = &res.element_groups;
    assert_eq!(groups.len(), 2);

    // ===== VERTEX definition ===============================================
    let g0 = &groups[0];
    assert_eq!(g0.def.name, "vertex");
    assert_eq!(g0.def.count, 3);
    assert_eq!(g0.def.property_defs.len(), 8);
    assert_eq!(g0.def.property_defs[PropIndex(0)].name, "x");
    assert_eq!(g0.def.property_defs[PropIndex(0)].ty, PropertyType::Scalar(ScalarType::Float));
    assert_eq!(g0.def.property_defs[PropIndex(1)].name, "y");
    assert_eq!(g0.def.property_defs[PropIndex(1)].ty, PropertyType::Scalar(ScalarType::Float));
    assert_eq!(g0.def.property_defs[PropIndex(2)].name, "z");
    assert_eq!(g0.def.property_defs[PropIndex(2)].ty, PropertyType::Scalar(ScalarType::Float));

    assert_eq!(g0.def.property_defs[PropIndex(3)].name, "foo[0]");
    assert_eq!(g0.def.property_defs[PropIndex(3)].ty, PropertyType::Scalar(ScalarType::Double));
    assert_eq!(g0.def.property_defs[PropIndex(4)].name, "foo[1]");
    assert_eq!(g0.def.property_defs[PropIndex(4)].ty, PropertyType::Scalar(ScalarType::Double));
    assert_eq!(g0.def.property_defs[PropIndex(5)].name, "foo[2]");
    assert_eq!(g0.def.property_defs[PropIndex(5)].ty, PropertyType::Scalar(ScalarType::Double));

    assert_eq!(g0.def.property_defs[PropIndex(6)].name, "bar");
    assert_eq!(g0.def.property_defs[PropIndex(6)].ty, PropertyType::List {
        len_type: ListLenType::UInt,
        scalar_type: ScalarType::Char,
    });

    assert_eq!(g0.def.property_defs[PropIndex(7)].name, "baz");
    assert_eq!(g0.def.property_defs[PropIndex(7)].ty, PropertyType::Scalar(ScalarType::UShort));

    // ===== VERTEX 0 =====
    println!("{:?}", g0.elements[0].data);
    println!("{:#?}", g0.elements[0].prop_infos);
    assert_eq!(g0.elements[0].prop_infos[PropIndex(0)].offset, RawOffset(0));
    assert_eq!(g0.elements[0].prop_infos[PropIndex(1)].offset, RawOffset(4));
    assert_eq!(g0.elements[0].prop_infos[PropIndex(2)].offset, RawOffset(8));
    assert_eq!(g0.elements[0].prop_infos[PropIndex(3)].offset, RawOffset(12));
    assert_eq!(g0.elements[0].prop_infos[PropIndex(4)].offset, RawOffset(20));
    assert_eq!(g0.elements[0].prop_infos[PropIndex(5)].offset, RawOffset(28));
    assert_eq!(g0.elements[0].prop_infos[PropIndex(6)].offset, RawOffset(36));
    assert_eq!(g0.elements[0].prop_infos[PropIndex(7)].offset, RawOffset(40));
    assert_eq!(g0.elements[0].iter().collect::<Vec<_>>(), &[
        Property::Float(0.0),
        Property::Float(0.0),
        Property::Float(0.0),
        Property::Double(0.93),
        Property::Double(0.2),
        Property::Double(0.3),
        Property::CharList(vec![].into()),
        Property::UShort(0),
    ]);

    // ===== VERTEX 1 =====
    assert_eq!(g0.elements[1].prop_infos[PropIndex(0)].offset, RawOffset(0));
    assert_eq!(g0.elements[1].prop_infos[PropIndex(1)].offset, RawOffset(4));
    assert_eq!(g0.elements[1].prop_infos[PropIndex(2)].offset, RawOffset(8));
    assert_eq!(g0.elements[1].prop_infos[PropIndex(3)].offset, RawOffset(12));
    assert_eq!(g0.elements[1].prop_infos[PropIndex(4)].offset, RawOffset(20));
    assert_eq!(g0.elements[1].prop_infos[PropIndex(5)].offset, RawOffset(28));
    assert_eq!(g0.elements[1].prop_infos[PropIndex(6)].offset, RawOffset(36));
    assert_eq!(g0.elements[1].prop_infos[PropIndex(7)].offset, RawOffset(41));
    assert_eq!(g0.elements[1].iter().collect::<Vec<_>>(), &[
        Property::Float(3.0),
        Property::Float(5.0),
        Property::Float(8.0),
        Property::Double(0.93),
        Property::Double(0.2),
        Property::Double(0.3),
        Property::CharList(vec![-1].into()),
        Property::UShort(3),
    ]);

    // ===== VERTEX 2 =====
    assert_eq!(g0.elements[2].prop_infos[PropIndex(0)].offset, RawOffset(0));
    assert_eq!(g0.elements[2].prop_infos[PropIndex(1)].offset, RawOffset(4));
    assert_eq!(g0.elements[2].prop_infos[PropIndex(2)].offset, RawOffset(8));
    assert_eq!(g0.elements[2].prop_infos[PropIndex(3)].offset, RawOffset(12));
    assert_eq!(g0.elements[2].prop_infos[PropIndex(4)].offset, RawOffset(20));
    assert_eq!(g0.elements[2].prop_infos[PropIndex(5)].offset, RawOffset(28));
    assert_eq!(g0.elements[2].prop_infos[PropIndex(6)].offset, RawOffset(36));
    assert_eq!(g0.elements[2].prop_infos[PropIndex(7)].offset, RawOffset(42));
    assert_eq!(g0.elements[2].iter().collect::<Vec<_>>(), &[
        Property::Float(1.942),
        Property::Float(152.99),
        Property::Float(0.007),
        Property::Double(0.93),
        Property::Double(0.2),
        Property::Double(0.3),
        Property::CharList(vec![3, 8].into()),
        Property::UShort(6),
    ]);


    // ===== FACE definition ===============================================
    let g1 = &groups[1];
    assert_eq!(g1.def.name, "face");
    assert_eq!(g1.def.count, 1);
    assert_eq!(g1.def.property_defs.len(), 2);

    assert_eq!(g1.def.property_defs[PropIndex(0)].name, "vertex_indices");
    assert_eq!(g1.def.property_defs[PropIndex(0)].ty, PropertyType::List {
        len_type: ListLenType::UChar,
        scalar_type: ScalarType::UInt,
    });

    assert_eq!(g1.def.property_defs[PropIndex(1)].name, "cats");
    assert_eq!(g1.def.property_defs[PropIndex(1)].ty, PropertyType::Scalar(ScalarType::Float));

    assert_eq!(g1.elements[0].prop_infos[PropIndex(0)].offset, RawOffset(0));
    assert_eq!(g1.elements[0].prop_infos[PropIndex(1)].offset, RawOffset(13));
    assert_eq!(g1.elements[0].iter().collect::<Vec<_>>(), &[
        Property::UIntList(vec![0, 1, 2].into()),
        Property::Float(-99.123),
    ]);
}

#[test]
fn read_raw_triangle_with_extra_props_bbe() -> Result<(), Error> {
    let input = include_test_file!("triangle_with_extra_props_bbe.ply");
    let res = Reader::new(input)?.into_raw_storage()?;

    check_triangle_extra_props(&res);

    Ok(())
}


#[test]
fn read_raw_triangle_with_extra_props_ble() -> Result<(), Error> {
    let input = include_test_file!("triangle_with_extra_props_ble.ply");
    let res = Reader::new(input)?.into_raw_storage()?;

    check_triangle_extra_props(&res);

    Ok(())
}


#[test]
fn read_raw_triangle_with_extra_props_ascii() -> Result<(), Error> {
    let input = include_test_file!("triangle_with_extra_props_ascii.ply");
    let res = Reader::new(input)?.into_raw_storage()?;

    check_triangle_extra_props(&res);

    Ok(())
}

/// We only need one test here (instead of one for ble, bbe and ascii) since
/// the raw data was already checked above (in the `raw` tests).
#[test]
fn read_triangle_with_extra_props_mini_mesh() -> Result<(), Error> {
    let input = include_test_file!("triangle_with_extra_props_ble.ply");
    let m = MiniMesh::<SharedVertexMesh>::create_from(Reader::new(input)?)?;

    // Check the mesh and properties
    let vh = VertexHandle::new;
    let fh = FaceHandle::new;

    assert_eq!(m.mesh.num_vertices(), 3);
    assert_eq!(m.mesh.num_faces(), 1);
    assert_eq!(m.mesh.vertices_around_triangle(fh(0)), [vh(0), vh(1), vh(2)]);

    assert_eq!(m.vertex_positions.get_ref(vh(0)), Some(&Point3::new(0.0, 0.0, 0.0)));
    assert_eq!(m.vertex_positions.get_ref(vh(1)), Some(&Point3::new(3.0, 5.0, 8.0)));
    assert_eq!(m.vertex_positions.get_ref(vh(2)), Some(&Point3::new(1.942, 152.99, 0.007)));

    Ok(())
}

#[test]
fn read_three_tris_all_props_ascii() -> Result<(), Error> {
    let input = include_test_file!("three_tris_all_props_ascii.ply");
    let m = FullMesh::create_from(Reader::new(input)?)?;
    check_three_tris_all_props(&m);
    Ok(())
}

#[test]
fn read_three_tris_all_props_ble() -> Result<(), Error> {
    let input = include_test_file!("three_tris_all_props_ble.ply");
    let m = FullMesh::create_from(Reader::new(input)?)?;
    check_three_tris_all_props(&m);
    Ok(())
}

#[test]
fn read_three_tris_all_props_bbe() -> Result<(), Error> {
    let input = include_test_file!("three_tris_all_props_bbe.ply");
    let m = FullMesh::create_from(Reader::new(input)?)?;
    check_three_tris_all_props(&m);
    Ok(())
}

// TODO: read tests where the mesh hands out strange handles
