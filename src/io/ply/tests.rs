use cgmath::{Point3, Vector3};
use failure::Error;

use crate::{
    mesh, MemSource, MemSink,
    prelude::*,
    ds::{
        HalfEdgeMesh, SharedVertexMesh,
        half_edge::PolyConfig,
    },
    fat::{MiniMesh, AnyMesh, any::{AnyPointMap, AnyVectorMap, AnyColorMap}},
    io::IsFormat,
    map::DenseMap,
};
use super::{
    Config, Encoding, Reader, is_file_start,
    raw::{PropertyType, ScalarType, PropIndex, Property, ListLenType, RawOffset, RawStorage},
};

// ===========================================================================
// ===== Test Utilities
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
// ===== Helper macros to avoid code duplication
// ===========================================================================

/// This abstract macro takes the name of another macro and invokes it three
/// times, with the identifiers `ascii`, `ble` and `bbe` as first argument.
///
/// It also accepts arbitarily any other args that are forwarded to the
/// `generator` macro.
macro_rules! gen_for_encodings {
    ($generator:ident $(, $args:ident)*) => {
        $generator!(ascii $(, $args)*);
        $generator!(ble $(, $args)*);
        $generator!(bbe $(, $args)*);
    };
}

macro_rules! encoding_config {
    (ascii) => { Config::ascii() };
    (ble) => { Config::new(Encoding::BinaryLittleEndian) };
    (bbe) => { Config::new(Encoding::BinaryBigEndian) };
}


// ===========================================================================
// ===== Test mesh definitions
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


#[derive(Empty, MemSink, MemSource, Debug)]
struct MeshVpVnVcFnFc {
    #[lox(core_mesh)]
    mesh: SharedVertexMesh,

    #[lox(vertex_position)]
    vertex_positions: DenseMap<VertexHandle, Point3<f64>>,

    #[lox(vertex_normal)]
    vertex_normals: DenseMap<VertexHandle, Vector3<f32>>,

    #[lox(vertex_color)]
    vertex_colors: DenseMap<VertexHandle, [u8; 3]>,

    #[lox(face_normal)]
    face_normals: DenseMap<FaceHandle, Vector3<f32>>,

    #[lox(face_color)]
    face_colors: DenseMap<FaceHandle, [u8; 4]>,
}

fn three_tris_many_props() -> MeshVpVnVcFnFc {
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

    MeshVpVnVcFnFc {
        mesh, vertex_positions, vertex_normals, vertex_colors, face_normals, face_colors
    }
}

fn check_three_tris_many_props(m: &AnyMesh) {
    let vh = VertexHandle::new;
    let fh = FaceHandle::new;
    let (v0, v1, v2, v3, v4) = (vh(0), vh(1), vh(2), vh(3), vh(4));
    let (f0, f1, f2) = (fh(0), fh(1), fh(2));

    // Mesh and connectivity
    assert_eq!(m.mesh.num_vertices(), 5);
    assert_eq!(m.mesh.num_faces(), 3);

    assert_eq!(m.mesh.vertex_handles().collect::<Vec<_>>(), [v0, v1, v2, v3, v4]);
    assert_eq!(m.mesh.face_handles().collect::<Vec<_>>(), [f0, f1, f2]);

    assert_rotated_eq!(m.mesh.vertices_around_face(f0).collect::<Vec<_>>(), [v0, v2, v4]);
    assert_rotated_eq!(m.mesh.vertices_around_face(f1).collect::<Vec<_>>(), [v0, v4, v1]);
    assert_rotated_eq!(m.mesh.vertices_around_face(f2).collect::<Vec<_>>(), [v2, v3, v4]);

    // Vertex props
    assert_any_prop!(m.vertex_positions, AnyPointMap::Float64, [
        v0 => Point3::new(0.011, 0.021, 0.031),
        v1 => Point3::new(0.012, 1.022, 0.032),
        v2 => Point3::new(1.013, 0.023, 0.033),
        v3 => Point3::new(1.014, 1.024, 0.034),
        v4 => Point3::new(0.515, 0.525, 0.035),
    ]);
    assert_any_prop!(m.vertex_normals, AnyVectorMap::Float32, [
        v0 => Vector3::new(0.011, 0.021, 1.031),
        v1 => Vector3::new(0.012, 0.022, 1.032),
        v2 => Vector3::new(0.013, 0.023, 1.033),
        v3 => Vector3::new(0.014, 0.024, 1.034),
        v4 => Vector3::new(0.015, 0.025, 1.035),
    ]);
    assert_any_prop!(m.vertex_colors, AnyColorMap::RgbUint8, [
        v0 => [0, 101, 202],
        v1 => [3, 104, 205],
        v2 => [6, 107, 208],
        v3 => [9, 110, 211],
        v4 => [12, 113, 214],
    ]);

    // Face props
    assert_any_prop!(m.face_normals, AnyVectorMap::Float32, [
        f0 => Vector3::new(0.041, 0.051, 1.061),
        f1 => Vector3::new(0.042, 0.052, 1.062),
        f2 => Vector3::new(0.043, 0.053, 1.063),
    ]);
    assert_any_prop!(m.face_colors, AnyColorMap::RgbaUint8, [
        f0 => [15, 116, 217, 224],
        f1 => [18, 119, 220, 225],
        f2 => [21, 122, 223, 226],
    ]);
}


#[derive(Empty, MemSink, MemSource, Debug)]
struct MeshVpFn {
    #[lox(core_mesh)]
    mesh: HalfEdgeMesh<PolyConfig>,

    #[lox(vertex_position)]
    vertex_positions: DenseMap<VertexHandle, Point3<f64>>,

    #[lox(face_normal)]
    face_normals: DenseMap<FaceHandle, Vector3<f32>>,
}

fn half_cube_with_normals() -> MeshVpFn {
    //
    //    (0) ----- (1)
    //     |         |
    //     |         |
    //     |         |
    //    (2) ----- (3) ----- (1)
    //     |         |         |
    //     |         |         |
    //     |         |         |
    //    (4) ----- (5) ----- (6)
    //
    let (mesh, vertex_positions, face_normals) = mesh! {
        type: HalfEdgeMesh<PolyConfig>,
        vertices: [
            v0: (Point3::new(0.011, 1.021, 1.031)),
            v1: (Point3::new(1.012, 1.022, 1.032)),
            v2: (Point3::new(0.013, 1.023, 0.033)),
            v3: (Point3::new(1.014, 1.024, 0.034)),
            v4: (Point3::new(0.015, 0.025, 0.035)),
            v5: (Point3::new(1.016, 0.026, 0.036)),
            v6: (Point3::new(1.017, 0.027, 1.037)),
        ],
        faces: [
            [v0, v2, v3, v1]: (Vector3::new(0.041, 0.051, 1.061)),
            [v2, v4, v5, v3]: (Vector3::new(0.042, 0.052, 1.062)),
            [v3, v5, v6, v1]: (Vector3::new(0.043, 0.053, 1.063)),
        ],
    };

    MeshVpFn { mesh, vertex_positions, face_normals }
}

fn check_half_cube_with_normals(m: &AnyMesh) {
    let vh = VertexHandle::new;
    let fh = FaceHandle::new;
    let (v0, v1, v2, v3, v4, v5, v6) = (vh(0), vh(1), vh(2), vh(3), vh(4), vh(5), vh(6));
    let (f0, f1, f2) = (fh(0), fh(1), fh(2));

    // Mesh and connectivity
    assert_eq!(m.mesh.num_vertices(), 7);
    assert_eq!(m.mesh.num_faces(), 3);

    assert_eq!(m.mesh.vertex_handles().collect::<Vec<_>>(), [v0, v1, v2, v3, v4, v5, v6]);
    assert_eq!(m.mesh.face_handles().collect::<Vec<_>>(), [f0, f1, f2]);

    assert_rotated_eq!(m.mesh.vertices_around_face(f0).collect::<Vec<_>>(), [v0, v2, v3, v1]);
    assert_rotated_eq!(m.mesh.vertices_around_face(f1).collect::<Vec<_>>(), [v2, v4, v5, v3]);
    assert_rotated_eq!(m.mesh.vertices_around_face(f2).collect::<Vec<_>>(), [v3, v5, v6, v1]);

    // Vertex position
    assert_any_prop!(m.vertex_positions, AnyPointMap::Float64, [
        v0 => Point3::new(0.011, 1.021, 1.031),
        v1 => Point3::new(1.012, 1.022, 1.032),
        v2 => Point3::new(0.013, 1.023, 0.033),
        v3 => Point3::new(1.014, 1.024, 0.034),
        v4 => Point3::new(0.015, 0.025, 0.035),
        v5 => Point3::new(1.016, 0.026, 0.036),
        v6 => Point3::new(1.017, 0.027, 1.037),
    ]);

    // Face normals
    assert_any_prop!(m.face_normals, AnyVectorMap::Float32, [
        f0 => Vector3::new(0.041, 0.051, 1.061),
        f1 => Vector3::new(0.042, 0.052, 1.062),
        f2 => Vector3::new(0.043, 0.053, 1.063),
    ]);
}


fn to_mem(config: Config, mesh: &impl MemSource) -> Result<Vec<u8>, Error> {
    let mut v = Vec::new();
    config.into_writer(&mut v).transfer_from(mesh)?;
    Ok(v)
}


// ===========================================================================
// ===== Writing
// ===========================================================================

macro_rules! write_triangle {
    ($encoding:ident) => {
        paste::item! {
            #[test]
            fn [<write_triangle_ $encoding>]() -> Result<(), Error> {
                let res = to_mem(encoding_config!($encoding), &triangle_mesh())?;
                assert_eq_file!(
                    &res,
                    concat!("triangle_", stringify!($encoding), ".ply")
                );

                Ok(())
            }
        }
    };
}

gen_for_encodings!(write_triangle);


macro_rules! write_triangle_with_comments {
    ($encoding:ident) => {
        paste::item! {
            #[test]
            fn [<write_triangle_with_comments_ $encoding>]() -> Result<(), Error> {
                let config = encoding_config!($encoding)
                    .add_comment("My name is Tom")
                    .add_comment("Yes we can have multiple comments :)")
                    .add_comment(
                        "Comments appear in the file in the same order as they \
                            are added with `add_comment`"
                    );
                let res = to_mem(config, &triangle_mesh())?;
                assert_eq_file!(
                    &res,
                    concat!("triangle_with_comments_", stringify!($encoding), ".ply")
                );

                Ok(())
            }
        }
    };
}

gen_for_encodings!(write_triangle_with_comments);


macro_rules! write_three_tris_many_props {
    ($encoding:ident) => {
        paste::item! {
            #[test]
            fn [<write_three_tris_many_props_ $encoding>]() -> Result<(), Error> {
                let res = to_mem(encoding_config!($encoding), &three_tris_many_props())?;
                assert_eq_file!(
                    &res,
                    concat!("three_tris_many_props_", stringify!($encoding), ".ply")
                );
                Ok(())
            }
        }
    };
}

gen_for_encodings!(write_three_tris_many_props);


macro_rules! write_half_cube_with_normals {
    ($encoding:ident) => {
        paste::item! {
            #[test]
            fn [<write_half_cube_with_normals_ $encoding>]() -> Result<(), Error> {
                let res = to_mem(encoding_config!($encoding), &half_cube_with_normals())?;
                assert_eq_file!(
                    &res,
                    concat!("half_cube_with_normals_", stringify!($encoding), ".ply")
                );
                Ok(())
            }
        }
    };
}

gen_for_encodings!(write_half_cube_with_normals);


#[test]
fn write_mesh_with_removed_elements() -> Result<(), Error> {
    let (mut mesh, mut vertex_positions) = mesh! {
        type: SharedVertexMesh,
        vertices: [
            v0: (Point3::new(0.1f32, 0.2, 0.3)),
            v1: (Point3::new(1.1f32, 1.2, 1.3)),
            v2: (Point3::new(2.1f32, 2.2, 2.3)),
            v3: (Point3::new(3.1f32, 3.2, 3.3)),
            v4: (Point3::new(4.1f32, 4.2, 4.3)),
        ],
        faces: [
            [v0, v1, v3],
            [v0, v3, v4],
            [v3, v1, v4],
        ],
    };

    mesh.remove_isolated_vertex(VertexHandle::new(2));
    vertex_positions.remove(VertexHandle::new(2));
    mesh.remove_face(FaceHandle::new(1));

    let mesh = MiniMesh { mesh, vertex_positions };

    let res = to_mem(Config::new(Encoding::Ascii), &mesh)?;
    assert_eq_file!(&res, "mesh_with_removed_elements.ply");
    Ok(())
}


// ===========================================================================
// ===== Reading
// ===========================================================================

macro_rules! read_raw_triangle {
    ($encoding:ident) => {
        paste::item! {
            #[test]
            fn [<read_raw_triangle_ $encoding>]() -> Result<(), Error> {
                let input = include_test_file!(
                    concat!("triangle_", stringify!($encoding), ".ply")
                );
                let res = Reader::new(input)?.into_raw_storage()?;

                check_triangle(&res);

                Ok(())
            }
        }
    };
}

gen_for_encodings!(read_raw_triangle);


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

macro_rules! read_raw_triangle_with_extra_props {
    ($encoding:ident) => {
        paste::item! {
            #[test]
            fn [<read_raw_triangle_with_extra_props_ $encoding>]() -> Result<(), Error> {
                let input = include_test_file!(
                    concat!("triangle_with_extra_props_", stringify!($encoding), ".ply")
                );
                let res = Reader::new(input)?.into_raw_storage()?;

                check_triangle_extra_props(&res);

                Ok(())
            }
        }
    };
}

gen_for_encodings!(read_raw_triangle_with_extra_props);

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


/// Test cases that read into an `AnyMesh`.
macro_rules! read_any {
    ($encoding:ident, $stem:ident) => {
        paste::item! {
            #[test]
            fn [<read_ $stem _ $encoding>] () -> Result<(), Error> {
                let input = include_test_file!(
                    concat!(stringify!($stem), "_", stringify!($encoding), ".ply")
                );
                let m = AnyMesh::create_from(Reader::new(input)?)?;
                [<check_ $stem>](&m);
                Ok(())
            }
        }
    };
}

gen_for_encodings!(read_any, three_tris_many_props);
gen_for_encodings!(read_any, half_cube_with_normals);


// TODO: read tests where the mesh hands out strange handles
