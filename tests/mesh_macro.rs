//! Tests related to the `mesh!` macro.

#![feature(proc_macro_non_items)]

use lox::{
    mesh, ExplicitFace, ExplicitVertex, MeshUnsorted,
    ds::SharedVertexMesh,
};


#[test]
fn empty_mesh() {
    let mesh = mesh! {
        type: SharedVertexMesh,
        vertices: [],
        faces: [],
    };

    assert_eq!(mesh.num_vertices(), 0);
    assert_eq!(mesh.num_faces(), 0);
}

#[test]
fn triangle() {
    let mesh = mesh! {
        type: SharedVertexMesh,
        vertices: [v0, v1, v2],
        faces: [[v0, v1, v2]],
    };

    assert_eq!(mesh.num_vertices(), 3);
    assert_eq!(mesh.num_faces(), 1);

    check_vertices_of_face_are_unique(&mesh);
}

#[test]
fn triangle_verbose() {
    let mesh = mesh! {
        type: SharedVertexMesh,
        vertices: [
            v0: (),
            v1,
            v2: (),
        ],
        faces: [
            [v0, v1, v2]: (),
        ],
    };

    assert_eq!(mesh.num_vertices(), 3);
    assert_eq!(mesh.num_faces(), 1);

    check_vertices_of_face_are_unique(&mesh);
}

#[test]
fn triangle_with_prop() {
    let (mesh, props) = mesh! {
        type: SharedVertexMesh,
        vertices: [
            v0: (10),
            v1: (20),
            v2: (30),
        ],
        faces: [[v0, v1, v2]],
    };

    assert_eq!(mesh.num_vertices(), 3);
    assert_eq!(mesh.num_faces(), 1);

    check_vertices_of_face_are_unique(&mesh);

    let face = mesh.faces().next().unwrap();
    let [va, vb, vc] = mesh.vertices_of_face(face.handle());

    let valid_props = [10, 20, 30];
    assert!(valid_props.contains(&props[va]));
    assert!(valid_props.contains(&props[vb]));
    assert!(valid_props.contains(&props[vc]));
}

#[test]
fn rectangle_multi_props() {
    let (mesh, _positions, _labels, names) = mesh! {
        type: SharedVertexMesh,
        vertices: [
            v0: (3.0, 'x'),
            v1: (5.0, 'y'),
            v2: (1.0, 'z'),
            v3: (8.2, 'w'),
        ],
        faces: [
            [v0, v1, v2]: ("one"),
            [v1, v2, v3]: ("two"),
        ],
    };


    assert_eq!(mesh.num_vertices(), 4);
    assert_eq!(mesh.num_faces(), 2);

    // TODO: check number of entries in property maps

    check_vertices_of_face_are_unique(&mesh);

    let valid_names = ["one", "two"];
    for face in mesh.faces() {
        assert!(valid_names.contains(&names[face.handle()]));
    }
}

fn check_vertices_of_face_are_unique(mesh: &(impl MeshUnsorted + ExplicitFace)) {
    for face in mesh.faces() {
        let [va, vb, vc] = mesh.vertices_of_face(face.handle());
        assert_ne!(va, vb);
        assert_ne!(va, vc);
        assert_ne!(vb, vc);
    }
}