//! Tests the custom derive for `Empty`.

use lox::{
    Empty, FaceHandle,
    ds::SharedVertexMesh,
    map::VecMap,
};


#[test]
fn unit_like() {
    #[derive(Empty)]
    struct UnitLike;

    let _ = UnitLike::empty();
}


#[test]
fn unnamed() {
    #[derive(Empty)]
    struct UnnamedEmpty();

    #[derive(Empty)]
    struct UnnamedSingle(SharedVertexMesh);

    #[derive(Empty)]
    struct UnnamedTwo(SharedVertexMesh, VecMap<FaceHandle, u32>);


    let _ = UnnamedEmpty::empty();
    let _ = UnnamedSingle::empty();
    let _ = UnnamedTwo::empty();
}


#[test]
fn named() {
    #![allow(dead_code)]

    #[derive(Empty)]
    struct NamedEmpty {}

    #[derive(Empty)]
    struct NamedSingle {
        mesh: SharedVertexMesh,
    }

    #[derive(Empty)]
    struct NamedTwo {
        mesh: SharedVertexMesh,
        stuff: VecMap<FaceHandle, u32>,
    }


    let _ = NamedEmpty::empty();
    let _ = NamedSingle::empty();
    let _ = NamedTwo::empty();
}
