//! Tests the custom derive for `Empty`.
//!
//! We use complete path everywhere and avoid imports to make sure when the
//! derive code falsely assumes a symbol is in scope.


#[test]
fn unit_like() {
    #[derive(lox::Empty)]
    struct UnitLike;

    let _ = <UnitLike as lox::traits::Empty>::empty();
}


#[test]
fn unnamed() {
    #[derive(lox::Empty)]
    struct UnnamedEmpty();

    #[derive(lox::Empty)]
    struct UnnamedSingle(lox::ds::SharedVertexMesh);

    #[derive(lox::Empty)]
    struct UnnamedTwo(lox::ds::SharedVertexMesh, lox::map::DenseMap<lox::FaceHandle, u32>);


    let _ = <UnnamedEmpty as lox::traits::Empty>::empty();
    let _ = <UnnamedSingle as lox::traits::Empty>::empty();
    let _ = <UnnamedTwo as lox::traits::Empty>::empty();
}


#[test]
fn named() {
    #![allow(dead_code)]

    #[derive(lox::Empty)]
    struct NamedEmpty {}

    #[derive(lox::Empty)]
    struct NamedSingle {
        mesh: lox::ds::SharedVertexMesh,
    }

    #[derive(lox::Empty)]
    struct NamedTwo {
        mesh: lox::ds::SharedVertexMesh,
        stuff: lox::map::DenseMap<lox::FaceHandle, u32>,
    }


    let _ = <NamedEmpty as lox::traits::Empty>::empty();
    let _ = <NamedSingle as lox::traits::Empty>::empty();
    let _ = <NamedTwo as lox::traits::Empty>::empty();
}
