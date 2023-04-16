//! Tests the custom derive for `Empty`.
//!
//! We use complete path everywhere and avoid imports to make sure when the
//! derive code falsely assumes a symbol is in scope.


#[test]
fn unit_like() {
    #[derive(lox::util::Empty)]
    struct UnitLike;

    let _ = <UnitLike as lox::util::Empty>::empty();
}


#[test]
fn unnamed() {
    #[derive(lox::util::Empty)]
    struct UnnamedEmpty();

    #[derive(lox::util::Empty)]
    struct UnnamedSingle(lox::core::SharedVertexMesh);

    #[derive(lox::util::Empty)]
    struct UnnamedTwo(lox::core::SharedVertexMesh, lox::map::DenseMap<lox::FaceHandle, u32>);


    let _ = <UnnamedEmpty as lox::util::Empty>::empty();
    let _ = <UnnamedSingle as lox::util::Empty>::empty();
    let _ = <UnnamedTwo as lox::util::Empty>::empty();
}


#[test]
fn named() {
    #![allow(dead_code)]

    #[derive(lox::util::Empty)]
    struct NamedEmpty {}

    #[derive(lox::util::Empty)]
    struct NamedSingle {
        mesh: lox::core::SharedVertexMesh,
    }

    #[derive(lox::util::Empty)]
    struct NamedTwo {
        mesh: lox::core::SharedVertexMesh,
        stuff: lox::map::DenseMap<lox::FaceHandle, u32>,
    }


    let _ = <NamedEmpty as lox::util::Empty>::empty();
    let _ = <NamedSingle as lox::util::Empty>::empty();
    let _ = <NamedTwo as lox::util::Empty>::empty();
}
