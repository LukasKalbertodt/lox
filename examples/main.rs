use lox::{
    tri_mesh,
    ds::SharedVertexMesh,
};

fn main() {
    let m: SharedVertexMesh = tri_mesh!{
        vertices: [v0, v1, v2, v3,],
        faces: [
            [v0, v1, v2],
            [v1, v2, v3],
        ],
    };

    println!("{:?}", m);
}
