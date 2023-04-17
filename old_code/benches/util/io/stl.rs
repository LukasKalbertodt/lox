use lox::{
    prelude::*,
    cgmath::prelude::*,
    io::stl::RawTriangle,
};


pub fn raw_sphere() -> Vec<RawTriangle> {
    let src = super::sphere();
    src.mesh.face_handles().map(|fh| {
        let [va, vb, vc] = src.mesh.vertices_around_triangle(fh);

        let get_v = |vh| -> [f32; 3] {
            src.vertex_position::<f32>(vh)
                .expect("unexpected error in sphere")
                .expect("no vertex in sphere")
                .convert()  // to array form
        };
        let vertices = [get_v(va), get_v(vb), get_v(vc)];

        RawTriangle {
            vertices,
            normal: calc_normal(&vertices),
            attribute_byte_count: 0,
        }
    }).collect()
}

fn calc_normal(positions: &[[f32; 3]; 3]) -> [f32; 3] {
    let pos_a = positions[0].to_point3();
    let pos_b = positions[1].to_point3();
    let pos_c = positions[2].to_point3();

    let normal = (pos_b - pos_a).cross(pos_c - pos_a).normalize();
    [normal.x, normal.y, normal.z]
}
