//! TODO

use criterion::{
    criterion_group, criterion_main, black_box, Criterion,
};

use lox::{
    prelude::*,
    ds::DirectedEdgeMesh,
    fat::MiniMesh,
    shape::Sphere,
};





// ===============================================================================================
// ===== Benchmarks
// ===============================================================================================

/// Count the adjacent faces per vertex
fn dem_count_adjacent_faces(c: &mut Criterion) {
    c.bench_function(
        "dem_count_adjacent_faces",
        |b| {
            let mesh = MiniMesh::<DirectedEdgeMesh>::create_from(Sphere {
                num_latitudes: 64,
                num_longitudes: 96,
                .. Sphere::default()
            }).unwrap();

            b.iter(|| {
                let mesh = black_box(&mesh.mesh);

                for v in mesh.vertices() {
                    black_box(v.adjacent_faces().count());
                }
            })
        },
    );

}


criterion_group!(benches,
    dem_count_adjacent_faces,
);
criterion_main!(benches);
