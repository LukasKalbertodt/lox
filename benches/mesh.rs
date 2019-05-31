//! TODO

use criterion::{
    criterion_group, criterion_main, black_box, Criterion,
};

use lox::{
    prelude::*,
    cgmath::Point3,
    ds::{
        DirectedEdgeMesh, HalfEdgeMesh, SharedVertexMesh,
        half_edge::TriConfig,
    },
    fat::MiniMesh,
    shape::Sphere,
};





// ===============================================================================================
// ===== Benchmarks
// ===============================================================================================

const MEDIUM_SPHERE: Sphere = Sphere {
    num_latitudes: 64,
    num_longitudes: 96,
    radius: 1.0,
    center: Point3::new(0.0, 0.0, 0.0),
};

/// Count the number of vertices
fn count_vertices(c: &mut Criterion) {
    macro_rules! gen {
        ($name:literal, $mesh:ty) => {
            c.bench_function($name, |b| {
                let mesh = MiniMesh::<$mesh>::create_from(MEDIUM_SPHERE).unwrap();

                b.iter(|| {
                    let mesh = black_box(&mesh.mesh);
                    count_iter(mesh.vertices())
                })
            });
        };
    }

    gen!("svm_count_vertices", SharedVertexMesh);
    gen!("dem_count_vertices", DirectedEdgeMesh);
    gen!("hem_count_vertices", HalfEdgeMesh<TriConfig>);
}

/// Count the number of faces
fn count_faces(c: &mut Criterion) {
    macro_rules! gen {
        ($name:literal, $mesh:ty) => {
            c.bench_function($name, |b| {
                let mesh = MiniMesh::<$mesh>::create_from(MEDIUM_SPHERE).unwrap();

                b.iter(|| {
                    let mesh = black_box(&mesh.mesh);
                    count_iter(mesh.faces())
                })
            });
        };
    }

    gen!("svm_count_faces", SharedVertexMesh);
    gen!("dem_count_faces", DirectedEdgeMesh);
    gen!("hem_count_faces", HalfEdgeMesh<TriConfig>);
}

/// Count the adjacent faces per vertex
fn count_adjacent_faces(c: &mut Criterion) {
    macro_rules! gen {
        ($name:literal, $mesh:ty) => {
            c.bench_function($name, |b| {
                let mesh = MiniMesh::<$mesh>::create_from(MEDIUM_SPHERE).unwrap();

                b.iter(|| {
                    let mesh = black_box(&mesh.mesh);

                    for v in mesh.vertices() {
                        black_box(count_iter(v.adjacent_faces()));
                    }
                })
            });
        };
    }

    gen!("dem_count_adjacent_faces", DirectedEdgeMesh);
    gen!("hem_count_adjacent_faces", HalfEdgeMesh<TriConfig>);
}


criterion_group!(benches,
    count_vertices,
    count_faces,
    count_adjacent_faces,
);
criterion_main!(benches);


// ===============================================================================================
// ===== Utilities
// ===============================================================================================

fn count_iter(it: impl Iterator) -> usize {
    let mut x = 0;
    for _ in it {
        x += 1;
    }
    x
}
