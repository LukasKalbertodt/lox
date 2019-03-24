//! Measures PLY IO read (parse) and write speeds.

use std::io::Cursor;
use criterion::{
    criterion_group, criterion_main, black_box, BatchSize, Criterion,
};
use cgmath::Point3;

use lox::{
    prelude::*,
    handle::hsize,
    VertexHandle, FaceHandle,
    io::{
        Error, Primitive,
        ply::{
            Reader,
            raw::{ElementDef, RawElement, RawSink},
        },
    },
    util::MeshSizeHint,
};


// ===============================================================================================
// ===== Helper utilities
// ===============================================================================================

/// A raw sink that just puts all data into the `black_box`.
struct NullRawSink;

impl RawSink for NullRawSink {
    fn element_group_start(&mut self, def: &ElementDef) -> Result<(), Error> {
        black_box(def);
        Ok(())
    }
    fn element(&mut self, elem: &RawElement) -> Result<(), Error> {
        black_box(elem);
        Ok(())
    }
}


/// A sink that puts all vertex positions into the `black_box`, ignores all
/// other properties.
struct NullSinkPos {
    vertex_count: hsize,
    face_count: hsize,
}

impl NullSinkPos {
    fn new() -> Self {
        Self {
            vertex_count: 0,
            face_count: 0,
        }
    }
}

impl MemSink for NullSinkPos {
    fn add_vertex(&mut self) -> VertexHandle {
        let out = VertexHandle::new(self.vertex_count);
        self.vertex_count += 1;
        out
    }
    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle {
        black_box(vertices);

        let out = FaceHandle::new(self.face_count);
        self.face_count += 1;
        out
    }

    fn size_hint(&mut self, hint: MeshSizeHint) {
        black_box(hint);
    }

    fn prepare_vertex_positions<N: Primitive>(&mut self, count: hsize) -> Result<(), Error> {
        black_box(count);
        Ok(())
    }
    fn set_vertex_position<N: Primitive>(
        &mut self,
        v: VertexHandle,
        position: Point3<N>,
    ) {
        black_box(v);
        black_box(position);
    }
}

// ===============================================================================================
// ===== Benchmarks
// ===============================================================================================

/// Measures body reading of `three_tris_all_props` files via `RawSink`.
fn three_tris_all_props_raw(c: &mut Criterion) {
    const FILES: [&[u8]; 3] = [
        include_bytes!("../src/io/ply/test_files/three_tris_all_props_ble.ply"),
        include_bytes!("../src/io/ply/test_files/three_tris_all_props_bbe.ply"),
        include_bytes!("../src/io/ply/test_files/three_tris_all_props_ascii.ply"),
    ];

    c.bench_function_over_inputs(
        "ply_three_tris_all_props_ble_raw",
        |b, encoding| {
            // We do this string -> index stuff here so that the resulting
            // benchmark names are more useful (`/"acsii"` than `/2`).
            let data = match *encoding {
                "ble" => FILES[0],
                "bbe" => FILES[1],
                "ascii" => FILES[2],
                _ => unreachable!(),
            };
            let reader = Reader::new(Cursor::new(data)).unwrap();
            let mut sink = NullRawSink;

            b.iter_batched(
                || reader.clone(),
                |r| r.read_raw(&mut sink),
                BatchSize::SmallInput,
            )
        },
        vec!["ble", "bbe", "ascii"],
    );
}

/// Measures body reading of `three_tris_all_props` files via `RawSink`.
fn sphere_raw(c: &mut Criterion) {
    c.bench_function_over_inputs(
        "ply_sphere_raw",
        |b, encoding| {
            const FILES: [&[u8]; 3] = [
                include_bytes!("../tests/files/ply/sphere_ble.ply"),
                include_bytes!("../tests/files/ply/sphere_bbe.ply"),
                include_bytes!("../tests/files/ply/sphere_ascii.ply"),
            ];

            // We do this string -> index stuff here so that the resulting
            // benchmark names are more useful (`/"acsii"` than `/2`).
            let data = match *encoding {
                "ble" => FILES[0],
                "bbe" => FILES[1],
                "ascii" => FILES[2],
                _ => unreachable!(),
            };
            let reader = Reader::new(Cursor::new(data)).unwrap();
            let mut sink = NullRawSink;

            b.iter_batched(
                || reader.clone(),
                |r| r.read_raw(&mut sink),
                BatchSize::SmallInput,
            )
        },
        vec!["ble", "bbe", "ascii"],
    );

    c.bench_function_over_inputs(
        "ply_sphere_vnormals_raw",
        |b, encoding| {
            const FILES: [&[u8]; 3] = [
                include_bytes!("../tests/files/ply/sphere_vnormals_ble.ply"),
                include_bytes!("../tests/files/ply/sphere_vnormals_bbe.ply"),
                include_bytes!("../tests/files/ply/sphere_vnormals_ascii.ply"),
            ];

            // We do this string -> index stuff here so that the resulting
            // benchmark names are more useful (`/"acsii"` than `/2`).
            let data = match *encoding {
                "ble" => FILES[0],
                "bbe" => FILES[1],
                "ascii" => FILES[2],
                _ => unreachable!(),
            };
            let reader = Reader::new(Cursor::new(data)).unwrap();
            let mut sink = NullRawSink;

            b.iter_batched(
                || reader.clone(),
                |r| r.read_raw(&mut sink),
                BatchSize::SmallInput,
            )
        },
        vec!["ble", "bbe", "ascii"],
    );
}


criterion_group!(benches, three_tris_all_props_raw, sphere_raw);
criterion_main!(benches);
