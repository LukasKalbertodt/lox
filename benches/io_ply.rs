//! Measures PLY IO read (parse) and write speeds.

use std::io::Cursor;
use criterion::{
    criterion_group, criterion_main, black_box, BatchSize, Criterion,
};
use cgmath::{Point3, Vector3};

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

/// A sink that puts all vertex positions and vertex normals  into the
/// `black_box`, ignores all other properties.
struct NullSinkPosNormal(NullSinkPos);

impl NullSinkPosNormal {
    fn new() -> Self {
        Self(NullSinkPos::new())
    }
}

impl MemSink for NullSinkPosNormal {
    fn add_vertex(&mut self) -> VertexHandle {
        self.0.add_vertex()
    }
    fn add_face(&mut self, vertices: [VertexHandle; 3]) -> FaceHandle {
        self.0.add_face(vertices)
    }

    fn size_hint(&mut self, hint: MeshSizeHint) {
        self.0.size_hint(hint)
    }

    fn prepare_vertex_positions<N: Primitive>(&mut self, count: hsize) -> Result<(), Error> {
        self.0.prepare_vertex_positions::<N>(count)
    }
    fn set_vertex_position<N: Primitive>(
        &mut self,
        v: VertexHandle,
        position: Point3<N>,
    ) {
        self.0.set_vertex_position::<N>(v, position)
    }

    fn prepare_vertex_normals<N: Primitive>(&mut self, count: hsize) -> Result<(), Error> {
        black_box(count);
        Ok(())
    }
    fn set_vertex_normal<N: Primitive>(
        &mut self,
        v: VertexHandle,
        normal: Vector3<N>,
    ) {
        black_box(v);
        black_box(normal);
    }
}

/// Helper struct to improve benchmark names.
///
/// We use `benchmark_function_over_inputs` a lot. It generates benchmark names
/// with the inputs `Debug` representation. The best idea is to use the strings
/// "ble", "bbe" and "ascii" as input. But the actual input is a byte slices.
/// That's what this type is for: the mapping from string to byte slice.
struct AllEncodings {
    bbe: &'static [u8],
    ble: &'static [u8],
    ascii: &'static [u8],
}

impl AllEncodings {
    fn get_for(&self, encoding: &str) -> &'static [u8] {
        match encoding {
            "ble" => self.ble,
            "bbe" => self.bbe,
            "ascii" => self.ascii,
            _ => panic!(),
        }
    }
}


// ===============================================================================================
// ===== Benchmarks
// ===============================================================================================

/// Measures body reading of `three_tris_all_props` files via `RawSink`.
fn three_tris_all_props_raw(c: &mut Criterion) {
    const FILES: AllEncodings = AllEncodings {
        ble: include_bytes!("../src/io/ply/test_files/three_tris_all_props_ble.ply"),
        bbe: include_bytes!("../src/io/ply/test_files/three_tris_all_props_bbe.ply"),
        ascii: include_bytes!("../src/io/ply/test_files/three_tris_all_props_ascii.ply"),
    };

    c.bench_function_over_inputs(
        "ply_three_tris_all_props_ble_raw",
        |b, encoding| {
            let reader = Reader::new(Cursor::new(FILES.get_for(encoding))).unwrap();
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
            const FILES: AllEncodings = AllEncodings {
                ble: include_bytes!("../tests/files/ply/sphere_ble.ply"),
                bbe: include_bytes!("../tests/files/ply/sphere_bbe.ply"),
                ascii: include_bytes!("../tests/files/ply/sphere_ascii.ply"),
            };

            let reader = Reader::new(Cursor::new(FILES.get_for(encoding))).unwrap();
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
            const FILES: AllEncodings = AllEncodings {
                ble: include_bytes!("../tests/files/ply/sphere_vnormals_ble.ply"),
                bbe: include_bytes!("../tests/files/ply/sphere_vnormals_bbe.ply"),
                ascii: include_bytes!("../tests/files/ply/sphere_vnormals_ascii.ply"),
            };

            let reader = Reader::new(Cursor::new(FILES.get_for(encoding))).unwrap();
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
fn sphere_hl(c: &mut Criterion) {
    c.bench_function_over_inputs(
        "ply_sphere_hl",
        |b, encoding| {
            const FILES: AllEncodings = AllEncodings {
                ble: include_bytes!("../tests/files/ply/sphere_ble.ply"),
                bbe: include_bytes!("../tests/files/ply/sphere_bbe.ply"),
                ascii: include_bytes!("../tests/files/ply/sphere_ascii.ply"),
            };

            let reader = Reader::new(Cursor::new(FILES.get_for(encoding))).unwrap();

            b.iter_batched(
                || (reader.clone(), NullSinkPos::new()),
                |(r, mut sink)| {
                    let out = r.transfer_to(&mut sink);
                    black_box(sink);
                    out
                },
                BatchSize::SmallInput,
            )
        },
        vec!["ble", "bbe", "ascii"],
    );

    c.bench_function_over_inputs(
        "ply_sphere_ignore_vnormals_hl",
        |b, encoding| {
            const FILES: AllEncodings = AllEncodings {
                ble: include_bytes!("../tests/files/ply/sphere_vnormals_ble.ply"),
                bbe: include_bytes!("../tests/files/ply/sphere_vnormals_bbe.ply"),
                ascii: include_bytes!("../tests/files/ply/sphere_vnormals_ascii.ply"),
            };

            let reader = Reader::new(Cursor::new(FILES.get_for(encoding))).unwrap();

            b.iter_batched(
                || (reader.clone(), NullSinkPos::new()),
                |(r, mut sink)| {
                    let out = r.transfer_to(&mut sink);
                    black_box(sink);
                    out
                },
                BatchSize::SmallInput,
            )
        },
        vec!["ble", "bbe", "ascii"],
    );

    c.bench_function_over_inputs(
        "ply_sphere_vnormals_hl",
        |b, encoding| {
            const FILES: AllEncodings = AllEncodings {
                ble: include_bytes!("../tests/files/ply/sphere_vnormals_ble.ply"),
                bbe: include_bytes!("../tests/files/ply/sphere_vnormals_bbe.ply"),
                ascii: include_bytes!("../tests/files/ply/sphere_vnormals_ascii.ply"),
            };

            let reader = Reader::new(Cursor::new(FILES.get_for(encoding))).unwrap();

            b.iter_batched(
                || (reader.clone(), NullSinkPosNormal::new()),
                |(r, mut sink)| {
                    let out = r.transfer_to(&mut sink);
                    black_box(sink);
                    out
                },
                BatchSize::SmallInput,
            )
        },
        vec!["ble", "bbe", "ascii"],
    );
}


criterion_group!(benches, three_tris_all_props_raw, sphere_raw, sphere_hl);
criterion_main!(benches);
