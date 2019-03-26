//! Measures PLY IO read (parse) and write speeds.

use std::io::Cursor;
use criterion::{
    criterion_group, criterion_main, black_box, BatchSize, Criterion,
};

use lox::{
    prelude::*,
    io::ply::Reader,
};

mod util;

use util::{
    io::{
        NullSinkPos, NullSinkPosNormal,
        ply::NullRawSink,
    },
};

// ===============================================================================================
// ===== Helper utilities
// ===============================================================================================



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