//! Measures STL IO read (parse) and write speeds.

use std::io::Cursor;
use criterion::{
    criterion_group, criterion_main, black_box, BatchSize, Criterion,
};

use lox::{
    prelude::*,
    io::stl::Reader,
};

pub mod util;

use util::{
    io::{NullSinkPos, NullSinkPosFNormal},
};

// ===============================================================================================
// ===== Helper utilities
// ===============================================================================================

/// Helper struct to improve benchmark names.
///
/// We use `benchmark_function_over_inputs` a lot. It generates benchmark names
/// with the inputs `Debug` representation. The best idea is to use the strings
/// "binary" and "ascii" as input. But the actual input is a byte slices.
/// That's what this type is for: the mapping from string to byte slice.
struct AllEncodings {
    binary: &'static [u8],
    ascii: &'static [u8],
}

impl AllEncodings {
    fn get_for(&self, encoding: &str) -> &'static [u8] {
        match encoding {
            "binary" => self.binary,
            "ascii" => self.ascii,
            _ => panic!("bug: wrong format specified in benchmark!"),
        }
    }
}


// ===============================================================================================
// ===== Benchmarks
// ===============================================================================================

fn sphere_raw(c: &mut Criterion) {
    c.bench_function_over_inputs(
        "stl_sphere_raw",
        |b, encoding| {
            const FILES: AllEncodings = AllEncodings {
                binary: include_bytes!("../tests/files/stl/sphere_binary.stl"),
                ascii: include_bytes!("../tests/files/stl/sphere_ascii.stl"),
            };

            let reader = Reader::new(Cursor::new(FILES.get_for(encoding))).unwrap();

            b.iter_batched(
                || reader.clone(),
                |r| r.read_raw(|tri| { black_box(tri); }),
                BatchSize::SmallInput,
            )
        },
        vec!["binary", "ascii"],
    );
}

fn sphere_hl(c: &mut Criterion) {
    c.bench_function_over_inputs(
        "stl_sphere_ignore_fnormals_hl",
        |b, encoding| {
            const FILES: AllEncodings = AllEncodings {
                binary: include_bytes!("../tests/files/stl/sphere_binary.stl"),
                ascii: include_bytes!("../tests/files/stl/sphere_ascii.stl"),
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
        vec!["binary", "ascii"],
    );

    c.bench_function_over_inputs(
        "stl_sphere_fnormals_hl",
        |b, encoding| {
            const FILES: AllEncodings = AllEncodings {
                binary: include_bytes!("../tests/files/stl/sphere_binary.stl"),
                ascii: include_bytes!("../tests/files/stl/sphere_ascii.stl"),
            };

            let reader = Reader::new(Cursor::new(FILES.get_for(encoding))).unwrap();

            b.iter_batched(
                || (reader.clone(), NullSinkPosFNormal::new()),
                |(r, mut sink)| {
                    let out = r.transfer_to(&mut sink);
                    black_box(sink);
                    out
                },
                BatchSize::SmallInput,
            )
        },
        vec!["binary", "ascii"],
    );
}


criterion_group!(benches, sphere_raw, sphere_hl);
criterion_main!(benches);
