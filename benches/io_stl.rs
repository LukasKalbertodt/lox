//! Measures STL IO read (parse) and write speeds.

use std::io::Cursor;
use criterion::{
    criterion_group, criterion_main, black_box, BatchSize, Criterion,
};

use lox::{
    prelude::*,
    io::stl::{Config, Reader},
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

fn read_sphere_raw(c: &mut Criterion) {
    let mut group = c.benchmark_group("stl_read_sphere_raw");
    for encoding in ["binary", "ascii"] {
        group.bench_with_input(encoding, encoding, |b, encoding| {
            const FILES: AllEncodings = AllEncodings {
                binary: include_bytes!("../tests/files/stl/sphere_binary.stl"),
                ascii: include_bytes!("../tests/files/stl/sphere_ascii.stl"),
            };

            let reader = Reader::new(Cursor::new(FILES.get_for(encoding))).unwrap();

            b.iter_batched(
                || reader.clone(),
                |r| r.read_raw(|tri| { black_box(tri); Ok(()) }),
                BatchSize::SmallInput,
            );
        });
    }
    group.finish();
}

fn read_sphere_hl(c: &mut Criterion) {
    let mut group = c.benchmark_group("stl_read_sphere_ignore_fnormals_hl");
    for encoding in ["binary", "ascii"] {
        group.bench_with_input(encoding, encoding, |b, encoding| {
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
            );
        });
    }
    group.finish();

    let mut group = c.benchmark_group("stl_sphere_fnormals_hl");
    for encoding in ["binary", "ascii"] {
        group.bench_with_input(encoding, encoding, |b, encoding| {
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
            );
        });
    }
    group.finish();
}

fn write_sphere_raw(c: &mut Criterion) {
    let mut group = c.benchmark_group("stl_write_sphere_raw");
    for encoding in ["binary", "ascii"] {
        group.bench_with_input(encoding, encoding, |b, encoding| {
            let triangles = util::io::stl::raw_sphere();
            let config = match encoding {
                "binary" => Config::binary(),
                "ascii" => Config::ascii(),
                _ => panic!("bug: wrong encoding in benchmark"),
            };

            // We reserve memory beforehand to ensure the vector doesn't have
            // to reallocate. The resulting STL files of the sphere above are
            // 38K and 187K bytes large for binary and ASCII respectively.
            let mut out = Vec::with_capacity(200_000);

            b.iter(|| {
                let res = config.clone()
                    .into_writer(&mut out)
                    .write_raw(triangles.len() as u32, triangles.iter().map(|t| Ok(*t)));
                let _ = black_box(res);
                black_box(&out);
                out.clear();
            });
        });
    }
    group.finish();
}

fn write_sphere_hl(c: &mut Criterion) {
    let mut group = c.benchmark_group("stl_write_sphere_hl");
    for encoding in ["binary", "ascii"] {
        group.bench_with_input(encoding, encoding, |b, encoding| {
            let sphere = util::io::sphere();
            let config = match encoding {
                "binary" => Config::binary(),
                "ascii" => Config::ascii(),
                _ => panic!("bug: wrong encoding in benchmark"),
            };

            // We reserve memory beforehand to ensure the vector doesn't have
            // to reallocate. The resulting STL files of the sphere above are
            // 38K and 187K bytes large for binary and ASCII respectively.
            let mut out = Vec::with_capacity(200_000);

            b.iter(|| {
                let res = config.clone().into_writer(&mut out).transfer_from(&sphere);
                let _ = black_box(res);
                black_box(&out);
                out.clear();
            });
        });
    }
    group.finish();
}


criterion_group!(benches, read_sphere_raw, read_sphere_hl, write_sphere_raw, write_sphere_hl);
criterion_main!(benches);
