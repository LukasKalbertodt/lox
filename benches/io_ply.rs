//! Measures PLY IO read (parse) and write speeds.

use std::io::Cursor;
use criterion::{
    criterion_group, criterion_main, black_box, BatchSize, Criterion,
};

use lox::{
    prelude::*,
    io::ply::{
        Config, Encoding, Reader,
        raw::{PropertyType, PropertyDef, ElementDef, ListLenType, ScalarType},
    },
};

pub mod util;

use util::{
    io::{
        NullSinkPos, NullSinkPosVNormal,
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
fn read_three_tris_all_props_raw(c: &mut Criterion) {
    const FILES: AllEncodings = AllEncodings {
        ble: include_bytes!("../src/io/ply/test_files/three_tris_all_props_ble.ply"),
        bbe: include_bytes!("../src/io/ply/test_files/three_tris_all_props_bbe.ply"),
        ascii: include_bytes!("../src/io/ply/test_files/three_tris_all_props_ascii.ply"),
    };

    c.bench_function_over_inputs(
        "read_ply_three_tris_all_props_raw",
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
fn read_sphere_raw(c: &mut Criterion) {
    c.bench_function_over_inputs(
        "ply_read_sphere_raw",
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
        "ply_read_sphere_vnormals_raw",
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
fn read_sphere_hl(c: &mut Criterion) {
    c.bench_function_over_inputs(
        "ply_read_sphere_hl",
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
        "ply_read_sphere_ignore_vnormals_hl",
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
        "ply_read_sphere_vnormals_hl",
        |b, encoding| {
            const FILES: AllEncodings = AllEncodings {
                ble: include_bytes!("../tests/files/ply/sphere_vnormals_ble.ply"),
                bbe: include_bytes!("../tests/files/ply/sphere_vnormals_bbe.ply"),
                ascii: include_bytes!("../tests/files/ply/sphere_vnormals_ascii.ply"),
            };

            let reader = Reader::new(Cursor::new(FILES.get_for(encoding))).unwrap();

            b.iter_batched(
                || (reader.clone(), NullSinkPosVNormal::new()),
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

/// Measures body reading of `three_tris_all_props` files via `RawSink`.
fn write_sphere_raw(c: &mut Criterion) {
    // Writing only vertex positions
    c.bench_function_over_inputs(
        "ply_write_sphere_raw",
        |b, encoding| {
            let config = match *encoding {
                "ble" => Config::new(Encoding::BinaryLittleEndian),
                "bbe" => Config::new(Encoding::BinaryBigEndian),
                "ascii" => Config::ascii(),
                _ => panic!("bug: wrong encoding in benchmark"),
            };

            let src = util::io::ply::RawSphereSource::new();
            let header = [
                ElementDef {
                    name: "vertex".into(),
                    count: src.vertex_count(),
                    property_defs: ["x", "y", "z"].iter()
                        .map(|&name| {
                            PropertyDef {
                                name: name.into(),
                                ty: PropertyType::Scalar(ScalarType::Float)
                            }
                        })
                        .collect::<Vec<_>>()
                        .into(),
                },
                ElementDef {
                    name: "face".into(),
                    count: src.face_count(),
                    property_defs: vec![
                        PropertyDef {
                            name: "vertex_indices".into(),
                            ty: PropertyType::List  {
                                len_type: ListLenType::UChar,
                                scalar_type: ScalarType::UInt,
                            },
                        }
                    ].into(),
                },
            ];

            // We reserve memory beforehand to ensure the vector doesn't have
            // to reallocate.
            let mut out = Vec::with_capacity(50_000);

            b.iter(|| {
                let res = config.clone()
                    .into_writer(&mut out)
                    .write_raw(&header, &src);
                let _ = black_box(res);
                black_box(&out);
                out.clear();
            })
        },
        vec!["ble", "bbe", "ascii"],
    );

    // Writing vertex positions and normals
    c.bench_function_over_inputs(
        "ply_write_sphere_vnormals_raw",
        |b, encoding| {
            let config = match *encoding {
                "ble" => Config::new(Encoding::BinaryLittleEndian),
                "bbe" => Config::new(Encoding::BinaryBigEndian),
                "ascii" => Config::ascii(),
                _ => panic!("bug: wrong encoding in benchmark"),
            };

            let src = util::io::ply::RawSphereVNormalsSource::new();
            let header = [
                ElementDef {
                    name: "vertex".into(),
                    count: src.vertex_count(),
                    property_defs: ["x", "y", "z", "nx", "ny", "nz"].iter()
                        .map(|&name| {
                            PropertyDef {
                                name: name.into(),
                                ty: PropertyType::Scalar(ScalarType::Float)
                            }
                        })
                        .collect::<Vec<_>>()
                        .into(),
                },
                ElementDef {
                    name: "face".into(),
                    count: src.face_count(),
                    property_defs: vec![
                        PropertyDef {
                            name: "vertex_indices".into(),
                            ty: PropertyType::List  {
                                len_type: ListLenType::UChar,
                                scalar_type: ScalarType::UInt,
                            },
                        }
                    ].into(),
                },
            ];

            // We reserve memory beforehand to ensure the vector doesn't have
            // to reallocate.
            let mut out = Vec::with_capacity(50_000);

            b.iter(|| {
                let res = config.clone()
                    .into_writer(&mut out)
                    .write_raw(&header, &src);
                let _ = black_box(res);
                black_box(&out);
                out.clear();
            })
        },
        vec!["ble", "bbe", "ascii"],
    );
}

/// Measures body reading of `three_tris_all_props` files via `RawSink`.
fn write_sphere_hl(c: &mut Criterion) {
    // Writing only vertex positions
    c.bench_function_over_inputs(
        "ply_write_sphere_hl",
        |b, encoding| {
            let sphere = util::io::sphere();
            let config = match *encoding {
                "ble" => Config::new(Encoding::BinaryLittleEndian),
                "bbe" => Config::new(Encoding::BinaryBigEndian),
                "ascii" => Config::ascii(),
                _ => panic!("bug: wrong encoding in benchmark"),
            };

            // We reserve memory beforehand to ensure the vector doesn't have
            // to reallocate.
            let mut out = Vec::with_capacity(50_000);

            b.iter(|| {
                let res = config.clone()
                    .into_writer(&mut out)
                    .transfer_from(&sphere);
                let _ = black_box(res);
                black_box(&out);
                out.clear();
            })
        },
        vec!["ble", "bbe", "ascii"],
    );

    // Writing vertex positions and normals
    c.bench_function_over_inputs(
        "ply_write_sphere_vnormals_hl",
        |b, encoding| {
            let sphere = util::io::sphere_vnormals();
            let config = match *encoding {
                "ble" => Config::new(Encoding::BinaryLittleEndian),
                "bbe" => Config::new(Encoding::BinaryBigEndian),
                "ascii" => Config::ascii(),
                _ => panic!("bug: wrong encoding in benchmark"),
            };

            // We reserve memory beforehand to ensure the vector doesn't have
            // to reallocate.
            let mut out = Vec::with_capacity(50_000);

            b.iter(|| {
                let res = config.clone()
                    .into_writer(&mut out)
                    .transfer_from(&sphere);
                let _ = black_box(res);
                black_box(&out);
                out.clear();
            })
        },
        vec!["ble", "bbe", "ascii"],
    );
}


criterion_group!(benches,
    read_three_tris_all_props_raw,
    read_sphere_raw,
    read_sphere_hl,
    write_sphere_raw,
    write_sphere_hl,
);
criterion_main!(benches);
