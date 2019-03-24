//! Measures PLY IO read (parse) and write speeds.

use std::io::Cursor;
use criterion::{
    criterion_group, criterion_main, black_box, BatchSize, Criterion,
};

use lox::{
    io::{
        Error,
        ply::{
            Reader,
            raw::{ElementDef, RawElement, RawSink},
        },
    },
};


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
