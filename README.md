# Lox: polygon mesh library

[<img alt="CI status of main" src="https://img.shields.io/github/actions/workflow/status/LukasKalbertodt/lox/ci.yml?branch=main&label=CI&logo=github&logoColor=white&style=for-the-badge" height="23">](https://github.com/LukasKalbertodt/lox/actions/workflows/ci.yml)
[<img alt="Crates.io Version" src="https://img.shields.io/crates/v/lox?logo=rust&style=for-the-badge" height="23">](https://crates.io/crates/lox)
[<img alt="docs.rs" src="https://img.shields.io/crates/v/lox?color=blue&label=docs&style=for-the-badge" height="23">](https://docs.rs/lox)

`lox` is a polygon mesh library with different data structures and traits to abstract over those.
It can be used to create, generate, process, and analyze polygon meshes.
This is part of the field "geometry processing", relevant for developing real-time 3D applications, simulations, 3D-printing, and much more.

**Main features**:

- Multiple optimized and well tested mesh data structures, including *half edge mesh* and *directed edge mesh*.
- Ability to abstract over different data structures without overhead.
- *BlAzInGlY fAsT*. Ok no actually, it's [pretty fast](https://docs.rs/lox#speed) and I have benchmarks to prove it.
- *Prop maps* as flexible solution for storing and managing additional mesh properties (e.g. vertex positions, face colors, ...).
- Built-in algorithms (only very few right now).
- **Notably missing**: IO. [Explanation](https://docs.rs/lox#background-and-missing-features).

See [**the documentation**](https://docs.rs/lox) for more details and on how to get started.

<br />

---

## License

Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this project by you, as defined in the Apache-2.0 license,
shall be dual licensed as above, without any additional terms or conditions.
