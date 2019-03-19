Fuzzing tests
=============

This folder contains a few fuzzing tests for file parsers. In order to interact with this, you need [`cargo-fuzz`](https://github.com/rust-fuzz/cargo-fuzz). You can install it via `cargo install cargo-fuzz`.


### Run fuzzer

To start fuzzing, run this from the root folder (not the `fuzz` folder):

```
cargo fuzz list         # print all fuzzing targets
cargo fuzz run {name}   # for example "ply-parser"
```

This runs the fuzz target in debug mode, but you might want to run it in release mode with `--release`. However, due to a bug, you probably need to set `RUSTFLAGS='-C codegen-units=1'` to successfully compile in release mode.

Other useful options:

- `-j8`: multiple concurrent jobs
- `-a`: debug assertions
- `cargo fuzz run {name} -- {options}`: these options are passed to libfuzz. Examples:
    - `-max_total_time=3s`

Useful command to copy:

```
RUSTFLAGS='-C codegen-units=1' cargo fuzz run --release -a -j8 ply-parser
```


### Add fuzz target

To add a new fuzz target, run `cargo fuzz add {name}`. Then, add a folder with the same name in the `corpus` directory. Place a `.gitignore` file with the following contents in that folder:

```
*
!*.ply       # change to your format's file extension!
!.gitignore
´´´

Finally, place a few files of that format into the directory.
