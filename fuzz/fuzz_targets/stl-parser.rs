#![no_main]

use libfuzzer_sys::fuzz_target;
use lox::io::stl::{Reader, CounterSink};

fuzz_target!(|data: &[u8]| {
    // We just pass the data to the parsing function and ignore all parsed
    // data. We are just interested in panics or other even worse crashes. So
    // we also ignore the returned `Result` as it's fine if the parser says
    // "this is not a valid STL file".
    let _ = Reader::new(data).read_raw_into(&mut CounterSink::new());
});
