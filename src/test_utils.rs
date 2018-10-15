
pub(crate) fn file_failure(actual: &[u8], expected: &[u8], filename: &str) {
    use std::fmt::Write;

    let mut msg = String::new();

    writeln!(msg, "===== Expected data ('{}')", filename);
    write_data(&mut msg, expected);
    writeln!(msg);

    writeln!(msg, "===== Actual data");
    write_data(&mut msg, actual);

    panic!("assertion failed: \n{}", msg);

    fn write_data(msg: &mut String, data: &[u8]) {

        let s = std::str::from_utf8(data);
        if data.iter().any(|b| *b == 0) || s.is_err() {
            for chunk in data.chunks(32) {
                for b in chunk {
                    write!(msg, "{:02x} ", b);
                }
                writeln!(msg);
            }
        } else {
            writeln!(msg, "{}", s.unwrap());
        }
    }
}

macro_rules! assert_eq_file {
    ($actual:expr, $filename:expr) => {
        let actual = $actual as &[u8];
        let expected = include_bytes!(concat!("test_files/", $filename)) as &[u8];
        if actual != expected {
            crate::test_utils::file_failure(actual, expected, $filename);
        }
    }
}
