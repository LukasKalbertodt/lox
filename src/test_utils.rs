use std::fmt::Debug;


pub(crate) fn file_failure(actual: &[u8], expected: &[u8], filename: &str) {
    use std::fmt::Write;

    let mut msg = String::new();

    writeln!(msg, "===== Expected data ('{}')", filename).unwrap();
    write_data(&mut msg, expected);
    writeln!(msg).unwrap();

    writeln!(msg, "===== Actual data (written to 'dump.bin')").unwrap();
    std::fs::write("dump.bin", actual).expect("failed to dump actual data");
    write_data(&mut msg, actual);

    panic!("assertion failed: \n{}", msg);

    fn write_data(msg: &mut String, data: &[u8]) {
        let s = std::str::from_utf8(data);
        if data.iter().any(|b| *b == 0) || s.is_err() {
            for chunk in data.chunks(32) {
                for b in chunk {
                    write!(msg, "{:02x} ", b).unwrap();
                }
                writeln!(msg).unwrap();
            }
        } else {
            writeln!(msg, "{}", s.unwrap()).unwrap();
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

macro_rules! include_test_file {
    ($filename:expr) => {{
        let bytes = include_bytes!(concat!("test_files/", $filename)) as &[u8];
        std::io::Cursor::new(bytes)
    }}
}

macro_rules! assert_eq {
    ($left:expr, $right:expr) => {
        crate::test_utils::assert_eq_fn(&*&$left, &*&$right, file!(), line!(), column!())
    };
    ($left:expr, $right:expr, ) => {
        assert_eq!($left, $right)
    };
    ($left:expr, $right:expr, $( $arg:tt ) + ) => {
        std::assert_eq($left, $right, $($arg)+)
    };
}

#[inline(never)]
pub fn assert_eq_fn<T, U>(left: &T, right: &U, file: &str, line: u32, col: u32)
where
    T: Debug + PartialEq<U>,
    U: Debug,
{
    if left != right {
        panic!(
            "assert_eq failed:\n  left: `{:?}`, \n right: `{:?}`\nAt: {}:{}:{}",
            left,
            right,
            file,
            line,
            col,
        )
    }
}
