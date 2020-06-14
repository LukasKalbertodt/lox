use std::fmt::Debug;
use crate::prelude::*;

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
            crate::test_utils::io::file_failure(actual, expected, $filename);
        }
    }
}

macro_rules! include_test_file {
    ($filename:expr) => {{
        let bytes = include_bytes!(concat!("test_files/", $filename)) as &[u8];
        std::io::Cursor::new(bytes)
    }}
}


macro_rules! assert_any_prop {
    ($map:expr, $variant:path, [
        $($handle:expr => $value:expr),* $(,)?
    ]) => {
        match &$map {
            None => {
                panic!("assert_any_prop failed: given map `{}` is `None`", stringify!($map));
            }
            Some($variant(map)) => {
                crate::test_utils::io::assert_any_prop(map, &[$(
                    ($handle, $value),
                )*]);
            }
            Some(map) => {
                panic!(
                    "assert_any_prop failed: map expected to be {}, but is {:?}",
                    stringify!($variant),
                    map.ty(),
                );
            }
        }
    };
}

pub(crate) fn assert_any_prop<M, H, V>(map: &M, checks: &[(H, V)])
where
    H: Handle,
    V: Debug + PartialEq,
    M: PropStore<H, Target = V>
{
    if map.num_props() as usize != checks.len() {
        panic!(
            "assert_any_prop failed: map is supposed to have {} values, but has {}",
            checks.len(),
            map.num_props(),
        );
    }

    for (handle, expected) in checks {
        let actual = map.get_ref(*handle);
        match actual {
            None => {
                panic!(
                    "assert_any_prop failed: prop for handle {:?} should be {:?}, but there \
                        is no prop associated with that handle",
                    handle,
                    expected,
                );
            }
            Some(actual) if actual != expected => {
                panic!(
                    "assert_any_prop failed: prop for handle {:?} should be {:?}, but is {:?}",
                    handle,
                    expected,
                    actual,
                );
            }
            _ => {}
        }
    }
}
