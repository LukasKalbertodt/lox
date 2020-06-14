use std::fmt::Debug;

use crate::prelude::*;

#[cfg(feature = "io")]
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

#[cfg(feature = "io")]
macro_rules! assert_eq_file {
    ($actual:expr, $filename:expr) => {
        let actual = $actual as &[u8];
        let expected = include_bytes!(concat!("test_files/", $filename)) as &[u8];
        if actual != expected {
            crate::test_utils::file_failure(actual, expected, $filename);
        }
    }
}

#[cfg(feature = "io")]
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
pub(crate) fn assert_eq_fn<T, U>(left: &T, right: &U, file: &str, line: u32, col: u32)
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

/// Checks if the given code will trigger a panic. If it does, nothing happens
/// (except side effects of the given code). If no panic is caused by the given
/// code, this macro panics with a message.
macro_rules! assert_panic {
    ($($body:tt)*) => {{
        let res = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            $($body)*
        }));
        if let Ok(x) = res {
            panic!(
                "expected panic for '{}', but got '{:?}' ",
                stringify!($($body)*),
                x,
            );
        }
    }}
}

/// Macro version of `cmp_rotated` with nicer error message.
#[allow(unused_macros)]
macro_rules! assert_rotated_eq {
    ($lhs:expr, $rhs:expr) => {{
        let lhs = $lhs;
        let rhs = $rhs;
        if crate::test_utils::cmp_rotated(&lhs, &rhs).is_err() {
            panic!(
                "assert_rotated_eq failed:n\
                    |  left: {:?} ({})\n\
                    | right: {:?} ({})\n",
                lhs,
                stringify!($lhs),
                rhs,
                stringify!($rhs),
            );
        }
    }};
}

/// Compares `actual` and `expected`. This function checks if both slices are
/// equal when treating them like a "ring". This means that if we can rotate
/// one slice so that it equals the other slice, we consider them equal.
/// `[a, b, c, d]` and `[b, c, d, a]` and `[d, a, b, c]` are all equal.
///
/// If the slices are equal, `Ok(())` is returned. Otherwise, `Err(rotated)` is
/// returned, where `rotated` is `expected` but potentially rotated by some
/// amount. This can be used to print in the error message as the returned
/// vector looks more similar to the `actual` value.
pub(crate) fn cmp_rotated<T: Debug + PartialEq + Clone>(
    actual: &[T],
    expected: &[T],
) -> Result<(), Vec<T>> {
    let mut rotated = expected.to_vec();

    if actual.len() != expected.len() {
        return Err(rotated);
    }

    if actual.len() > 0 {
        // Find the rotate-offset
        let pos = match actual.iter().position(|e| e == &expected[0]) {
            Some(pos) => pos,
            None => return Err(rotated),
        };

        // Align my rotating back
        rotated.rotate_right(pos);

        if actual != &rotated[..] {
            return Err(rotated);
        }
    }

    Ok(())
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
                crate::test_utils::assert_any_prop(map, &[$(
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
