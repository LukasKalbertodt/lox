use std::fmt::Debug;

// #[cfg(feature = "io")]
// #[macro_use]
// pub(crate) mod io;


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
