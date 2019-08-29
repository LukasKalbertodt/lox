use std::{
    collections::HashSet,
    fmt,
    hash::Hash,
};

use crate::{
    prelude::*,
    handle::hsize,
};


pub(crate) fn check<S, T>(left: &mut S, right: &mut [(FaceHandle, T)])
where
    S: PropMap<FaceHandle, Target = T> + PropStoreMut<FaceHandle, Output = T>,
    T: Eq + fmt::Debug + Hash,
{
    assert_eq!(left.num_props(), right.len() as hsize);
    assert_eq!(left.is_empty(), right.is_empty());

    macro_rules! assert_iter_eq {
        ($left:expr, $right:expr) => {{
            // The order of the iterator is unspecified, so we collect both
            // into a hash set.
            let left_set = $left.collect::<HashSet<_>>();
            let right_set = $right.collect::<HashSet<_>>();

            if left_set != right_set {
                panic!(
                    "assert_iter_eq failed!\n  left: `{}` => {:?}\n right: `{}` => {:?}\n",
                    stringify!($left),
                    left_set,
                    stringify!($right),
                    right_set,
                );
            }
        }};
    }

    assert_iter_eq!(left.iter(), right.iter().map(|(h, v)| (*h, v)));
    assert_iter_eq!(left.values(), right.iter().map(|(_, v)| v));
    assert_iter_eq!(left.handles(), right.iter().map(|(h, _)| *h));
    assert_iter_eq!(left.iter_mut(), right.iter_mut().map(|(h, v)| (*h, v)));
    assert_iter_eq!(left.values_mut(), right.iter_mut().map(|(_, v)| v));

    if let Some(max) = right.iter().map(|(h, _)| h.idx()).max() {
        for idx in 0..max + 2 {
            let h = FaceHandle::new(idx);
            if let Some(pos) = right.iter().position(|(h, _)| h.idx() == idx) {
                assert!(left.contains_handle(h));
                assert_eq!(left.get_ref(h), Some(&right[pos].1));
                assert_eq!(&left[h], &right[pos].1);
                assert_eq!(left.get_mut(h), Some(&mut right[pos].1));
                assert_eq!(&mut left[h], &mut right[pos].1);
            } else {
                assert_eq!(left.get(h), None);
                assert_eq!(left.get_ref(h), None);
                assert_eq!(left.get_mut(h), None);
            }
        }
    }
}

macro_rules! gen_tests_for_store_impl {
    ($name:ident) => {
        use crate::{
            FaceHandle,
            map::{PropMap, PropStoreMut},
        };


        // ----- Helper stuff ------------------------------------------------
        fn h(x: usize) -> FaceHandle {
            FaceHandle::from_usize(x)
        }

        macro_rules! check {
            ($store:ident, $expected:expr) => {
                crate::map::tests::check(&mut $store, &mut $expected);
            };
        }


        // ----- Tests -------------------------------------------------------

        #[test]
        fn empty() {
            let mut m = $name::<FaceHandle, ()>::empty();
            check!(m, []);
        }

        #[test]
        fn with_capacity_empty() {
            let mut m = $name::<FaceHandle, ()>::with_capacity(23);
            check!(m, []);
        }

        #[test]
        fn insert_one() {
            let mut m = $name::empty();
            m.insert(h(1), "a");
            check!(m, [(h(1), "a")]);
        }

        #[test]
        fn reinsert() {
            let mut m = $name::empty();
            let ret0 = m.insert(h(1), "a");
            let ret1 = m.insert(h(1), "b");

            assert_eq!(ret0, None);
            assert_eq!(ret1, Some("a"));
            check!(m, [(h(1), "b")]);
        }

        #[test]
        fn clear() {
            let mut m = $name::empty();

            m.insert(h(0), "a");
            m.insert(h(0), "x");
            m.insert(h(1), "b");
            m.insert(h(5), "c");
            check!(m, [(h(0), "x"), (h(1), "b"), (h(5), "c")]);

            m.clear();
            check!(m, []);
            assert!(m.get(h(5)).is_none());

            m.insert(h(4), "y");
            check!(m, [(h(4), "y")]);
        }

        #[test]
        fn remove_nothing() {
            let mut m = $name::<_, ()>::empty();
            assert!(m.remove(h(1)).is_none());
            check!(m, []);
        }

        #[test]
        fn remove_simple() {
            let mut m = $name::empty();

            m.insert(h(0), "a");
            m.insert(h(1), "b");
            m.insert(h(1), "c");
            m.insert(h(2), "d");
            check!(m, [(h(0), "a"), (h(1), "c"), (h(2), "d")]);

            let ret = m.remove(h(0));
            assert_eq!(ret, Some("a"));
            check!(m, [(h(1), "c"), (h(2), "d")]);

            let ret = m.remove(h(1));
            assert_eq!(ret, Some("c"));
            check!(m, [(h(2), "d")]);
        }

        #[test]
        fn remove_more_complex() {
            let mut m = $name::with_capacity(23);

            m.insert(h(0), "a");
            m.insert(h(1), "b");
            m.insert(h(1), "c");
            m.insert(h(2), "d");
            m.insert(h(3), "e");
            m.insert(h(4), "f");
            check!(m, [(h(0), "a"), (h(1), "c"), (h(2), "d"), (h(3), "e"), (h(4), "f")]);

            assert_eq!(m.remove(h(3)), Some("e"));
            check!(m, [(h(0), "a"), (h(1), "c"), (h(2), "d"), (h(4), "f")]);

            assert_eq!(m.remove(h(1)), Some("c"));
            check!(m, [(h(0), "a"), (h(2), "d"), (h(4), "f")]);

            assert_eq!(m.remove(h(1)), None);
            check!(m, [(h(0), "a"), (h(2), "d"), (h(4), "f")]);

            assert_eq!(m.insert(h(3), "x"), None);
            check!(m, [(h(0), "a"), (h(2), "d"), (h(3), "x"), (h(4), "f")]);

            assert_eq!(m.remove(h(3)), Some("x"));
            check!(m, [(h(0), "a"), (h(2), "d"), (h(4), "f")]);
        }

        #[test]
        fn values_mut() {
            let mut m = $name::with_capacity(23);

            m.insert(h(0), "a");
            m.insert(h(1), "b");
            m.insert(h(2), "c");
            m.insert(h(3), "d");
            m.insert(h(4), "e");

            for s in m.values_mut() {
                match *s {
                    "b" => *s = "nonono",
                    "e" => *s = "yes",
                    _ => (),
                }
            }

            check!(m, [(h(0), "a"), (h(1), "nonono"), (h(2), "c"), (h(3), "d"), (h(4), "yes")]);
        }
    }
}
