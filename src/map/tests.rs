

macro_rules! gen_tests_for_store_impl {
    ($name:ident) => {
        use crate::{
            FaceHandle,
            map::{PropMap, PropStore, PropStoreMut},
        };

        fn h(x: usize) -> FaceHandle {
            FaceHandle::from_usize(x)
        }

        #[test]
        fn empty() {
            let m = $name::<FaceHandle, ()>::empty();
            assert_eq!(m.num_props(), 0);
        }

        #[test]
        fn insert_one() {
            let mut m = $name::empty();
            let f0 = h(0);
            m.insert(f0, "a");

            assert_eq!(m.num_props(), 1);
            assert_eq!(m.get(f0).map(|x| *x), Some("a"));
            assert_eq!(m.get_ref(f0).map(|x| *x), Some("a"));
            assert_eq!(m.get_mut(f0).map(|x| *x), Some("a"));

            let f1 = h(1);
            assert!(m.get(f1).is_none());
            assert!(m.get_ref(f1).is_none());
            assert!(m.get_mut(f1).is_none());

            assert_eq!(m.handles().collect::<Vec<_>>(), [f0]);
        }

        #[test]
        fn reinsert() {
            let mut m = $name::empty();
            let f0 = h(0);

            m.insert(f0, "a");
            let ret = m.insert(f0, "b");

            assert_eq!(ret, Some("a"));
            assert_eq!(m.num_props(), 1);
            assert_eq!(m.get(f0).map(|x| *x), Some("b"));
            assert_eq!(m.get_ref(f0).map(|x| *x), Some("b"));
            assert_eq!(m.get_mut(f0).map(|x| *x), Some("b"));
        }

        #[test]
        fn clear() {
            let mut m = $name::empty();

            m.insert(h(0), "a");
            m.insert(h(0), "x");
            m.insert(h(1), "b");
            m.insert(h(5), "c");

            assert_eq!(m.num_props(), 3);
            assert_eq!(m.handles().count(), 3);

            m.clear();

            assert_eq!(m.num_props(), 0);
            assert_eq!(m.handles().count(), 0);
            assert!(m.get(h(0)).is_none());
            assert!(m.get(h(0)).is_none());
            assert!(m.get(h(1)).is_none());
            assert!(m.get(h(5)).is_none());

            m.insert(h(4), "y");

            assert_eq!(m.num_props(), 1);
            assert_eq!(m.handles().count(), 1);
            assert!(m.get(h(0)).is_none());
            assert!(m.get(h(0)).is_none());
            assert!(m.get(h(1)).is_none());
            assert!(m.get(h(5)).is_none());


            assert_eq!(m.get(h(4)).map(|x| *x), Some("y"));
            assert_eq!(m.get_ref(h(4)).map(|x| *x), Some("y"));
            assert_eq!(m.get_mut(h(4)).map(|x| *x), Some("y"));
        }

        #[test]
        fn remove_nothing() {
            let mut m = $name::<_, ()>::empty();
            assert!(m.remove(h(0)).is_none());
        }

        #[test]
        fn remove_simple() {
            let mut m = $name::empty();

            m.insert(h(0), "a");
            m.insert(h(1), "b");
            m.insert(h(1), "c");
            m.insert(h(2), "d");

            assert_eq!(m.num_props(), 3);
            assert_eq!(m.handles().count(), 3);

            let ret = m.remove(h(0));

            assert_eq!(ret, Some("a"));
            assert_eq!(m.num_props(), 2);
            assert_eq!(m.handles().count(), 2);

            let ret = m.remove(h(1));
            assert_eq!(ret, Some("c"));
            assert_eq!(m.num_props(), 1);
            assert_eq!(m.handles().count(), 1);
        }
    }
}
