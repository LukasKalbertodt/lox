/// Helper macro to generate iterator types for the different maps.
macro_rules! gen_mapped_iter {
    (
        $name:ident<$lt:tt, $h:ident, $t:ident>($($fields:path),*);
        mut_token: [$($mut:tt)?],
        extra_derives: [$($derives:ident)*],
        mapping: |$args:tt| $body:expr,
        double_ended: $double_ended:tt,
    ) => {

        #[derive(Debug $(, $derives)* )]
        pub struct $name<$lt, $h: Handle, $t>($($fields),*);

        impl<$lt, $h: Handle, $t> Iterator for $name<$lt, $h, $t> {
            type Item = ($h, & $lt $($mut)? $t);
            fn next(&mut self) -> Option<Self::Item> {
                self.0.next().map(|$args| $body)
            }

            fn size_hint(&self) -> (usize, Option<usize>) {
                self.0.size_hint()
            }

            fn count(self) -> usize {
                self.0.count()
            }

            $crate::map::util::gen_mapped_iter!(@last_impl $double_ended);
        }

        $crate::map::util::gen_mapped_iter!(@double_ended_impl
            $double_ended $name<$lt, $h, $t> |$args| $body
        );

        impl<$lt, $h: Handle, $t> ExactSizeIterator for $name<$lt, $h, $t> {
            fn len(&self) -> usize {
                self.0.len()
            }
        }

        impl<$lt, $h: Handle, $t> std::iter::FusedIterator for $name<$lt, $h, $t> {}
    };
    (@last_impl false) => {};
    (@last_impl true) => {
        fn last(mut self) -> Option<Self::Item> {
            self.next_back()
        }
    };
    (@double_ended_impl false $($t:tt)*) => {};
    (@double_ended_impl true $name:ident<$lt:tt, $h:ident, $t:ident> |$args:tt| $body:expr) => {
        impl<$lt, $h: Handle, $t> DoubleEndedIterator for $name<$lt, $h, $t> {
            fn next_back(&mut self) -> Option<Self::Item> {
                self.0.next_back().map(|$args| $body)
            }
        }
    };
}

pub(super) use gen_mapped_iter;
