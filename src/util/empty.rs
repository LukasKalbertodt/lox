/// Types that have a notion of “being empty” and can create such an empty
/// instance.
///
/// This is very similar to `Default` from the standard library, but makes it
/// explicit that the returned instance is *empty* and not just any default
/// instance.
///
/// This trait is implemented for several standard types.
///
///
/// # Deriving
///
/// This trait can automatically be derived. See [the derive
/// macro](../derive.Empty.html) for more information on that.
pub trait Empty {
    /// Returns an empty value of this type.
    fn empty() -> Self;
}

/// Derive macro for the [`Empty` trait][util::Empty].
///
/// ```
/// use lox::util::Empty;
///
/// #[derive(Empty)]
/// struct MyStruct {
///     a: Vec<u32>,        // => `vec![]`
///     b: Option<String>,  // => `None`
///     c: (),              // => `()`
/// }
/// ```
///
/// This can only be derived for structs. All struct fields need to implement
/// `Empty` in order for the derive to work. If your struct has generic
/// parameters, they won't be bounded with `Empty` in the generated impl block.
/// This is useful most of the time, because things like `Vec<T>` and
/// `Option<T>` don't require `T: Empty` to implement `Empty`. But this means
/// that you sometimes have to add a global `Empty` bound to your parameter or
/// implement `Empty` manually.
pub use lox_macros::Empty;


// ===========================================================================
// ===== Implementations
// ===========================================================================
macro_rules! impl_empty_via_default {
    ($( { $($impl_header:tt)+ } ,)*) => {
        $(
            $($impl_header)* {
                fn empty() -> Self {
                    Self::default()
                }
            }
        )*
    }
}

impl_empty_via_default!(
    { impl Empty for () },
    { impl<T: ?Sized> Empty for std::marker::PhantomData<T> },
    { impl<T> Empty for Option<T> },
    { impl Empty for String },
    { impl<T> Empty for Vec<T> },
    { impl<T: Ord> Empty for std::collections::BTreeSet<T> },
    { impl<T: Eq + std::hash::Hash> Empty for std::collections::HashSet<T> },
    { impl<T> Empty for std::collections::LinkedList<T> },
    { impl<T> Empty for std::collections::VecDeque<T> },
    { impl<K: Ord, V> Empty for std::collections::BTreeMap<K, V> },
    { impl<K: Eq + std::hash::Hash, V> Empty for std::collections::HashMap<K, V> },
);

impl<T: Empty> Empty for Box<T> {
    fn empty() -> Self {
        Box::new(T::empty())
    }
}

impl<A: Empty> Empty for (A,) {
    fn empty() -> Self { (A::empty(),) }
}
impl<A: Empty, B: Empty> Empty for (A, B) {
    fn empty() -> Self { (A::empty(), B::empty()) }
}
impl<A: Empty, B: Empty, C: Empty> Empty for (A, B, C) {
    fn empty() -> Self { (A::empty(), B::empty(), C::empty()) }
}
impl<A: Empty, B: Empty, C: Empty, D: Empty> Empty for (A, B, C, D) {
    fn empty() -> Self { (A::empty(), B::empty(), C::empty(), D::empty()) }
}

impl<T: Empty> Empty for [T; 0] {
    fn empty() -> Self { [] }
}
impl<T: Empty> Empty for [T; 1] {
    fn empty() -> Self { [T::empty()] }
}
impl<T: Empty> Empty for [T; 2] {
    fn empty() -> Self { [T::empty(), T::empty()] }
}
impl<T: Empty> Empty for [T; 3] {
    fn empty() -> Self { [T::empty(), T::empty(), T::empty()] }
}
impl<T: Empty> Empty for [T; 4] {
    fn empty() -> Self { [T::empty(), T::empty(), T::empty(), T::empty()] }
}
