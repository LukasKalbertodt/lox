use std::{fmt, marker::PhantomData};

use crate::sealed::Sealed;


/// An optional and configurable field of a mesh data structure.
///
/// Often, data structures can store additional fields to speed up some
/// operations, while having a higher memory cost. This trait is just useful to
/// configure data structures, so you will see it in various `Config` traits.
///
/// This trait is only implemented by [`StoreField`] and [`OmitField`] and
/// cannot be implemented for other types. Further, the trait items are
/// implementation detail.
pub trait OptionalField: Sealed {
    #[doc(hidden)]
    type Storage<T: fmt::Debug + Copy>: FieldStorage<T>;
}

/// Configures a data structure to *store* an optional field.
#[allow(missing_debug_implementations)]
pub enum StoreField {}
impl Sealed for StoreField {}
impl OptionalField for StoreField {
    type Storage<T: fmt::Debug + Copy> = StoredField<T>;
}

/// Configures a data structure to *omit* an optional field.
#[allow(missing_debug_implementations)]
pub enum OmitField {}
impl Sealed for OmitField {}
impl OptionalField for OmitField {
    type Storage<T: fmt::Debug + Copy> = OmittedField<T>;
}

pub trait FieldStorage<T>: From<T> + fmt::Debug + Copy {
    fn into_option(self) -> Option<T>;
}

#[derive(Clone, Copy, Debug)]
pub struct StoredField<T>(T);

impl<T: Copy + fmt::Debug> FieldStorage<T> for StoredField<T> {
    #[inline(always)]
    fn into_option(self) -> Option<T> {
        Some(self.0)
    }
}

impl<T> From<T> for StoredField<T> {
    #[inline(always)]
    fn from(value: T) -> Self {
        Self(value)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct OmittedField<T>(PhantomData<T>);

impl<T: Copy + fmt::Debug> FieldStorage<T> for OmittedField<T> {
    #[inline(always)]
    fn into_option(self) -> Option<T> {
        None
    }
}

impl<T> From<T> for OmittedField<T> {
    #[inline(always)]
    fn from(_: T) -> Self {
        Self(PhantomData)
    }
}
