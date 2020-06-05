//! Types and traits for the raw PLY API. You probably don't need this.
//!
//! These are low level building blocks for reading and writing PLY files. The
//! PLY format is rather complex as it supports arbitrary properties, arbitrary
//! elements and dynamic typing (properties can have a number of different
//! types). The high level APIs via `StreamSink` and `StreamSource` are built
//! using these building blocks.
//!
//! You usually can just use the high level API, but if you have very special
//! needs, the raw API could help you. For reading files, the entry point is
//! [`Reader::read_raw`][super::Reader::read_raw]. For writing files, it's
//! [`Writer::write_raw`][super::Writer::write_raw].

use std::{
    fmt,
    ops,
    str::FromStr,
};

use byteorder::{ByteOrder, NativeEndian};
use derive_more::{Add, AddAssign, From, Sub, SubAssign};
use smallvec::SmallVec;

use crate::{
    Empty,
    io::{Error, ErrorKind, Primitive, PrimitiveType},
};


// ===========================================================================
// ===== RawSink
// ===========================================================================
/// A type that can accept raw data from a PLY file. This is used for
/// [`Reader::read_raw`][super::Reader::read_raw].
// TODO: implement for `Writer`?
pub trait RawSink {
    /// Is called when a new element group begins.
    ///
    /// `def` describes the layout of all elements in this group. This method
    /// is *always* called before `element` is called.
    fn element_group_start(&mut self, def: &ElementDef) -> Result<(), Error>;

    /// Is called for each element that is read. When called, the element
    /// belongs to the last element group (the last `element_group_start`
    /// call).
    fn element(&mut self, elem: &RawElement) -> Result<(), Error>;
}

/// A type that can provide raw body data for a PLY file. Used for
/// [`Writer::write_raw`][super::Writer::write_raw].
// TODO: implement for `Reader` (?) and `RawStorage`!
pub trait RawSource {
    /// Is only called once with some "serializer". The source is then required
    /// to write correct data through the serializer.
    ///
    /// Again, this is a low level interface and the implementor of `RawSource`
    /// has to make sure to write correct data. Lists have to be serialized
    /// correctly by first writing the length with the correct type and then
    /// the elements.
    fn serialize_into<S: Serializer>(self, ser: S) -> Result<(), Error>;
}

/// Abstraction over a PLY encoding. is used by [`RawSource`] to write data.
pub trait Serializer {
    /// Encoding a single value into the serializer.
    fn add<P: PlyScalar>(&mut self, v: P) -> Result<(), Error>;

    /// Encode a slice of values into the serializer. If you have multiple
    /// values of the same type, use this function to improve performance. The
    /// slice is a mutable slice because the method implementation might want
    /// to mutate it without creating a copy (e.g. changing endianess).
    fn add_slice<P: PlyScalar>(&mut self, s: &mut [P]) -> Result<(), Error> {
        for x in s {
            self.add(*x)?;
        }
        Ok(())
    }

    /// Call this whenever one element is finished. This is used for the line
    /// break in ASCII encoding.
    fn end_element(&mut self) -> Result<(), Error>;
}

/// A PLY scalar type. This trait is only for internal use. You can't implement
/// it for your own types.
pub trait PlyScalar: Primitive + fmt::Display {
    /// Converts the slice into the given endianness.
    fn to_endianness<E: ByteOrder>(s: &mut [Self]);
}

macro_rules! impl_ply_scalar {
    ($ty:ty, $fun:ident) => {
        impl PlyScalar for $ty {
            fn to_endianness<E: ByteOrder>(s: &mut [Self]) {
                E::$fun(s)
            }
        }
    }
}

impl_ply_scalar!(i16, from_slice_i16);
impl_ply_scalar!(i32, from_slice_i32);
impl_ply_scalar!(u16, from_slice_u16);
impl_ply_scalar!(u32, from_slice_u32);
impl_ply_scalar!(f32, from_slice_f32);
impl_ply_scalar!(f64, from_slice_f64);

impl PlyScalar for i8 {
    fn to_endianness<E: ByteOrder>(_s: &mut [Self]) {}
}
impl PlyScalar for u8 {
    fn to_endianness<E: ByteOrder>(_s: &mut [Self]) {}
}




// ===========================================================================
// ===== Data structures to hold header data of a PLY file
// ===========================================================================
/// The header definition of one element group.
#[derive(Debug, Clone)]
pub struct ElementDef {
    pub name: String,

    /// Number of elements in this group.
    pub count: u64,

    /// Definitions for all properties of elements in this group.
    pub property_defs: PropVec<PropertyDef>,
}

impl ElementDef {
    pub fn prop_pos(&self, prop_name: &str) -> Option<PropIndex> {
        self.property_defs.iter()
            .position(|p| p.name == prop_name)
            .map(|idx| PropIndex(idx as u8))
    }

    /// Attempts to check for a three-element property with the property names
    /// given in `names`.
    ///
    /// `prop_name_plural` is a human-readable name/description for the
    /// property you are searching for. Typical uses of this method are:
    /// - `.check_vec3_prop(["x", "y", "z"], "positions")`
    /// - `.check_vec3_prop(["nx", "ny", "nz"], "normals")`
    ///
    /// If the first property name is not found, `Ok(None)` is returned. If it
    /// is found, the two other properties are required -- if they are not
    /// found, an error is returned. Additionally, the following things are
    /// checked (and an error returned if they are violated):
    /// - all three properties must have the same type
    /// - the type of the properties must be a scalar type (not a list)
    ///
    /// If everything works well and the three properties are found,
    /// `Ok(Some((offsets, type)))` is returned. `offsets` describes the offset
    /// in the `property_def` vector for the three properties (in the same
    /// order as `names`). `type` represents the type of all three properties.
    pub fn check_vec3_prop(
        &self,
        names: [&str; 3],
        prop_name_plural: &str,
    ) -> Result<Option<([PropIndex; 3], ScalarType)>, Error> {
        let [xs, ys, zs] = names;
        if let Some(px_idx) = self.prop_pos(xs) {
            let py_idx = self.prop_pos(ys).ok_or_else(|| Error::new(|| ErrorKind::InvalidInput(
                format!(
                    "element '{}' has '{}' property, but no '{}' property \
                        (only 3D {} supported)",
                    self.name,
                    xs,
                    ys,
                    prop_name_plural,
                )
            )))?;
            let pz_idx = self.prop_pos(zs).ok_or_else(|| Error::new(|| ErrorKind::InvalidInput(
                format!(
                    "elem '{}' has '{}' property, but no '{}' property \
                        (only 3D {} supported)",
                    self.name,
                    xs,
                    zs,
                    prop_name_plural,
                )
            )))?;

            let px = &self.property_defs[px_idx];
            let py = &self.property_defs[py_idx];
            let pz = &self.property_defs[pz_idx];

            if px.ty.is_list() {
                return Err(Error::new(|| ErrorKind::InvalidInput(
                    format!(
                        "property '{}' (element '{}') has a list type (only scalars allowed)",
                        xs,
                        self.name,
                    )
                )));
            }

            if px.ty != py.ty || px.ty != pz.ty {
                return Err(Error::new(|| ErrorKind::InvalidInput(
                    format!(
                        "properties '{}', '{}' and '{}' (element '{}') don't have the same type",
                        xs,
                        ys,
                        zs,
                        self.name,
                    )
                )));
            }

            Ok(Some((
                [px_idx, py_idx, pz_idx],
                px.ty.scalar_type(),
            )))
        } else {
            Ok(None)
        }
    }

    /// Checks for a color property (names 'red', 'green, 'blue and
    /// (optionally) 'alpha').
    ///
    /// If no 'red' property is found, `OK(None)` is returned. If it is found,
    /// 'green' and 'blue' have to exist as well or else an error is returned.
    /// All properties (including 'alpha') must have the type `uchar` (this is
    /// apparently the only color format PLY supports). If that's not the case,
    /// an error is returned.
    ///
    /// If everything goes well, `Ok(Some((offsets, contains_alpha)))` is
    /// returned.
    ///
    /// There can be an optional 'alpha' property. If one is found, `offsets`
    /// contains the offsets (in the `property_def` vector) of the four
    /// properties (in order RGBA) and `contains_alpha` is `true`. If there is
    /// no 'alpha' property, `contains_alpha` is `false` and `offsets` contains
    /// the offsets of the RGB properties and a `0` as last element (this is a
    /// dummy value).
    pub fn check_color_prop(&self) -> Result<Option<([PropIndex; 4], bool)>, Error> {
        if let Some(red_idx) = self.prop_pos("red") {
            let green_idx = self.prop_pos("green")
                .ok_or_else(|| Error::new(|| ErrorKind::InvalidInput(
                    format!(
                        "element '{}' has 'red' property, but no 'green' property \
                            (only RGB and RGBA colors supported)",
                        self.name,
                    )
                )))?;
            let blue_idx = self.prop_pos("blue")
                .ok_or_else(|| Error::new(|| ErrorKind::InvalidInput(
                    format!(
                        "element '{}' has 'red' property, but no 'blue' property \
                            (only RGB and RGBA colors supported)",
                        self.name,
                    )
                )))?;

            let red = &self.property_defs[red_idx];
            let green = &self.property_defs[green_idx];
            let blue = &self.property_defs[blue_idx];

            let check_type = |name, ty: PropertyType| {
                if ty.is_list() {
                    return Err(Error::new(|| ErrorKind::InvalidInput(
                        format!(
                            "property '{}' (element '{}') is a list (should be scalar 'uchar')",
                            name,
                            self.name,
                        )
                    )));
                }

                if ty.scalar_type() != ScalarType::UChar {
                    return Err(Error::new(|| ErrorKind::InvalidInput(
                        format!(
                            "property '{}' (element '{}') has type '{}' (should be 'uchar')",
                            name,
                            self.name,
                            ty.scalar_type().ply_type_name(),
                        )
                    )));
                }

                Ok(())
            };

            check_type("red", red.ty)?;
            check_type("green", green.ty)?;
            check_type("blue", blue.ty)?;

            let (alpha_idx, alpha) = if let Some(alpha_idx) = self.prop_pos("alpha") {
                let alpha = &self.property_defs[alpha_idx];
                check_type("alpha", alpha.ty)?;
                (alpha_idx, true)
            } else {
                (PropIndex(0), false)
            };

            Ok(Some((
                [red_idx, green_idx, blue_idx, alpha_idx],
                alpha,
            )))
        } else {
            Ok(None)
        }
    }
}

/// Te header definition of one property of an element.
#[derive(Debug, Clone)]
pub struct PropertyDef {
    pub ty: PropertyType,
    pub name: String,
}

/// The type of a PLY property: either a list or a scalar.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PropertyType {
    Scalar(ScalarType),
    List {
        len_type: ListLenType,
        scalar_type: ScalarType,
    }
}

impl PropertyType {
    /// Returns the `len_type` of the list, or `None` if `self` is a scalar
    /// value.
    pub fn len_type(&self) -> Option<ListLenType> {
        match self {
            PropertyType::Scalar(_) => None,
            PropertyType::List { len_type, .. } => Some(*len_type),
        }
    }

    /// Returns the scalar type of the list or the scalar.
    pub fn scalar_type(&self) -> ScalarType {
        match *self {
            PropertyType::Scalar(scalar_type) => scalar_type,
            PropertyType::List { scalar_type, .. } => scalar_type,
        }
    }

    /// Returns `true` if this type is a list type.
    pub fn is_list(&self) -> bool {
        self.len_type().is_some()
    }
}

/// A subset of `ScalarType`: types that can be used to store the length of a
/// list.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ListLenType {
    UChar,
    UShort,
    UInt,
}

impl ListLenType {
    /// Returns the given type as `ListLenType`, or `None` if the given scalar
    /// type is not a valid list length type.
    pub fn from_scalar_type(ty: ScalarType) -> Option<Self> {
        match ty {
            ScalarType::UChar => Some(ListLenType::UChar),
            ScalarType::UShort => Some(ListLenType::UShort),
            ScalarType::UInt => Some(ListLenType::UInt),
            _ => None,
        }
    }

    pub fn to_scalar_type(self) -> ScalarType {
        match self {
            ListLenType::UChar => ScalarType::UChar,
            ListLenType::UShort => ScalarType::UShort,
            ListLenType::UInt => ScalarType::UInt,
        }
    }

    /// Returns the number of bytes this type occupies.
    pub fn len(&self) -> ScalarLen {
        match self {
            ListLenType::UChar => ScalarLen::One,
            ListLenType::UShort => ScalarLen::Two,
            ListLenType::UInt => ScalarLen::Four,
        }
    }
}

/// All allowed PLY scalar types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarType {
    Char,
    Short,
    Int,
    UChar,
    UShort,
    UInt,
    Float,
    Double,
}

impl ScalarType {
    pub fn to_primitive_type(&self) -> PrimitiveType {
        match self {
            ScalarType::Char => PrimitiveType::Int8,
            ScalarType::Short => PrimitiveType::Int16,
            ScalarType::Int => PrimitiveType::Int32,
            ScalarType::UChar => PrimitiveType::Uint8,
            ScalarType::UShort => PrimitiveType::Uint16,
            ScalarType::UInt => PrimitiveType::Uint32,
            ScalarType::Float => PrimitiveType::Float32,
            ScalarType::Double => PrimitiveType::Float64,
        }
    }

    /// Returns `true` if and only if the type is either `float` or `double`.
    pub fn is_floating_point(&self) -> bool {
        *self == ScalarType::Float || *self == ScalarType::Double
    }

    /// Returns `true` if and only if the type is one of `uchar`, `ushort` or
    /// `uint`.
    pub fn is_unsigned_integer(&self) -> bool {
        match self {
            ScalarType::UChar | ScalarType::UShort | ScalarType::UInt => true,
            _ => false,
        }
    }

    /// Returns `true` if and only if the type is one of `char`, `short` or
    /// `int`.
    pub fn is_signed_integer(&self) -> bool {
        match self {
            ScalarType::Char | ScalarType::Short | ScalarType::Int => true,
            _ => false,
        }
    }

    /// Returns the number of bytes this type occupies.
    pub fn len(&self) -> ScalarLen {
        match self {
            ScalarType::Char => ScalarLen::One,
            ScalarType::Short => ScalarLen::Two,
            ScalarType::Int => ScalarLen::Four,
            ScalarType::UChar => ScalarLen::One,
            ScalarType::UShort => ScalarLen::Two,
            ScalarType::UInt => ScalarLen::Four,
            ScalarType::Float => ScalarLen::Four,
            ScalarType::Double => ScalarLen::Eight,
        }
    }

    /// Returns the type name used in the header (e.g. `short`). This is simply
    /// the variant name in lowercase.
    pub fn ply_type_name(&self) -> &'static str {
        match *self {
            ScalarType::Char => "char",
            ScalarType::Short => "short",
            ScalarType::Int => "int",
            ScalarType::UChar => "uchar",
            ScalarType::UShort => "ushort",
            ScalarType::UInt => "uint",
            ScalarType::Float => "float",
            ScalarType::Double => "double",
        }
    }
}

impl FromStr for ScalarType {
    type Err = ScalarTypeParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "char" => Ok(ScalarType::Char),
            "short" => Ok(ScalarType::Short),
            "int" => Ok(ScalarType::Int),
            "uchar" => Ok(ScalarType::UChar),
            "ushort" => Ok(ScalarType::UShort),
            "uint" => Ok(ScalarType::UInt),
            "float" => Ok(ScalarType::Float),
            "double" => Ok(ScalarType::Double),
            other => Err(ScalarTypeParseError(other.to_string())),
        }
    }
}

/// The error emitted when the `FromStr` implementation for `ScalarType` cannot
/// parse the given string.
pub struct ScalarTypeParseError(String);

impl fmt::Display for ScalarTypeParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\"{}\" is not a valid PLY scalar type", self.0)
    }
}

impl fmt::Debug for ScalarTypeParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}



// ===========================================================================
// ===== `RawElement` and body data definitions
// ===========================================================================

/// Represents on element with all its property values.
///
/// The property values are encoded as a densely packed byte array. This type
/// also includes meta data about the properties. These metadata are mostly the
/// same for all elements of one element group.
#[derive(Clone)]
pub struct RawElement {
    /// The packed data of all properties in native endianess.
    pub data: RawData,

    /// Some meta information about each property in this element.
    pub prop_infos: PropVec<RawPropertyInfo>,
}

/// Metadata about a property.
#[derive(Debug, Clone)]
pub struct RawPropertyInfo {
    /// The byte offset in the packed, native-endian data at which this
    /// property starts.
    pub offset: RawOffset,

    /// The type of this property.
    pub ty: PropertyType,

    /// Name of this property.
    pub name: String,
}

impl RawElement {
    /// Decodes the list at the given property index and returns information
    /// about that list. Returns `None` if there is a scalar value at the given
    /// position. Panics if `idx` is invalid.
    pub fn decode_list_at(&self, idx: PropIndex) -> Option<RawListInfo> {
        match self.prop_infos[idx].ty {
            PropertyType::Scalar(_) => None,
            PropertyType::List { len_type, scalar_type } => {
                let len_offset = self.prop_infos[idx].offset;
                let list_len = match len_type {
                    ListLenType::UChar => self.data[len_offset] as u32,
                    ListLenType::UShort => NativeEndian::read_u16(&self.data[len_offset..]) as u32,
                    ListLenType::UInt => NativeEndian::read_u32(&self.data[len_offset..]),
                };

                Some(RawListInfo {
                    list_len,
                    len_offset,
                    data_offset: len_offset + len_type.len(),
                    len_type,
                    scalar_type,
                })
            }
        }
    }

    /// Decodes the property at the given index and returns it as dynamically
    /// typed `Property` value.
    ///
    /// This should be just used for testing or debugging. Storing many values
    /// as `Property` is space-inefficient and slow.
    fn prop_at(&self, idx: PropIndex) -> Property {
        fn read_scalar(buf: &[u8], ty: ScalarType) -> Property {
            match ty {
                ScalarType::Char => Property::Char(buf[0] as i8),
                ScalarType::UChar => Property::UChar(buf[0]),
                ScalarType::Short => Property::Short(NativeEndian::read_i16(buf)),
                ScalarType::UShort => Property::UShort(NativeEndian::read_u16(buf)),
                ScalarType::Int => Property::Int(NativeEndian::read_i32(buf)),
                ScalarType::UInt => Property::UInt(NativeEndian::read_u32(buf)),
                ScalarType::Float => Property::Float(NativeEndian::read_f32(buf)),
                ScalarType::Double => Property::Double(NativeEndian::read_f64(buf)),
            }
        }

        let info = &self.prop_infos[idx];

        match info.ty {
            PropertyType::Scalar(ty) => read_scalar(&self.data[info.offset..], ty),
            PropertyType::List { scalar_type, .. } => {
                let list_info = self.decode_list_at(idx).unwrap();

                macro_rules! list {
                    ($variant:ident, |$buf:ident| $e:expr) => {{
                        let mut list = SmallVec::new();
                        let mut offset = list_info.data_offset;
                        for _ in 0..list_info.list_len {
                            let $buf = &self.data[offset..];
                            list.push($e);
                            offset += list_info.scalar_type.len();
                        }

                        Property::$variant(list)
                    }}
                }

                match scalar_type {
                    ScalarType::Char => list!(CharList, |buf| buf[0] as i8),
                    ScalarType::UChar => list!(UCharList, |buf| buf[0]),
                    ScalarType::Short => list!(ShortList, |buf| NativeEndian::read_i16(buf)),
                    ScalarType::UShort => list!(UShortList, |buf| NativeEndian::read_u16(buf)),
                    ScalarType::Int => list!(IntList, |buf| NativeEndian::read_i32(buf)),
                    ScalarType::UInt => list!(UIntList, |buf| NativeEndian::read_u32(buf)),
                    ScalarType::Float => list!(FloatList, |buf| NativeEndian::read_f32(buf)),
                    ScalarType::Double => list!(DoubleList, |buf| NativeEndian::read_f64(buf)),
                }
            }
        }
    }

    /// Returns an iterator over all properties of this element.
    ///
    /// This iterator iterates over `Property` values and thus should also only
    /// be used for debugging or testing.
    pub fn iter(&self) -> RawElementIter<'_> {
        RawElementIter {
            elem: self,
            idx: 0.into(),
        }
    }
}

impl fmt::Debug for RawElement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = f.debug_struct("RawElement");

        for (prop, info) in self.iter().zip(self.prop_infos.iter()) {
            // TODO: maybe add offset
            s.field(&info.name, &prop);
        }

        s.finish()
    }
}

/// Iterator over the property values of a [`RawElement`]. Can be obtained via
/// [`RawElement::iter`].
#[derive(Debug)]
pub struct RawElementIter<'a> {
    elem: &'a RawElement,
    idx: PropIndex,
}

impl Iterator for RawElementIter<'_> {
    type Item = Property;
    fn next(&mut self) -> Option<Self::Item> {
        if self.elem.prop_infos.len() <= self.idx.as_usize() {
            None
        } else {
            let out = self.elem.prop_at(self.idx);
            self.idx += PropIndex::from(1);
            Some(out)
        }
    }
}

/// Meta data about a decoded list.
#[derive(Debug, Clone, Copy)]
pub struct RawListInfo {
    /// Number of elements in the list.
    pub list_len: u32,

    /// The byte offset of the length field in the [`RawElement`] this list is
    /// stored in.
    pub len_offset: RawOffset,

    /// The byte offset of the first list element in the [`RawElement`] this
    /// list is stored in.
    pub data_offset: RawOffset,

    /// The type in which the list length is stored.
    pub len_type: ListLenType,

    /// The type of the list's elements.
    pub scalar_type: ScalarType,
}


// ===========================================================================
// ===== Strongly typed wrapper for certain things
// ===========================================================================

/// A byte offset into the raw data of one element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Add, Sub, AddAssign, SubAssign, From)]
pub struct RawOffset(pub u32);

impl RawOffset {
    #[inline(always)]
    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

impl From<ScalarLen> for RawOffset {
    fn from(src: ScalarLen) -> Self {
        (src.as_u8() as u32).into()
    }
}

impl ops::Add<ScalarLen> for RawOffset {
    type Output = RawOffset;
    fn add(self, len: ScalarLen) -> Self::Output {
        (self.0 + len.as_u8() as u32).into()
    }
}

impl ops::AddAssign<ScalarLen> for RawOffset {
    fn add_assign(&mut self, rhs: ScalarLen) {
        *self = *self + rhs;
    }
}

/// Length of a PLY scalar value in bytes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarLen {
    One = 1,
    Two = 2,
    Four = 4,
    Eight = 8,
}

impl ScalarLen {
    pub fn as_u8(&self) -> u8 {
        *self as u8
    }
}

/// Index of a specific property in the ordered list of properties of one
/// element group. Can be used to index a [`PropVec`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, From, Add, Sub, AddAssign, SubAssign)]
pub struct PropIndex(pub u8);

impl PropIndex {
    #[inline(always)]
    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

/// Raw data of one element in native endianess. Can be indexed by `RawOffset`.
///
/// For PLY files stored in native endianess, this is an exact chunk from
/// the file. For ASCII files and files in non-native endianess, the
/// properties are first converted to this format.
#[derive(Debug, Clone, From)]
pub struct RawData(Vec<u8>);

impl ops::Index<RawOffset> for RawData {
    type Output = u8;
    fn index(&self, idx: RawOffset) -> &Self::Output {
        &self.0[idx.as_usize()]
    }
}

impl ops::Index<ops::Range<RawOffset>> for RawData {
    type Output = [u8];
    #[inline(always)]
    fn index(&self, range: ops::Range<RawOffset>) -> &Self::Output {
        &self.0[range.start.as_usize()..range.end.as_usize()]
    }
}

impl ops::Index<ops::RangeFrom<RawOffset>> for RawData {
    type Output = [u8];
    #[inline(always)]
    fn index(&self, range: ops::RangeFrom<RawOffset>) -> &Self::Output {
        &self.0[range.start.as_usize()..]
    }
}

impl ops::Index<ops::RangeTo<RawOffset>> for RawData {
    type Output = [u8];
    #[inline(always)]
    fn index(&self, range: ops::RangeTo<RawOffset>) -> &Self::Output {
        &self.0[..range.end.as_usize()]
    }
}

impl ops::Index<ops::RangeFull> for RawData {
    type Output = [u8];
    #[inline(always)]
    fn index(&self, _: ops::RangeFull) -> &Self::Output {
        &self.0[..]
    }
}

impl ops::IndexMut<RawOffset> for RawData {
    #[inline(always)]
    fn index_mut(&mut self, idx: RawOffset) -> &mut Self::Output {
        &mut self.0[idx.as_usize()]
    }
}

impl ops::IndexMut<ops::Range<RawOffset>> for RawData {
    #[inline(always)]
    fn index_mut(&mut self, range: ops::Range<RawOffset>) -> &mut Self::Output {
        &mut self.0[range.start.as_usize()..range.end.as_usize()]
    }
}

impl ops::IndexMut<ops::RangeFrom<RawOffset>> for RawData {
    #[inline(always)]
    fn index_mut(&mut self, range: ops::RangeFrom<RawOffset>) -> &mut Self::Output {
        &mut self.0[range.start.as_usize()..]
    }
}

impl ops::IndexMut<ops::RangeTo<RawOffset>> for RawData {
    #[inline(always)]
    fn index_mut(&mut self, range: ops::RangeTo<RawOffset>) -> &mut Self::Output {
        &mut self.0[..range.end.as_usize()]
    }
}

impl ops::IndexMut<ops::RangeFull> for RawData {
    #[inline(always)]
    fn index_mut(&mut self, _: ops::RangeFull) -> &mut Self::Output {
        &mut self.0[..]
    }
}

impl ops::Deref for RawData {
    type Target = Vec<u8>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ops::DerefMut for RawData {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// A vector that holds data for each property. Can be indexed by
/// [`PropIndex`].
///
/// This is simply a wrapper around a `Vec` to use strong typing. This should
/// only be indexed with `PropIndex`.
#[derive(Debug, Clone, From)]
pub struct PropVec<T>(Vec<T>);

impl<T> PropVec<T> {
    pub fn new() -> Self {
        Self(vec![])
    }
}

impl<T> ops::Index<PropIndex> for PropVec<T> {
    type Output = T;
    #[inline(always)]
    fn index(&self, idx: PropIndex) -> &Self::Output {
        &self.0[idx.as_usize()]
    }
}

impl<T> ops::Deref for PropVec<T> {
    type Target = Vec<T>;
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> ops::DerefMut for PropVec<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}



// ===========================================================================
// ===== Dynamically typed values and body data: `Property` & `RawStorage`
// ===========================================================================

/// One property value of some PLY type.
///
/// This is a fairly space inefficient representation of properties and it's
/// pretty slow. This should only be used for debugging and testing.
///
/// The sizes of the smallvecs are choosen so that the inline variant won't
/// inflict a size overhead (on x64). This still means that the most common
/// form of list, the three-tuple `vertex_indices`, will fit inline.
#[derive(Debug, Clone, PartialEq)]
pub enum Property {
    Char(i8),
    Short(i16),
    Int(i32),
    UChar(u8),
    UShort(u16),
    UInt(u32),
    Float(f32),
    Double(f64),
    CharList(SmallVec<[i8; 16]>),
    ShortList(SmallVec<[i16; 8]>),
    IntList(SmallVec<[i32; 4]>),
    UCharList(SmallVec<[u8; 16]>),
    UShortList(SmallVec<[u16; 8]>),
    UIntList(SmallVec<[u32; 4]>),
    FloatList(SmallVec<[f32; 4]>),
    DoubleList(SmallVec<[f64; 2]>),
}

impl Property {
    /// Returns the value as integer, or `None` if the property does not have
    /// an integer type.
    pub fn as_integer(&self) -> Option<i64> {
        match *self {
            Property::Char(v) => Some(v.into()),
            Property::Short(v) => Some(v.into()),
            Property::Int(v) => Some(v.into()),
            Property::UChar(v) => Some(v.into()),
            Property::UShort(v) => Some(v.into()),
            Property::UInt(v) => Some(v.into()),
            _ => None,
        }
    }

    /// Returns the value as unsigned integer, or `None` if the property does
    /// not have an unsigned integer type.
    pub fn as_unsigned_integer(&self) -> Option<u32> {
        match *self {
            Property::UChar(v) => Some(v.into()),
            Property::UShort(v) => Some(v.into()),
            Property::UInt(v) => Some(v),
            _ => None,
        }
    }

    /// Returns the value as signed integer, or `None` if the property does
    /// not have a signed integer type.
    pub fn as_signed_integer(&self) -> Option<i32> {
        match *self {
            Property::Char(v) => Some(v.into()),
            Property::Short(v) => Some(v.into()),
            Property::Int(v) => Some(v),
            _ => None,
        }
    }

    /// Returns the value as float, or `None` if the property does not have a
    /// float type.
    pub fn as_floating_point(&self) -> Option<f64> {
        match *self {
            Property::Float(v) => Some(v.into()),
            Property::Double(v) => Some(v),
            _ => None,
        }
    }
}

/// Contains all contents of a PLY file. Implements [`RawSink`].
#[derive(Debug, Clone, Empty)]
pub struct RawStorage {
    /// Usually there are two element groups: "vertex" and "face".
    pub element_groups: Vec<RawElementGroup>,
}

/// Represents one element group with header and body data.
#[derive(Debug, Clone)]
pub struct RawElementGroup {
    pub def: ElementDef,
    pub elements: Vec<RawElement>,
}

impl RawSink for RawStorage {
    fn element_group_start(&mut self, def: &ElementDef) -> Result<(), Error> {
        self.element_groups.push(RawElementGroup {
            def: def.clone(),
            elements: vec![],
        });
        Ok(())
    }

    fn element(&mut self, elem: &RawElement) -> Result<(), Error> {
        self.element_groups
            .last_mut()
            .unwrap()
            .elements
            .push(elem.clone());
        Ok(())
    }
}
