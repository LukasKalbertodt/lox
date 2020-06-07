//! API to retrieve some semantic information about standard elements and their
//! properties from a PLY file.
//!
//! Just like the `raw` module, you probably don't need to use any of this. The
//! "entry point" is [`ElementInfo`].

use std::convert::TryInto;

use crate::{
    handle::hsize,
    io::{Error, ErrorKind},
};
use super::{
    EDGE_ELEMENT_NAMES,
    FACE_ELEMENT_NAMES,
    VERTEX_ELEMENT_NAMES,
    raw::{ElementDef, PropertyType, PropIndex, ScalarType},
};


/// Information about elements and their semantic properties. Stores the indices
/// and scalar types of those properties.
#[derive(Debug, Clone)]
pub struct ElementInfo {
    pub vertex: VertexInfo,
    pub face: Option<FaceInfo>,
    pub edge: Option<EdgeInfo>,
}

/// Information about the 'vertex' element.
#[derive(Debug, Clone)]
pub struct VertexInfo {
    pub  count: hsize,
    pub  position: Option<Vec3PropInfo>,
    pub  normal: Option<Vec3PropInfo>,
    pub  color: Option<ColorPropInfo>,
}

/// Information about the 'face' element.
#[derive(Debug, Clone)]
pub struct FaceInfo {
    pub count: hsize,
    pub vertex_indices: VertexIndicesInfo,
    pub normal: Option<Vec3PropInfo>,
    pub color: Option<ColorPropInfo>,
}

/// Information about the 'edge' element.
#[derive(Debug, Clone)]
pub struct EdgeInfo {
    pub count: hsize,
    pub endpoints: EdgeEndpointsInfo,
    pub color: Option<ColorPropInfo>,
}

/// Information about the `vertex_indices` property of the 'face' element.
#[derive(Debug, Clone, Copy)]
pub struct VertexIndicesInfo {
    pub ty: ScalarType,
    pub idx: PropIndex,
}

/// Information about the `vertex1` and `vertex2` properties defining the two
/// endpoints of an edge.
#[derive(Debug, Clone, Copy)]
pub struct EdgeEndpointsInfo {
    pub idx: [PropIndex; 2],
    pub ty: ScalarType,
}

/// Information about a 3-element property (position or normal).
#[derive(Debug, Clone, Copy)]
pub struct Vec3PropInfo {
    pub ty: ScalarType,
    pub idx: Vec3PropIndex,
}

/// Index information about the three single elements of a vec3 like property.
#[derive(Debug, Clone, Copy)]
pub enum Vec3PropIndex {
    /// All three properties are right next to each other and in the right order
    /// (x, y, z). The `PropIndex` denotes the index of the first (x) property.
    Contiguous(PropIndex),
    /// Unusual case: the properties are in the wrong order or not next to each
    /// other. The indices are given separately for x, y, and z in order.
    Separate([PropIndex; 3]),
}

/// Information about a color property.
#[derive(Debug, Clone, Copy)]
pub enum ColorPropInfo {
    /// No alpha value, the rgb properties are right next to each other and in
    /// the right order. The `PropIndex` denotes the index of the r property.
    ContiguousRgb(PropIndex),
    /// No alpha value. Unusual case: the properties are in the wrong order or
    /// not next to each other. The indices are given separately for r, g, and b
    /// in order.
    SeparateRgb([PropIndex; 3]),
    /// With alpha value, the rgba properties are right next to each other and
    /// in the right order. The `PropIndex` denotes the index of the r property.
    ContiguousRgba(PropIndex),
    /// With alpha value. Unusual case: the properties are in the wrong order or
    /// not next to each other. The indices are given separately for r, g, b,
    /// and a in order.
    SeparateRgba([PropIndex; 4]),
}


impl ElementInfo {
    /// Gathers semantic information about vertex, face and edge elements and
    /// their respective properties.
    ///
    /// Vertices are required, the other elements are not. Furthermore, faces
    /// have to be stored after vertices and edges have to be stored after
    /// faces. An error is returned if any of those properties is violated.
    pub fn new(elements: &[ElementDef]) -> Result<Self, Error> {
        let vertex_pos = elements.iter()
            .position(|e| VERTEX_ELEMENT_NAMES.contains(&e.name.as_str()))
            .ok_or_else(|| invalid_input!("no 'vertex' elements in PLY file"))?;

        let face_pos = elements.iter().position(|e| FACE_ELEMENT_NAMES.contains(&e.name.as_str()));
        if let Some(face_pos) = face_pos {
            // Faces can only be in the file after vertices.
            if face_pos < vertex_pos {
                return Err(invalid_input!(
                    "found 'face' elements before 'vertex' elements (that's not allowed)"
                ));
            }
        }

        let edge_pos = elements.iter().position(|e| EDGE_ELEMENT_NAMES.contains(&e.name.as_str()));
        if let Some(edge_pos) = edge_pos {
            // Edges can only be in the file after vertices and faces.
            if face_pos.is_none() || edge_pos < face_pos.unwrap() {
                let problem = if face_pos.is_none() { "but no" } else { "before" };
                return Err(invalid_input!(
                    "found 'edge' elements {} 'face' elements (that's not allowed as \
                        LOX can't add edges on their own; edges always need to be part of a face)",
                    problem,
                ));
            }
        }

        Ok(Self {
            vertex: VertexInfo::new(&elements[vertex_pos])?,
            face: face_pos.map(|pos| FaceInfo::new(&elements[pos])).transpose()?,
            edge: edge_pos.map(|pos| EdgeInfo::new(&elements[pos])).transpose()?,
        })
    }
}

impl VertexInfo {
    pub fn new(group: &ElementDef) -> Result<Self, Error> {
        Ok(Self {
            count: u64_to_hsize(group.count, "vertices")?,
            position: Vec3PropInfo::new(group, ["x", "y", "z"], "positions")?,
            normal: Vec3PropInfo::new(group, ["nx", "ny", "nz"], "normals")?,
            color: ColorPropInfo::new(group)?,
        })
    }
}

impl FaceInfo {
    pub fn new(group: &ElementDef) -> Result<Self, Error> {
        Ok(Self {
            count: u64_to_hsize(group.count, "faces")?,
            vertex_indices: VertexIndicesInfo::new(group)?,
            normal: Vec3PropInfo::new(group, ["nx", "ny", "nz"], "normals")?,
            color: ColorPropInfo::new(group)?,
        })
    }
}

impl EdgeInfo {
    pub fn new(group: &ElementDef) -> Result<Self, Error> {
        Ok(Self {
            count: u64_to_hsize(group.count, "edges")?,
            endpoints: EdgeEndpointsInfo::new(group)?,
            color: ColorPropInfo::new(group)?,
        })
    }
}

impl VertexIndicesInfo {
    /// Gathers information about the `vertex_indices` property.
    ///
    /// This property is treated as required: if it doesn't exist in the given
    /// group, an error is returned. Additionally, an error is returned if the
    /// property is not a list or the list scalar type is floating point.
    pub fn new(group: &ElementDef) -> Result<Self, Error> {
        let vi_idx = match group.prop_pos("vertex_indices") {
            Some(x) => x,
            None => {
                return Err(invalid_input!("'face' elements without 'vertex_indices' property"));
            }
        };

        let vi = &group.property_defs[vi_idx];
        if !vi.ty.is_list() {
            return Err(
                invalid_input!("'vertex_indices' property has a scalar type (must be a list)")
            );
        }

        if vi.ty.scalar_type().is_floating_point() {
            return Err(invalid_input!(
                "'vertex_indices' list has a floating point element type (only \
                    integers are allowed)"
            ));
        }

        Ok(Self {
            ty: vi.ty.scalar_type(),
            idx: vi_idx,
        })
    }
}

impl EdgeEndpointsInfo {
    /// Gathers information about the properties "vertex1" and "vertex2".
    ///
    /// The properties are required; if they are missing, an error is returned.
    /// Furthermore, an error is returned if either of those properties is a
    /// list or has a floating point type.
    pub fn new(group: &ElementDef) -> Result<Self, Error> {
        let v1_idx = group.prop_pos("vertex1");
        let v2_idx = group.prop_pos("vertex2");

        // The properties `vertex1` and `vertex2` are required.
        let (v1_idx, v2_idx) = match (v1_idx, v2_idx) {
            (Some(a), Some(b)) => (a, b),
            _ => {
                return Err(invalid_input!(
                    "'edge' element is missing 'vertex1' or 'vertex2' property (or both)"
                ));
            }
        };

        for &(name, idx) in &[("vertex1", v1_idx), ("vertex2", v2_idx)] {
            let def = &group.property_defs[idx];
            if def.ty.is_list() {
                return Err(
                    invalid_input!("'{}' property has a list type (must be a scalar)", name)
                );
            }

            if def.ty.scalar_type().is_floating_point() {
                return Err(invalid_input!(
                    "'{}' list has a floating point element type (only integers are allowed)",
                    name
                ));
            }
        }

        Ok(Self {
            idx: [v1_idx, v2_idx],
            ty: group.property_defs[v1_idx].ty.scalar_type(),
        })
    }
}

impl Vec3PropInfo {
    /// Gathers information about a three-element property with the property
    /// names given in `names` within the given element `group`.
    ///
    /// If the first property name is not found, `Ok(None)` is returned. If it
    /// is found, the two other properties are required -- if they are not
    /// found, an error is returned. Additionally, the following things are
    /// checked (and an error returned if they are violated):
    /// - all three properties must have the same type
    /// - the type of the properties must be a scalar type (not a list)
    ///
    /// If everything works well and the three properties are found,
    /// `Ok(Some(_))` is returned.
    ///
    /// `prop_name_plural` is a human-readable name/description for the property
    /// you are searching for. It's used for error messages. Typical uses of
    /// this method are:
    /// - `Vec3PropInfo::new(_, ["x", "y", "z"], "positions")`
    /// - `Vec3PropInfo::new(_, ["nx", "ny", "nz"], "normals")`
    pub fn new(
        group: &ElementDef,
        names: [&str; 3],
        prop_name_plural: &str,
    ) -> Result<Option<Self>, Error> {
        let [xs, ys, zs] = names;

        let px_idx = match group.prop_pos(xs) {
            Some(x) => x,
            None => return Ok(None),
        };

        let py_idx = group.prop_pos(ys).ok_or_else(|| invalid_input!(
            "element '{}' has '{}' property, but no '{}' property (only 3D {} supported)",
            group.name,
            xs,
            ys,
            prop_name_plural,
        ))?;
        let pz_idx = group.prop_pos(zs).ok_or_else(|| invalid_input!(
            "elem '{}' has '{}' property, but no '{}' property (only 3D {} supported)",
            group.name,
            xs,
            zs,
            prop_name_plural,
        ))?;

        let px = &group.property_defs[px_idx];
        let py = &group.property_defs[py_idx];
        let pz = &group.property_defs[pz_idx];

        if px.ty.is_list() {
            return Err(invalid_input!(
                "property '{}' (element '{}') has a list type (only scalars allowed)",
                xs,
                group.name,
            ));
        }

        if px.ty != py.ty || px.ty != pz.ty {
            return Err(invalid_input!(
                "properties '{}', '{}' and '{}' (element '{}') don't have the same type",
                xs,
                ys,
                zs,
                group.name,
            ));
        }

        Ok(Some(Self {
            ty: px.ty.scalar_type(),
            idx: Vec3PropIndex::new([px_idx, py_idx, pz_idx]),
        }))
    }
}

impl Vec3PropIndex {
    /// Creates a new instance from three given indices. If the indices are
    /// contiguous, `Self::Contiguous` is returned, `Self::Separate` otherwise.
    pub fn new([x, y, z]: [PropIndex; 3]) -> Self {
        if y.0 == x.0 + 1 && z.0 == y.0 + 1 {
            Self::Contiguous(x)
        } else {
            Self::Separate([x, y, z])
        }
    }

    /// Returns the indices of all properties.
    pub fn indices(&self) -> [PropIndex; 3] {
        match *self {
            Self::Contiguous(x) => [x, PropIndex(x.0 + 1), PropIndex(x.0 + 2)],
            Self::Separate(all) => all,
        }
    }
}

impl ColorPropInfo {
    /// Gathers information about a color property (names 'red', 'green, 'blue'
    /// and (optionally) 'alpha') in the given 'group'
    ///
    /// If no 'red' property is found, `OK(None)` is returned. If it is found,
    /// 'green' and 'blue' have to exist as well or else an error is returned.
    /// All properties (including 'alpha') must have the type `uchar` (this is
    /// apparently the only color format PLY supports). If that's not the case,
    /// an error is returned.
    ///
    /// If everything goes well, `Ok(Some(_))` is returned.
    pub fn new(group: &ElementDef) -> Result<Option<Self>, Error> {
        let red_idx = match group.prop_pos("red") {
            Some(x) => x,
            None => return Ok(None),
        };

        let green_idx = group.prop_pos("green").ok_or_else(|| invalid_input!(
            "element '{}' has 'red' property, but no 'green' property \
                (only RGB and RGBA colors supported)",
            group.name,
        ))?;
        let blue_idx = group.prop_pos("blue").ok_or_else(|| invalid_input!(
            "element '{}' has 'red' property, but no 'blue' property \
                (only RGB and RGBA colors supported)",
            group.name,
        ))?;

        let red = &group.property_defs[red_idx];
        let green = &group.property_defs[green_idx];
        let blue = &group.property_defs[blue_idx];

        let check_type = |name, ty: PropertyType| {
            if ty.is_list() {
                return Err(invalid_input!(
                    "property '{}' (element '{}') is a list (should be scalar 'uchar')",
                    name,
                    group.name,
                ));
            }

            if ty.scalar_type() != ScalarType::UChar {
                return Err(invalid_input!(
                    "property '{}' (element '{}') has type '{}' (should be 'uchar')",
                    name,
                    group.name,
                    ty.scalar_type().ply_type_name(),
                ));
            }

            Ok(())
        };

        check_type("red", red.ty)?;
        check_type("green", green.ty)?;
        check_type("blue", blue.ty)?;

        let rgb_contiguous = red_idx.0 + 1 == green_idx.0 && green_idx.0 + 1 == blue_idx.0;

        let out = match (group.prop_pos("alpha"), rgb_contiguous) {
            (Some(alpha_idx), _) => {
                let alpha = &group.property_defs[alpha_idx];
                check_type("alpha", alpha.ty)?;

                if rgb_contiguous && blue_idx.0 + 1 == alpha_idx.0 {
                    Self::ContiguousRgba(red_idx)
                } else {
                    Self::SeparateRgba([red_idx, green_idx, blue_idx, alpha_idx])
                }
            }
            (None, true) => Self::ContiguousRgb(red_idx),
            (None, false) => Self::SeparateRgb([red_idx, green_idx, blue_idx]),
        };

        Ok(Some(out))
    }

    pub fn has_alpha(&self) -> bool {
        match self {
            Self::ContiguousRgb(_) => false,
            Self::SeparateRgb(_) => false,
            Self::ContiguousRgba(_) => true,
            Self::SeparateRgba(_) => true,
        }
    }
}

// Tries to convert the given `u64` value to a `hsize`. If the value is too
// large, a descriptive `InvalidInput` error is returned.
fn u64_to_hsize(v: u64, elem: &str) -> Result<hsize, Error> {
    v.try_into().map_err(|_| invalid_input!(
        "too many {} (LOX meshes can only contain 2^32 elements, \
            unless you enabled the 'large-handle' feature)",
        elem,
    ))
}
