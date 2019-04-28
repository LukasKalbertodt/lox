use std::fmt;

use cgmath::{
    prelude::*,
    Point3,
};

use crate::{
    math::PrimitiveFloat,
    prop::Pos3Like,
};


/// A bounding sphere defined by a center and a radius.
#[derive(Debug, Clone, Copy)]
pub struct BoundingSphere<P: Pos3Like> {
    pub center: P,
    pub radius: P::Scalar,
}

/// Calculates a bounding sphere with the algorithm by Jack Ritter.
///
/// The returned bounding sphere is usually not minimal. This function iterates
/// over all points three times. The given points must not be empty or else
/// this function panics.
///
/// Reference: Ritter, Jack. "An efficient bounding sphere."
/// Graphics gems 1 (1990): 301-303.
pub fn ritter_sphere<I, ScalarT>(positions: I) -> BoundingSphere<I::Item>
where
    I: Iterator +  Clone,
    I::Item: Pos3Like<Scalar = ScalarT>,
    ScalarT: PrimitiveFloat,
{
    assert!(positions.clone().next().is_some(), "point set must not be empty");

    // Finds the point furthest away from `origin`.
    let furthest_away_from = |origin| {
        let mut max = ScalarT::zero();
        let mut out = origin;

        for p in positions.clone().map(|p| p.to_point3()) {
            let dist = p.distance2(origin);
            if dist > max {
                max = dist;
                out = p;
            }
        }

        out
    };

    // First, take a random point and find another point with maximum distance
    // to it. Then find a point with maximum distance to *that*.
    let x = positions.clone().next().unwrap().to_point3();
    let y = furthest_away_from(x);
    let z = furthest_away_from(y);

    // The center is the midpoint between y and z.
    let center = Point3::centroid(&[y, z]);
    let mut radius = y.distance(center);

    // If any point lies outside the sphere, we grow the sphere to include it.
    // The center stays the same.
    for p in positions {
        let dist = p.to_point3().distance(center);
        if dist > radius {
            radius = dist;
        }
    }

    BoundingSphere { center: center.convert(), radius }
}

/// A fast algorithm to obtain a bounding sphere of a set of points.
///
/// The returned bounding sphere is usually not minimal. This algorithm is very
/// simple: first, the axis aligned bounding box is calculated. The center of
/// that bounding box is the center of the resulting bounding sphere. The
/// radius of the sphere is simply the maximum distance from the center to a
/// point. As such, this function iterates over all points twice (e.g. the
/// Richter algorithm iterates over all points three times).
///
/// The given points must not be empty or else this function panics.
pub fn fast_sphere<I, ScalarT>(positions: I) -> BoundingSphere<I::Item>
where
    I: Iterator +  Clone,
    I::Item: Pos3Like<Scalar = ScalarT>,
    ScalarT: PrimitiveFloat,
{
    assert!(positions.clone().next().is_some(), "point set must not be empty");

    // Calculate bounding box, take it's center, find vertex with maximum
    // distance to that center, use its distance as radius.
    let bounding_box = BoundingBox::around(positions.clone());
    let center = bounding_box.center();
    let radius = positions
        .map(|p| p.to_point3().distance(center))
        .max_by(|a, b| a.partial_cmp(b).unwrap())
        .unwrap();

    BoundingSphere { center: center.convert(), radius }
}


/// An axis aligned bounding box.
pub struct BoundingBox<F: PrimitiveFloat> {
    x_range: [F; 2],
    y_range: [F; 2],
    z_range: [F; 2],
}

impl<F: PrimitiveFloat> BoundingBox<F> {
    /// Creates an invalid bounding box: all lower bounds are ∞, all upper
    /// bounds are -∞. Once you added a single point, the bounding box will be
    /// valid.
    pub fn new() -> Self {
        Self {
            x_range: [F::infinity(), F::neg_infinity()],
            y_range: [F::infinity(), F::neg_infinity()],
            z_range: [F::infinity(), F::neg_infinity()],
        }
    }

    /// Creates a bounding box around all points of the given iterator. If the
    /// iterator is empty, an invalid bounding box is returned (see
    /// [`BoundingBox::new`]).
    pub fn around<I>(iter: I) -> Self
    where
        I: IntoIterator,
        I::Item: Pos3Like<Scalar = F>,
    {
        let mut out = Self::new();
        for pos in iter {
            out.add_point(pos);
        }
        out
    }

    /// Returns the `[lower, upper]` limits for the x coordinate.
    pub fn x(&self) -> [F; 2] {
        self.x_range
    }

    /// Returns the `[lower, upper]` limits for the y coordinate.
    pub fn y(&self) -> [F; 2] {
        self.y_range
    }

    /// Returns the `[lower, upper]` limits for the z coordinate.
    pub fn z(&self) -> [F; 2] {
        self.z_range
    }

    /// Returns the center of this bounding box.
    pub fn center(&self) -> Point3<F> {
        Point3::new(
            (self.x_range[1] + self.x_range[0]) / F::from_f32(2.0),
            (self.y_range[1] + self.y_range[0]) / F::from_f32(2.0),
            (self.z_range[1] + self.z_range[0]) / F::from_f32(2.0),
        )
    }

    /// Adds a point to the bounding box, enlarging it if the point lies
    /// outside of the box.
    pub fn add_point<P: Pos3Like<Scalar = F>>(&mut self, p: P) {
        fn min<F: PartialOrd>(state: &mut F, new: F) {
            if new < *state {
                *state = new;
            }
        }
        fn max<F: PartialOrd>(state: &mut F, new: F) {
            if new > *state {
                *state = new;
            }
        }


        min(&mut self.x_range[0], p.x());
        max(&mut self.x_range[1], p.x());
        min(&mut self.y_range[0], p.y());
        max(&mut self.y_range[1], p.y());
        min(&mut self.z_range[0], p.z());
        max(&mut self.z_range[1], p.z());
    }

    /// Returns `true` if all bounds are finite.
    pub fn is_valid(&self) -> bool {
        self.x_range[0].is_finite()
            && self.x_range[1].is_finite()
            && self.y_range[0].is_finite()
            && self.y_range[1].is_finite()
            && self.z_range[0].is_finite()
            && self.z_range[1].is_finite()
    }
}

impl<F: PrimitiveFloat> fmt::Debug for BoundingBox<F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("BoundingBox")
            .field("x", &(self.x_range[0]..self.x_range[1]))
            .field("y", &(self.y_range[0]..self.y_range[1]))
            .field("z", &(self.z_range[0]..self.z_range[1]))
            .finish()
    }
}
