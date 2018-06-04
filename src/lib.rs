#![feature(crate_in_paths)]
#![feature(non_modrs_mods)]
#![feature(never_type)]
#![feature(specialization)]

extern crate stable_vec;
extern crate num_traits;
extern crate tuple_utils;


pub mod handle;
pub mod impls;
pub mod map;
pub mod io;
pub mod shape;
pub mod shape2;


use handle::{DefaultIndex, FaceHandle, VertexHandle};


pub trait TriMesh {
    type VertexProp;
    type FaceProp;

    // type VertexIter: Iterator<Item = VertexHandle<Self::Idx>>;

    // TODO: use once GATs are available
    // type FaceIter: Iterator<Item = FaceHandle<Self::Idx>>;

    fn empty() -> Self where Self: Sized;

    fn num_faces(&self) -> DefaultIndex;
    fn num_vertices(&self) -> DefaultIndex;

    fn add_vertex(&mut self, prop: Self::VertexProp) -> VertexHandle;
    fn add_face(&mut self, vertices: [VertexHandle; 3], prop: Self::FaceProp) -> FaceHandle;

    // fn vertices(&self) -> Self::VertexIter;
    // fn faces(&self) -> Self::FaceIter;
    // TODO: change once GATs are available
    fn vertices<'a>(&'a self) -> Box<Iterator<Item = VertexHandle> + 'a>;
    fn faces<'a>(&'a self) -> Box<Iterator<Item = FaceHandle> + 'a>;

    fn vertices_of_face(&self, face: FaceHandle) -> [VertexHandle; 3];
}


/// Types that can be interpreted to represent some kind of 2D position.
///
/// This type is implemented for strongly typed "position"-types, like
/// `cgmath::Point2`, as well as for generic "weaker" types such as tuples
/// `(T, T)` and arrays `[T; 2]`. You should use strong types to represent
/// points in space instead of simple tuples to avoid logic errors.
pub trait Pos2D {
    type Scalar;

    fn x(&self) -> &Self::Scalar;
    fn y(&self) -> &Self::Scalar;
}

impl<T> Pos2D for (T, T) {
    type Scalar = T;

    fn x(&self) -> &Self::Scalar {
        &self.0
    }

    fn y(&self) -> &Self::Scalar {
        &self.1
    }
}

impl<T> Pos2D for [T; 2] {
    type Scalar = T;

    fn x(&self) -> &Self::Scalar {
        &self[0]
    }

    fn y(&self) -> &Self::Scalar {
        &self[1]
    }
}

/// Types that can be interpreted to represent some kind of 3D position.
pub trait Pos3D {
    type Scalar;

    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self;

    fn x(&self) -> &Self::Scalar;
    fn y(&self) -> &Self::Scalar;
    fn z(&self) -> &Self::Scalar;
}

impl<T> Pos3D for (T, T, T) {
    type Scalar = T;

    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        (x, y, z)
    }

    fn x(&self) -> &Self::Scalar {
        &self.0
    }

    fn y(&self) -> &Self::Scalar {
        &self.1
    }

    fn z(&self) -> &Self::Scalar {
        &self.2
    }
}

impl<T> Pos3D for [T; 3] {
    type Scalar = T;

    fn from_coords(x: Self::Scalar, y: Self::Scalar, z: Self::Scalar) -> Self {
        [x, y, z]
    }

    fn x(&self) -> &Self::Scalar {
        &self[0]
    }

    fn y(&self) -> &Self::Scalar {
        &self[1]
    }

    fn z(&self) -> &Self::Scalar {
        &self[2]
    }
}
