#![feature(crate_in_paths)]
#![feature(non_modrs_mods)]


extern crate stable_vec;


pub mod handle;
pub mod impls;
pub mod map;
pub mod io;


use handle::{HandleIndex, FaceHandle, VertexHandle};


pub trait TriMesh {
    type Idx: HandleIndex;
    // type VertexIter: Iterator<Item = VertexHandle<Self::Idx>>;

    // TODO: use once GATs are available
    // type FaceIter: Iterator<Item = FaceHandle<Self::Idx>>;

    fn num_faces(&self) -> Self::Idx;
    fn num_vertices(&self) -> Self::Idx;

    // fn vertices(&self) -> Self::VertexIter;
    // fn faces(&self) -> Self::FaceIter;
    // TODO: change once GATs are available
    fn vertices<'a>(&'a self) -> Box<Iterator<Item = VertexHandle<Self::Idx>> + 'a>;
    fn faces<'a>(&'a self) -> Box<Iterator<Item = FaceHandle<Self::Idx>> + 'a>;

    fn vertices_of_face(&self, face: FaceHandle<Self::Idx>)
        -> [VertexHandle<Self::Idx>; 3];
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

    fn x(&self) -> &Self::Scalar;
    fn y(&self) -> &Self::Scalar;
    fn z(&self) -> &Self::Scalar;
}

impl<T> Pos3D for (T, T, T) {
    type Scalar = T;

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
