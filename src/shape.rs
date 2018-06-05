use std::{
    marker::{PhantomData},
    ops::{Index, IndexMut},
};

use num_traits::{AsPrimitive, Float, FloatConst, Zero};

use crate::{
    TriMesh,
    Pos3Like,
    handle::{Handle, VertexHandle},
    map::{PropMap, PropMapMut, VertexMapMut},
};



pub fn disk<R, PosT>(steps: u64) -> R
where
    R: GenResult,
    R::PosMap: Index<VertexHandle, Output = PosT>,
    PosT: Pos3Like + Sized,
    PosT::Scalar: Float + FloatConst + AsPrimitive<u64> + 'static,
    u64: AsPrimitive<PosT::Scalar>,
{
    let ring_pos = |step: u64| {
        let around_circle = (2 * step).as_() * (PosT::Scalar::PI() / steps.as_());
        let x = around_circle.sin();
        let y = around_circle.cos();
        let z = PosT::Scalar::zero();

        [x, y, z]
    };

    let add_vertex = |out: &mut R, [x, y, z]: [PosT::Scalar; 3]| {
        let v = out.mesh().add_vertex(());
        out.positions().map(|m| {
            let pos = PosT::from_coords(x, y, z);
            m.insert(v, pos);
        });

        v
    };

    let mut out = R::empty();

    let center = add_vertex(&mut out, [PosT::Scalar::zero(); 3]);
    let top = add_vertex(&mut out, ring_pos(0));
    let mut last = top;

    for step in 1..=steps {
        let curr = if step == steps {
            top
        } else {
            add_vertex(&mut out, ring_pos(step))
        };

        out.mesh().add_face([center, last, curr], ());
        last = curr;
    }

    out
}

// enum ResultTypes {
//     Position,
//     // VertexNormal,
//     // FaceNormal,
// }

pub struct GenPosition<M>(pub M);

pub trait GenResult {
    type Mesh: TriMesh<VertexProp = (), FaceProp = ()>;
    type PosMap: VertexMapMut;

    fn empty() -> Self;

    fn mesh(&mut self) -> &mut Self::Mesh;
    fn positions(&mut self) -> Option<&mut Self::PosMap>;
}

impl<T> GenResult for T
where
    T: TriMesh<VertexProp = (), FaceProp = ()>,
{
    type Mesh = Self;
    type PosMap = NullMap<(f32, f32, f32)>;

    fn empty() -> Self {
        <Self as TriMesh>::empty()
    }

    fn mesh(&mut self) -> &mut Self::Mesh {
        self
    }
    fn positions(&mut self) -> Option<&mut Self::PosMap> {
        None
    }
}

impl<MeshT, MapT> GenResult for (MeshT, GenPosition<MapT>)
where
    MeshT: TriMesh<VertexProp = (), FaceProp = ()>,
    MapT: VertexMapMut,
{
    type Mesh = MeshT;
    type PosMap = MapT;

    fn empty() -> Self {
        (MeshT::empty(), GenPosition(MapT::empty()))
    }

    fn mesh(&mut self) -> &mut Self::Mesh {
        &mut self.0
    }
    fn positions(&mut self) -> Option<&mut Self::PosMap> {
        let (_, GenPosition(map)) = self;
        Some(map)
    }
}

















pub struct NullMap<T = !>(PhantomData<T>);


impl<T, H: Handle> PropMap<H> for NullMap<T> {
    fn get(&self, _: H) -> Option<&Self::Output> {
        panic!("called method on a NullMap");
    }
}

impl<T, H: Handle> PropMapMut<H> for NullMap<T> {
    fn get_mut(&mut self, _: H) -> Option<&mut Self::Output> {
        panic!("called method on a NullMap");
    }

    fn empty() -> Self where Self: Sized {
        panic!("called method on a NullMap");
    }

    fn insert(&mut self, _: H, _: Self::Output) -> Option<Self::Output>
        where Self::Output: Sized
    {
        panic!("called method on a NullMap");
    }
}


impl<T, H: Handle> Index<H> for NullMap<T> {
    type Output = T;
    fn index(&self, _: H) -> &Self::Output {
        panic!("called method on a NullMap");
    }
}

impl<T, H: Handle> IndexMut<H> for NullMap<T> {
    fn index_mut(&mut self, _: H) -> &mut Self::Output {
        panic!("called method on a NullMap");
    }
}
