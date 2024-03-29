
/// Derive macro for [the `MemSink` trait][io::MemSink].
///
/// You can easily derive `MemSink` for your own types. To do that, you have to
/// attach `#[derive(MemSink)]` to your struct definition (note: currently, the
/// trait can only be derived for structs with named fields). You also have to
/// annotate your fields with `#[lox(...)]` attributes to tell the derive macro
/// what a field should be used for. Example:
///
/// ```
/// use lox::{
///     MemSink, VertexHandle,
///     cgmath::Point3,
///     core::HalfEdgeMesh,
///     map::DenseMap,
/// };
///
///
/// #[derive(MemSink)]
/// struct MyMesh {
///     #[lox(core_mesh)]
///     mesh: HalfEdgeMesh,
///
///     #[lox(vertex_position)]
///     positions: DenseMap<VertexHandle, Point3<f32>>,
/// }
/// ```
///
/// There is one required field: the core mesh field. That field's type has to
/// implement several mesh traits, in particular `MeshMut` and `TriMeshMut`.
/// You have to annotate that mesh with `#[lox(core_mesh)]`.
///
/// Additionally, you can have fields for each mesh property, like vertex
/// position or face colors. The type of those fields has to implement
/// `PropStoreMut` with a compatible element type. You have to annotate these
/// property fields with the corresponding attribute. The available properties
/// are:
///
/// - `vertex_position`
/// - `vertex_normal`
/// - `vertex_color`
/// - `face_normal`
/// - `face_color`
///
/// Furthermore, there are some configurations (like the cast mode) that can be
/// configured via `lox(...)` attributes as well. See below for more
/// information.
///
///
/// ## Cast modes
///
/// You can set a *cast mode* for each field. A `MemSink` has to be able to
/// "handle" any primitive type as the source is allowed to call the property
/// methods with any type. The sink can handle types either by casting or by
/// returning an error. The field's cast mode determines which casts are
/// allowed and which are not. Possible cast modes:
///
/// - `cast = "none"`
/// - `cast = "lossless"`
/// - `cast = "rounding"`
/// - `cast = "clamping"`
/// - `cast = "lossy"` (*default*)
///
/// The `none` mode does not allow casting at all. If the type provided by the
/// source does not match the type in your struct, an error is returned. All
/// other modes correspond to the cast modes in the [`cast`
/// module][crate::cast].
///
/// Note that the cast modes are used by `derive(MemSource)` as well.
///
/// You can specify the cast mode either per field or globally on the whole
/// struct. The mode of the struct applies to all fields that don't have a
/// field-specific mode.
///
/// ```
/// # use lox::{
/// #     MemSink, VertexHandle,
/// #     cgmath::{Point3, Vector3},
/// #     core::HalfEdgeMesh,
/// #     map::DenseMap,
/// # };
/// #
/// #[derive(MemSink)]
/// #[lox(cast = "none")]
/// struct MyMesh {
///     #[lox(core_mesh)]
///     mesh: HalfEdgeMesh,
///
///     #[lox(vertex_position)]
///     positions: DenseMap<VertexHandle, Point3<f32>>,
///
///     #[lox(vertex_normal, cast = "lossy")]
///     normals: DenseMap<VertexHandle, Vector3<f32>>,
/// }
/// ```
///
/// In this example, the vertex positions inherit the "struct global" cast mode
/// (`none`), while the vertex normals override that mode to `lossy`.
///
///
/// ### Exact traits required for each field
///
/// Traits required for the `core_mesh` field:
/// - TODO
///
/// Traits required for property fields. For type `T` of the field:
/// - `T` must implement [`PropStoreMut`][crate::map::PropStoreMut] (with
///   fitting handle type). Additionally:
///     - For `vertex_position`: `T::Target` must implement
///       [`Pos3Like`][crate::prop::Pos3Like].
///     - For `*_normal`: `T::Target` must implement
///       [`Vec3Like`][crate::prop::Vec3Like].
///     - For `*_color`: `T::Target` must implement
///       [`ColorLike`][crate::prop::ColorLike] and `T::Target::Channel` must
///       implement [`Primitive`][io::Primitive].
#[cfg(feature= "io")]
pub use lox_macros::MemSink;

/// Derive macro for [the `MemSource` trait][io::MemSource].
///
/// You can easily derive `MemSource` for your own types. To do that, you have
/// to attach `#[derive(MemSource)]` to your struct definition (note:
/// currently, the trait can only be derived for structs with named fields).
/// You also have to annotate your fields with `#[lox(...)]` attributes to tell
/// the derive macro what a field should be used for. Example:
///
/// ```
/// use lox::{
///     MemSource, VertexHandle,
///     cgmath::Point3,
///     core::SharedVertexMesh,
///     map::DenseMap,
/// };
///
///
/// #[derive(MemSource)]
/// struct MyMesh {
///     #[lox(core_mesh)]
///     mesh: SharedVertexMesh,
///
///     #[lox(vertex_position)]
///     positions: DenseMap<VertexHandle, Point3<f32>>,
/// }
/// ```
///
/// Deriving this trait works very similar to deriving [`MemSink`]. See its
/// documentation for more information on the custom derive.
///
///
/// ### Exact traits required for each field
///
/// Traits required for the `core_mesh` field:
/// - TODO
///
/// Traits required for property fields. For type `T` of the field:
/// - `T` must implement [`PropStore`][crate::map::PropStore] (with fitting
///   handle type). Additionally:
///     - For `vertex_position`: `T::Target` must implement
///       [`Pos3Like`][crate::prop::Pos3Like] and `T::Target::Scalar` must
///       implement [`Primitive`][io::Primitive].
///     - For `*_normal`: `T::Target` must implement
///       [`Vec3Like`][crate::prop::Vec3Like] and `T::Target::Scalar` must
///       implement [`Primitive`][io::Primitive].
///     - For `*_color`: `T::Target` must implement
///       [`ColorLike`][crate::prop::ColorLike] and `T::Target::Channel` must
///       implement [`Primitive`][io::Primitive].
#[cfg(feature= "io")]
pub use lox_macros::MemSource;
