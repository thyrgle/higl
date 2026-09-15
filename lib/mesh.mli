(** A mesh is an ordered, CPU-side queue of primitives.

    Build one with the [add_*] functions, flush it with [Renderer.flush].
    Flushing is non-destructive: the mesh keeps its primitives (with a dirty
    flag to skip redundant work), clear it explicitly with [clear]. *)

type t

(** [create ?capacity ()] is a new empty mesh. [capacity] is a hint in
    primitives (default 1024); meshes grow as needed. *)
val create : ?capacity:int -> unit -> t

(** [prim_count t] is the number of primitives queued in [t]. *)
val prim_count : t -> int

(** [is_dirty t] is [true] when [t] changed since its vertex/index caches
    were last rebuilt. *)
val is_dirty : t -> bool

(** [add_prim t p] appends primitive [p] to [t]. *)
val add_prim : t -> Geom.primitive -> unit

(** [add_primitives t ps] appends all of [ps], in order. *)
val add_primitives : t -> Geom.primitive list -> unit

(** [add_triangle t tri] appends triangle [tri] to [t]. *)
val add_triangle : t -> Geom.triangle -> unit

(** [add_line t l] appends line [l] to [t]. *)
val add_line : t -> Geom.line -> unit

(** [add_point t p] appends point [p] to [t]. *)
val add_point : t -> Geom.point_prim -> unit

(** [add_sprite t s] appends textured quad [s] to [t]. Sprites batch
    separately from untextured primitives: the renderer issues one draw
    call per contiguous run of same-texture primitives. *)
val add_sprite : t -> Geom.sprite -> unit

(** Retained shape helpers: each appends the decomposition of a shape
    (see [Shape]) to [t]. The shape is baked into the mesh as plain
    triangles/lines; static shapes are re-uploaded only when the mesh
    changes. *)

val add_rect : t -> ?color:Geom.color -> Geom.rect -> unit

val add_rect_outline :
  t -> ?color:Geom.color -> ?thickness:float -> Geom.rect -> unit

val add_circle :
  t -> ?color:Geom.color -> ?segments:int -> Gg.V2.t -> float -> unit

val add_circle_outline :
  t -> ?color:Geom.color -> ?thickness:float -> ?segments:int ->
  Gg.V2.t -> float -> unit

val add_ellipse :
  t -> ?color:Geom.color -> ?segments:int -> ?rotation:float ->
  center:Gg.V2.t -> rx:float -> ry:float -> unit -> unit

val add_ellipse_outline :
  t -> ?color:Geom.color -> ?thickness:float -> ?segments:int ->
  ?rotation:float -> center:Gg.V2.t -> rx:float -> ry:float -> unit -> unit

val add_polygon :
  t -> ?color:Geom.color -> sides:int -> ?rotation:float ->
  Gg.V2.t -> float -> unit

val add_polygon_outline :
  t -> ?color:Geom.color -> ?thickness:float -> sides:int -> ?rotation:float ->
  Gg.V2.t -> float -> unit

val add_polyline :
  t -> ?color:Geom.color -> ?width:float -> Gg.V2.t list -> unit

(** [clear t] empties the mesh. Does not free its internal buffers. *)
val clear : t -> unit

(** A contiguous stretch of primitives sharing one texture ([None] =
    untextured). [first]/[count] are index positions. Exposed for the
    renderer; treat as opaque. *)
type tex_run = { tex : Texture.t option; first : int; count : int }

(** Internal: used by [Renderer], not part of the stable API. *)
module Internal : sig
  (** Vertex layout: pos3 color4 uv2, interleaved, float32. *)
  val floats_per_vertex : int
  val stride_bytes : int

  (** [expand t] (re)expands the primitive list into the vertex/index
      caches if (and only if) [t] is dirty. Returns true when it
      re-expanded. *)
  val expand : t -> bool

  (** [runs t] is the texture runs of the last expansion, in insertion
      order. *)
  val runs : t -> tex_run array

  val vertices : t -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  val vertex_floats : t -> int
  val indices : t -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
  val index_count : t -> int
end
