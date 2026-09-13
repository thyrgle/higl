(** A mesh is an ordered, CPU-side queue of primitives.

    Build one with the [add_*] functions, flush it with [Renderer.flush].
    Flushing is non-destructive: the mesh keeps its primitives (with a dirty
    flag to skip redundant work), clear it explicitly with [clear]. *)

type t

(** [create ?capacity ()] is a new empty mesh. [capacity] is a hint in
    primitives (default 1024); meshes grow as needed. *)
val create : ?capacity:int -> unit -> t

val prim_count : t -> int
val is_dirty : t -> bool

val add_prim : t -> Geom.primitive -> unit
val add_primitives : t -> Geom.primitive list -> unit
val add_triangle : t -> Geom.triangle -> unit
val add_line : t -> Geom.line -> unit
val add_point : t -> Geom.point_prim -> unit

(** [clear t] empties the mesh. Does not free its internal buffers. *)
val clear : t -> unit

(** Internal: used by [Renderer], not part of the stable API. *)
module Internal : sig
  (** Vertex layout: pos3 color4 uv2, interleaved, float32. *)
  val floats_per_vertex : int
  val stride_bytes : int

  (** [expand t] (re)expands the primitive list into the vertex/index caches
      if (and only if) [t] is dirty. Returns true when it re-expanded. *)
  val expand : t -> bool

  val vertices : t -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
  val vertex_floats : t -> int
  val indices : t -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
  val index_count : t -> int
end
