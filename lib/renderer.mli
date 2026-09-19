(* Renderer: owns GPU state; draws CPU-side [Mesh.t] values.

   The user creates the window and GL context (higl only assumes GL calls
   work). The default path: [create], queue primitives, [flush] every frame. *)

type t

(** An orthographic camera: the axis-aligned box
    [{left,right} x {bottom,top} x {near,far}] of world space mapped into
    the viewport. *)
type camera = {
  left : float;
  right : float;
  bottom : float;
  top : float;
  near : float;
  far : float;
}

(** [default_camera ~width ~height] is an ortho camera mapping (0,0) to the
    bottom-left and (width,height) to the top-right, 1 unit = 1 pixel. *)
val default_camera : width:float -> height:float -> camera

(** [create ?camera ?depth ()] sets up GPU state. Requires a current GL
    context (3.3+ for the default shader). Blending is enabled. With
    [depth = true] the depth test is enabled too ([GL_LESS]-or-equal),
    for 3D content; the caller clears the depth buffer per pass with
    [Tgl4.Gl.clear]. *)
val create : ?camera:camera -> ?depth:bool -> unit -> t

(** [get_camera t] is [t]'s current camera. *)
val get_camera : t -> camera

(** [set_camera t cam] replaces [t]'s camera; it takes effect on the next
    [flush]. *)
val set_camera : t -> camera -> unit

(** [get_camera2d t] is [t]'s current 2D camera (see [Camera2d]). *)
val get_camera2d : t -> Camera2d.t

(** [set_camera2d t cam] replaces [t]'s 2D camera; it takes effect on the
    next [flush]. Whichever of [set_camera]/[set_camera2d]/
    [set_camera_matrix] ran last wins. The matrix is computed from the
    current viewport, so [set_viewport] also re-arms it. *)
val set_camera2d : t -> Camera2d.t -> unit

(** [set_camera_matrix t m] uses [m] — a full projection * view matrix,
    e.g. [Mat4.perspective] composed with [Mat4.look_at] — as the camera
    for the next [flush]. The escape hatch for 3D rendering: geometry
    keeps its world-space positions, [m] does the projection. *)
val set_camera_matrix : t -> Mat4.t -> unit

(** [set_viewport t ~width ~height] sets the GL viewport to
    [(0, 0, width, height)]. *)
val set_viewport : t -> width:int -> height:int -> unit

(** The renderer's own mesh; [queue] appends to it, [flush] draws it. *)
val default_mesh : t -> Mesh.t

(** [queue t prim] adds a primitive to the default mesh. *)
val queue : t -> Geom.primitive -> unit

(** [queue_texture ?src ?rotation ?pivot ?tint t tex dst] queues a sprite
    (textured quad) on the default mesh — the retained counterpart of
    raylib's DrawTexturePro. Nothing is drawn until [flush]; for static
    geometry use [Mesh.add_sprite] on your own mesh instead. [src] is
    pixel coordinates (top-left origin, defaults to the whole texture),
    [rotation] is CCW radians about [pivot] (default: [dst]'s center),
    [tint] multiplies the texture color. *)
val queue_texture :
  ?src:Geom.rect -> ?rotation:float -> ?pivot:Gg.V2.t -> ?tint:Geom.color ->
  t -> Texture.t -> Geom.rect -> unit

(** [clear_screen t color] fills the color buffer. *)
val clear_screen : t -> Geom.color -> unit

(** [flush ?mesh t] uploads [mesh] if dirty, then draws it
    (GL_TRIANGLES). Non-destructive; clear meshes explicitly with
    [Mesh.clear]. Flush order is paint order (blending is on). Sprites
    and untextured primitives interleave freely: each contiguous run of
    same-texture primitives becomes one draw call. *)
val flush : ?mesh:Mesh.t -> t -> unit
