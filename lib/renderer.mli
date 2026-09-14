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

(** [create ?camera ()] sets up GPU state. Requires a current GL context
    (3.3+ for the default shader). Blending is enabled. *)
val create : ?camera:camera -> unit -> t

(** [get_camera t] is [t]'s current camera. *)
val get_camera : t -> camera

(** [set_camera t cam] replaces [t]'s camera; it takes effect on the next
    [flush]. *)
val set_camera : t -> camera -> unit

(** [set_viewport t ~width ~height] sets the GL viewport to
    [(0, 0, width, height)]. *)
val set_viewport : t -> width:int -> height:int -> unit

(** The renderer's own mesh; [queue] appends to it, [flush] draws it. *)
val default_mesh : t -> Mesh.t

(** [queue t prim] adds a primitive to the default mesh. *)
val queue : t -> Geom.primitive -> unit

(** [clear_screen t color] fills the color buffer. *)
val clear_screen : t -> Geom.color -> unit

(** [flush ?mesh t] uploads [mesh] if dirty, then draws it (GL_TRIANGLES).
    Non-destructive; clear meshes explicitly with [Mesh.clear].
    Flush order is paint order (blending is on). *)
val flush : ?mesh:Mesh.t -> t -> unit
