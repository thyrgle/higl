(* Core geometry and color types.

   All primitives carry per-vertex colors: colors interpolate across the
    primitive (GL 2.x style smooth shading). *)

module V2 = Gg.V2

(** A position in 3D space. *)
type point = { x : float; y : float; z : float }

(** An RGBA color; each channel is in \[0.0, 1.0\]. *)
type color = { r : float; g : float; b : float; a : float }

(** A vertex: position plus its own color. *)
type vertex = { pos : point; color : color }

(** A triangle with per-vertex colors (interpolated when rendered). *)
type triangle = { v1 : vertex; v2 : vertex; v3 : vertex }

(** A line segment of thickness [width] (world units). *)
type line = { a : vertex; b : vertex; width : float }

(** A square of side [size] (world units) centered at [p]. *)
type point_prim = { p : vertex; size : float }

(** An axis-aligned rectangle. [x], [y] is the bottom-left corner in world
    space (y up); [w], [h] is the size. *)
type rect = { x : float; y : float; w : float; h : float }

(** A textured quad: [tex]'s [src] region (pixel coordinates, top-left
    origin) drawn into [dst] (world space), rotated [rotation] radians
    counter-clockwise about [pivot] (world space), tinted with [tint].
    Sprites draw at z = 0. *)
type sprite = {
  tex : Texture.t;
  dst : rect;
  src : rect;
  rotation : float;
  pivot : V2.t;
  tint : color;
}

(** A drawable primitive. *)
type primitive =
  | Triangle of triangle
  | Line of line
  | Point of point_prim
  | Sprite of sprite

(** Default width of a [line]. *)
let default_width = 1.0

(** Default size of a [point_prim]. *)
let default_size = 1.0

(** Opaque white. *)
let white = { r = 1.0; g = 1.0; b = 1.0; a = 1.0 }

(** [point x y z] is the point [(x, y, z)]. *)
let point x y z = { x; y; z }

(** [color r g b a] is the color with the given channel values. *)
let color r g b a = { r; g; b; a }

(** [vertex pos color] is a vertex at [pos] with [color]. *)
let vertex pos color = { pos; color }

(** [tri ?color p1 p2 p3] is a solid-color triangle. *)
let tri ?(color = white) p1 p2 p3 : triangle =
  { v1 = { pos = p1; color }
  ; v2 = { pos = p2; color }
  ; v3 = { pos = p3; color } }

(** [line ?width a b] is a line segment between vertices [a] and [b]. *)
let line ?(width = default_width) a b = { a; b; width }

(** [point_prim ?size p] is a square point at vertex [p]. *)
let point_prim ?(size = default_size) p = { p; size }

(** [tri_solid color t] overrides all vertex colors of [t]. *)
let tri_solid color t =
  { v1 = { t.v1 with color }
  ; v2 = { t.v2 with color }
  ; v3 = { t.v3 with color } }

(** [rect x y w h] is the rectangle with bottom-left corner [(x, y)] and
    size [(w, h)]. *)
let rect x y w h = { x; y; w; h }

(** [sprite ?src ?rotation ?pivot ?tint tex dst] is a textured quad of
    [tex] filling [dst]. [src] (pixel coordinates, top-left origin)
    defaults to the whole texture; [rotation] (CCW, radians about
    [pivot]) defaults to [0.0]; [pivot] defaults to the center of [dst];
    [tint] defaults to [white]. *)
let sprite ?src ?(rotation = 0.0) ?pivot ?(tint = white) tex dst =
  let src =
    match src with
    | Some s -> s
    | None ->
        rect 0.0 0.0 (float_of_int (Texture.width tex))
          (float_of_int (Texture.height tex))
  in
  let pivot =
    match pivot with
    | Some p -> p
    | None -> V2.v (dst.x +. (dst.w /. 2.0)) (dst.y +. (dst.h /. 2.0))
  in
  { tex; dst; src; rotation; pivot; tint }
