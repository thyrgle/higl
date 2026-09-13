(* Core geometry and color types.

   All primitives carry per-vertex colors: colors interpolate across the
   primitive (GL 2.x style smooth shading). *)

type point = { x : float; y : float; z : float }

type color = { r : float; g : float; b : float; a : float }

(** A vertex: position plus its own color. *)
type vertex = { pos : point; color : color }

type triangle = { v1 : vertex; v2 : vertex; v3 : vertex }

(** A line segment of thickness [width] (world units). *)
type line = { a : vertex; b : vertex; width : float }

(** A square of side [size] (world units) centered at [p]. *)
type point_prim = { p : vertex; size : float }

type primitive =
  | Triangle of triangle
  | Line of line
  | Point of point_prim

let default_width = 1.0
let default_size = 1.0

let white = { r = 1.0; g = 1.0; b = 1.0; a = 1.0 }

let point x y z = { x; y; z }
let color r g b a = { r; g; b; a }
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
