(* Camera2d: a 2D camera with pan, zoom and rotation.

   The world point [target] appears at the screen point [offset] (both
   in units; the screen origin is the bottom-left of the viewport, so
   with the default 1 unit = 1 pixel mapping, offsets are pixels).
   [zoom] > 1 magnifies, [rotation] rotates what you see counter-
   clockwise (in radians). Pure math: the renderer composes [matrix]
   with the viewport and uploads it as the camera uniform. *)

module V2 = Gg.V2
module M2 = Gg.M2

(** The camera state. *)
type t = {
  target : V2.t;    (* world point that lands on [offset] *)
  offset : V2.t;    (* screen point where [target] appears *)
  rotation : float; (* radians, CCW *)
  zoom : float;     (* screen units per world unit *)
}

(** [create ?target ?offset ?rotation ?zoom ()] is the camera with the
    given fields; defaults give an identity camera (no pan, no
    rotation, zoom 1). *)
let create ?(target = V2.zero) ?(offset = V2.zero) ?(rotation = 0.0)
    ?(zoom = 1.0) () =
  if Float.abs zoom < 1e-6 then invalid_arg "Camera2d.create: zoom must be nonzero";
  { target; offset; rotation; zoom }

(** [world_to_screen c p] is where world point [p] lands on screen, in
    viewport coordinates measured from the bottom-left. *)
let world_to_screen c p =
  V2.add c.offset (V2.ltr (M2.rot2 c.rotation) (V2.smul c.zoom (V2.sub p c.target)))

(** [screen_to_world c s] is the world point shown at screen point [s]
    (viewport coordinates, bottom-left origin). *)
let screen_to_world c s =
  let inv = V2.smul (1.0 /. c.zoom) (V2.ltr (M2.rot2 (-.c.rotation)) (V2.sub s c.offset)) in
  V2.add c.target inv

(** [matrix c ~width ~height] is the full projection for a viewport of
    [width] x [height] units: the ortho projection over the viewport
    composed with the camera's pan/zoom/rotation. *)
let matrix c ~width ~height =
  let proj =
    Mat4.ortho ~left:0.0 ~right:width ~bottom:0.0 ~top:height ~near:(-1.0)
      ~far:1.0
  in
  let view =
    Mat4.mul
      (Mat4.translate (V2.x c.offset) (V2.y c.offset) 0.0)
      (Mat4.mul (Mat4.rotate_z c.rotation)
         (Mat4.mul (Mat4.scale c.zoom c.zoom 1.0)
            (Mat4.translate (-.V2.x c.target) (-.V2.y c.target) 0.0)))
  in
  Mat4.mul proj view
