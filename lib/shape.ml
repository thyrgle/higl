(* Shape: constructors for common 2D shapes.

   Pure data: every function returns primitives built from [Geom.tri]
   and [Geom.line] — nothing touches GL and nothing is drawn here. Add
   the results to a [Mesh.t] (or use the [Mesh.add_*] convenience
   wrappers) and [Renderer.flush] draws them; shapes baked into a
   static mesh are re-uploaded only when the mesh changes. *)

module V2 = Gg.V2
module M2 = Gg.M2

(* Adaptive segment count for arcs, raylib-style: bigger radius, more
   segments, clamped to a sane range. *)
let default_segments radius =
  Int.max 12 (Int.min 256 (Float.to_int (Float.ceil (radius *. 1.5))))

let pt_v2 p = Geom.point (V2.x p) (V2.y p) 0.0

let vertex_v2 color p = { Geom.pos = pt_v2 p; color }

(* Filled arc: a fan of triangles [center, rim i, rim i+1]. [pt i] is
   the rim point at segment boundary [i]. *)
let fan ~center ~segments ~pt =
  let c = { Geom.pos = pt_v2 center; color = Geom.white } in
  List.init segments (fun i ->
      Geom.Triangle
        { Geom.v1 = c
        ; v2 = vertex_v2 Geom.white (pt i)
        ; v3 = vertex_v2 Geom.white (pt (i + 1)) })

(* Closed outline: line segments [rim i, rim i+1]. *)
let rim_lines ~segments ~width ~pt =
  List.init segments (fun i ->
      Geom.Line
        (Geom.line ~width (vertex_v2 Geom.white (pt i))
           (vertex_v2 Geom.white (pt (i + 1)))))

(* Paint every vertex/endpoint of [prims] with [color]. *)
let recolor color prims =
  List.map
    (fun p ->
      match p with
      | Geom.Triangle t -> Geom.Triangle (Geom.tri_solid color t)
      | Geom.Line l ->
          Geom.Line { l with Geom.a = { l.a with color }; b = { l.b with color } }
      | _ -> p)
    prims

(** [ellipse ?color ?segments ?rotation ~center ~rx ~ry ()] is a filled
    ellipse (circles are [rx = ry = radius]); [segments] is the number
    of rim triangles (default: adaptive, 12..256 based on the larger
    radius); the rim starts at +X and runs counter-clockwise, rotated
    by [rotation] radians (default 0). *)
let ellipse ?(color = Geom.white) ?segments ?(rotation = 0.0) ~center ~rx ~ry
    () =
  if rx < 0.0 || ry < 0.0 then invalid_arg "Shape.ellipse: negative radius";
  let segments =
    match segments with
    | Some s -> s
    | None -> default_segments (Float.max rx ry)
  in
  if segments < 3 then invalid_arg "Shape.ellipse: segments < 3";
  let step = (2.0 *. Float.pi) /. float_of_int segments in
  let pt i =
    let theta = rotation +. (float_of_int i *. step) in
    V2.add center (V2.ltr (M2.rot2 theta) (V2.v rx 0.0))
  in
  recolor color (fan ~center ~segments ~pt)

(** [circle ?color ?segments center radius] is a filled circle (see
    [ellipse]). *)
let circle ?color ?segments center radius =
  ellipse ?color ?segments ~center ~rx:radius ~ry:radius ()

(** [ellipse_outline ?color ?thickness ?segments ?rotation ~center ~rx ~ry ()]
    is an ellipse outline stroked with lines of [thickness] world units.
    Adjacent segments overlap slightly at the joins (visible with
    translucent colors). *)
let ellipse_outline ?(color = Geom.white) ?(thickness = Geom.default_width)
    ?segments ?(rotation = 0.0) ~center ~rx ~ry () =
  if rx < 0.0 || ry < 0.0 then invalid_arg "Shape.ellipse_outline: negative radius";
  let segments =
    match segments with
    | Some s -> s
    | None -> default_segments (Float.max rx ry)
  in
  if segments < 3 then invalid_arg "Shape.ellipse_outline: segments < 3";
  let step = (2.0 *. Float.pi) /. float_of_int segments in
  let pt i =
    let theta = rotation +. (float_of_int i *. step) in
    V2.add center (V2.ltr (M2.rot2 theta) (V2.v rx 0.0))
  in
  recolor color (rim_lines ~segments ~width:thickness ~pt)

(** [circle_outline ?color ?thickness ?segments center radius] is a
    circle outline (see [ellipse_outline]). *)
let circle_outline ?color ?thickness ?segments center radius =
  ellipse_outline ?color ?thickness ?segments ~center ~rx:radius ~ry:radius ()

(** [polygon ?color ~sides ?rotation center radius] is a filled regular
    [sides]-gon inscribed in the circle of [radius] around [center]; the
    first vertex is at +X, running counter-clockwise, pre-rotated by
    [rotation] radians. *)
let polygon ?(color = Geom.white) ~sides ?(rotation = 0.0) center radius =
  if sides < 3 then invalid_arg "Shape.polygon: sides < 3";
  if radius < 0.0 then invalid_arg "Shape.polygon: negative radius";
  let step = (2.0 *. Float.pi) /. float_of_int sides in
  let pt i =
    let theta = rotation +. (float_of_int i *. step) in
    V2.add center (V2.ltr (M2.rot2 theta) (V2.v radius 0.0))
  in
  recolor color (fan ~center ~segments:sides ~pt)

(** [polygon_outline ?color ?thickness ~sides ?rotation center radius] is
    a polygon outline (see [polygon]). *)
let polygon_outline ?(color = Geom.white) ?(thickness = Geom.default_width)
    ~sides ?(rotation = 0.0) center radius =
  if sides < 3 then invalid_arg "Shape.polygon_outline: sides < 3";
  if radius < 0.0 then invalid_arg "Shape.polygon_outline: negative radius";
  let step = (2.0 *. Float.pi) /. float_of_int sides in
  let pt i =
    let theta = rotation +. (float_of_int i *. step) in
    V2.add center (V2.ltr (M2.rot2 theta) (V2.v radius 0.0))
  in
  recolor color (rim_lines ~segments:sides ~width:thickness ~pt)

(** [rect ?color r] is [r] filled with two triangles. *)
let rect ?(color = Geom.white) (r : Geom.rect) =
  let p x y = Geom.point x y 0.0 in
  let bl = p r.Geom.x r.Geom.y
  and br = p (r.Geom.x +. r.Geom.w) r.Geom.y
  and tr = p (r.Geom.x +. r.Geom.w) (r.Geom.y +. r.Geom.h)
  and tl = p r.Geom.x (r.Geom.y +. r.Geom.h) in
  [ Geom.Triangle (Geom.tri ~color bl br tr);
    Geom.Triangle (Geom.tri ~color bl tr tl) ]

(** [rect_outline ?color ?thickness r] is [r]'s border stroked with four
    lines of [thickness] world units (square joins). *)
let rect_outline ?(color = Geom.white) ?(thickness = Geom.default_width)
    (r : Geom.rect) =
  let p x y = Geom.point x y 0.0 in
  let v x y = { Geom.pos = p x y; color } in
  let bl = v r.Geom.x r.Geom.y
  and br = v (r.Geom.x +. r.Geom.w) r.Geom.y
  and tr = v (r.Geom.x +. r.Geom.w) (r.Geom.y +. r.Geom.h)
  and tl = v r.Geom.x (r.Geom.y +. r.Geom.h) in
  [ Geom.Line (Geom.line ~width:thickness bl br);
    Geom.Line (Geom.line ~width:thickness br tr);
    Geom.Line (Geom.line ~width:thickness tr tl);
    Geom.Line (Geom.line ~width:thickness tl bl) ]

(** [polyline ?color ?width pts] is the path [p0 - p1 - ... - pn] stroked
    with lines of [width] world units (square joins, no caps). Fewer
    than two points yields no primitives. *)
let polyline ?(color = Geom.white) ?(width = Geom.default_width) pts =
  match pts with
  | [] | [ _ ] -> []
  | p0 :: rest ->
      let v p = vertex_v2 color p in
      let seg a b = Geom.Line (Geom.line ~width (v a) (v b)) in
      let rec go prev = function
        | [] -> []
        | p :: tl -> seg prev p :: go p tl
      in
      go p0 rest
