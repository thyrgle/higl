(* Minimal column-major 4x4 matrices, laid out for glUniformMatrix4fv
   with transpose = false. *)

type t = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

(** [create ()] is a new zero matrix. *)
let create () =
  let m = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 16 in
  Bigarray.Array1.fill m 0.0;
  m

(** [identity ()] is the identity matrix. *)
let identity () =
  let m = create () in
  m.{0} <- 1.0;
  m.{5} <- 1.0;
  m.{10} <- 1.0;
  m.{15} <- 1.0;
  m

(** Orthographic projection mapping the axis-aligned box into NDC. *)
let ortho ~left ~right ~bottom ~top ~near ~far =
  let m = create () in
  m.{0} <- 2.0 /. (right -. left);
  m.{5} <- 2.0 /. (top -. bottom);
  m.{10} <- -2.0 /. (far -. near);
  m.{12} <- -.( (right +. left) /. (right -. left) );
  m.{13} <- -.( (top +. bottom) /. (top -. bottom) );
  m.{14} <- -.( (far +. near) /. (far -. near) );
  m.{15} <- 1.0;
  m

(** [mul a b] is the matrix product [a * b]: transforming a point by
    [a * b] applies [b] first, then [a]. *)
let mul a b =
  let m = create () in
  for j = 0 to 3 do
    for i = 0 to 3 do
      let s = ref 0.0 in
      for k = 0 to 3 do
        s := !s +. (a.{(k * 4) + i} *. b.{(j * 4) + k})
      done;
      m.{(j * 4) + i} <- !s
    done
  done;
  m

(** [translate x y z] is the translation matrix. *)
let translate x y z =
  let m = identity () in
  m.{12} <- x;
  m.{13} <- y;
  m.{14} <- z;
  m

(** [scale x y z] is the scale matrix. *)
let scale x y z =
  let m = identity () in
  m.{0} <- x;
  m.{5} <- y;
  m.{10} <- z;
  m

(** [rotate_z rad] is the counter-clockwise rotation about the Z axis by
    [rad] radians. *)
let rotate_z rad =
  let m = identity () in
  let c = cos rad and s = sin rad in
  m.{0} <- c;
  m.{1} <- s;
  m.{4} <- -.s;
  m.{5} <- c;
  m

(** [perspective ~fovy ~aspect ~near ~far] is the standard OpenGL
    perspective projection (right-handed, clip z in [-1, 1]) for a
    vertical field of view of [fovy] radians, viewport aspect [aspect]
    (width / height) and the given near/far planes. *)
let perspective ~fovy ~aspect ~near ~far =
  if near <= 0.0 || far <= near then invalid_arg "Mat4.perspective: bad planes";
  if aspect <= 0.0 then invalid_arg "Mat4.perspective: bad aspect";
  let f = 1.0 /. tan (fovy /. 2.0) in
  let m = create () in
  m.{0} <- f /. aspect;
  m.{5} <- f;
  m.{10} <- (far +. near) /. (near -. far);
  m.{11} <- -1.0;
  m.{14} <- (2.0 *. far *. near) /. (near -. far);
  m

module V3 = Gg.V3

(** [look_at ~eye ~center ~up] is the right-handed view matrix placing
    the camera at [eye], looking at [center], with [up] roughly the
    screen's +Y direction. *)
let look_at ~eye ~center ~up =
  let f = V3.unit (V3.sub center eye) in
  let s = V3.unit (V3.cross f up) in
  let u = V3.cross s f in
  let m = identity () in
  (* column-major: column 0 holds row 0 of the transposed basis *)
  m.{0} <- V3.x s;
  m.{1} <- V3.x u;
  m.{2} <- -.V3.x f;
  m.{4} <- V3.y s;
  m.{5} <- V3.y u;
  m.{6} <- -.V3.y f;
  m.{8} <- V3.z s;
  m.{9} <- V3.z u;
  m.{10} <- -.V3.z f;
  m.{12} <- -.V3.dot s eye;
  m.{13} <- -.V3.dot u eye;
  m.{14} <- V3.dot f eye;
  m
