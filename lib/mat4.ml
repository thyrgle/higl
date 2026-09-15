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
