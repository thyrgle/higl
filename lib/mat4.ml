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
