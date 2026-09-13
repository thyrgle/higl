(* Headless tests: mesh expansion and mat4 ortho. No GL calls. *)

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

let close a b = Float.abs (a -. b) < 1e-5

let vertex x y z r g b a =
  Higl.vertex (Higl.point x y z) (Higl.color r g b a)

(* --- Mesh: counts and dirty flag --- *)

let test_counts () =
  let m = Higl.Mesh.create () in
  check "empty mesh" (Higl.Mesh.prim_count m = 0);
  check "empty mesh expands to nothing"
    (Higl.Mesh.Internal.expand m && Higl.Mesh.Internal.vertex_floats m = 0
     && Higl.Mesh.Internal.index_count m = 0);
  Higl.Mesh.add_triangle m
    { Higl.v1 = vertex 0. 0. 0. 1. 0. 0. 1.;
      v2 = vertex 1. 0. 0. 0. 1. 0. 1.;
      v3 = vertex 0. 1. 0. 0. 0. 1. 1. };
  check "tri prim count" (Higl.Mesh.prim_count m = 1);
  check "tri dirty" (Higl.Mesh.is_dirty m);
  check "tri expands" (Higl.Mesh.Internal.expand m);
  check "tri clean after expand" (not (Higl.Mesh.is_dirty m));
  check "tri no re-expand when clean" (not (Higl.Mesh.Internal.expand m));
  check "tri vertex floats (27)" (Higl.Mesh.Internal.vertex_floats m = 27);
  check "tri index count (3)" (Higl.Mesh.Internal.index_count m = 3);
  Higl.Mesh.add_line m (Higl.line (vertex 0. 0. 0. 1. 1. 1. 1.) (vertex 2. 0. 0. 1. 0. 0. 1.));
  check "line dirty" (Higl.Mesh.is_dirty m);
  ignore (Higl.Mesh.Internal.expand m);
  check "line vertex floats (27+36)" (Higl.Mesh.Internal.vertex_floats m = 63);
  check "line index count (3+6)" (Higl.Mesh.Internal.index_count m = 9);
  Higl.Mesh.add_point m (Higl.point_prim (vertex 1. 1. 0. 1. 1. 0. 1.));
  ignore (Higl.Mesh.Internal.expand m);
  check "point vertex floats (27+36+36)" (Higl.Mesh.Internal.vertex_floats m = 99);
  check "point index count (3+6+6)" (Higl.Mesh.Internal.index_count m = 15);
  Higl.Mesh.clear m;
  check "clear empties" (Higl.Mesh.prim_count m = 0 && Higl.Mesh.is_dirty m);
  ignore (Higl.Mesh.Internal.expand m);
  check "clear expands to nothing" (Higl.Mesh.Internal.vertex_floats m = 0)

(* --- Mesh: vertex/index contents --- *)

let test_contents () =
  let m = Higl.Mesh.create () in
  Higl.Mesh.add_triangle m
    { Higl.v1 = vertex 10. 20. 30. 1. 0. 0. 1.;
      v2 = vertex 40. 50. 60. 0. 1. 0. 1.;
      v3 = vertex 70. 80. 90. 0. 0. 1. 1. };
  ignore (Higl.Mesh.Internal.expand m);
  let ba = Higl.Mesh.Internal.vertices m in
  let f i = ba.{i} in
  check "v1 pos" (f 0 = 10. && f 1 = 20. && f 2 = 30.);
  check "v1 color" (f 3 = 1. && f 4 = 0. && f 5 = 0. && f 6 = 1.);
  check "v1 uv reserved" (f 7 = 0. && f 8 = 0.);
  check "v2 pos" (f 9 = 40. && f 10 = 50. && f 11 = 60.);
  check "v3 pos" (f 18 = 70. && f 19 = 80. && f 20 = 90.);
  check "v3 color" (f 21 = 0. && f 22 = 0. && f 23 = 1. && f 24 = 1.);
  let ib = Higl.Mesh.Internal.indices m in
  check "tri indices 0,1,2"
    (ib.{0} = 0l && ib.{1} = 1l && ib.{2} = 2l);

  (* Line (1,1)-(3,2) width 2: half-thickness 1, normal (-dy,dx)/len * hw =
     (-1,2)/sqrt(5) -> quad corners. Check first corner (a - n). *)
  let m = Higl.Mesh.create () in
  Higl.Mesh.add_line m
    (Higl.line ~width:2.0
       (vertex 1. 1. 0. 1. 1. 1. 1.)
       (vertex 3. 2. 0. 0. 0. 0. 1.));
  ignore (Higl.Mesh.Internal.expand m);
  let ba = Higl.Mesh.Internal.vertices m in
  let dx = 2.0 and dy = 1.0 in
  let len = sqrt ((dx *. dx) +. (dy *. dy)) in
  let nx = (-.dy /. len) *. 1.0 and ny = (dx /. len) *. 1.0 in
  check "line corner0 x" (close ba.{0} (1.0 -. nx));
  check "line corner0 y" (close ba.{1} (1.0 -. ny));
  check "line corner0 color" (ba.{3} = 1. && ba.{6} = 1.);
  check "line corner2 x" (close ba.{18} (3.0 +. nx));
  check "line corner2 y" (close ba.{19} (2.0 +. ny));
  check "line corner2 color" (ba.{21} = 0. && ba.{24} = 1.);
  let ib = Higl.Mesh.Internal.indices m in
  check "line quad indices"
    (ib.{0} = 0l && ib.{1} = 1l && ib.{2} = 2l
     && ib.{3} = 0l && ib.{4} = 2l && ib.{5} = 3l);

  (* Degenerate line contributes nothing. *)
  let m = Higl.Mesh.create () in
  Higl.Mesh.add_line m
    (Higl.line (vertex 5. 5. 0. 1. 1. 1. 1.) (vertex 5. 5. 0. 1. 1. 1. 1.));
  ignore (Higl.Mesh.Internal.expand m);
  check "degenerate line empty"
    (Higl.Mesh.Internal.vertex_floats m = 0 && Higl.Mesh.Internal.index_count m = 0);

  (* Point quad centered at (1,2), size 4. *)
  let m = Higl.Mesh.create () in
  Higl.Mesh.add_point m (Higl.point_prim ~size:4.0 (vertex 1. 2. 0. 0.5 0.5 0.5 1.));
  ignore (Higl.Mesh.Internal.expand m);
  let ba = Higl.Mesh.Internal.vertices m in
  check "point corners"
    (close ba.{0} (-1.) && close ba.{1} 4. && close ba.{9} 3.
     && close ba.{18} 3. && close ba.{19} 0. && close ba.{27} (-1.)
     && close ba.{28} 0.);
  let ib = Higl.Mesh.Internal.indices m in
  check "point quad indices"
    (ib.{0} = 0l && ib.{5} = 3l)

(* --- Mesh: growth --- *)

let test_growth () =
  let m = Higl.Mesh.create ~capacity:1 () in
  for i = 0 to 99 do
    Higl.Mesh.add_triangle m
      (Higl.tri (Higl.point (float_of_int i) 0. 0.)
         (Higl.point (float_of_int i +. 1.) 0. 0.)
         (Higl.point (float_of_int i +. 0.5) 1. 0.))
  done;
  ignore (Higl.Mesh.Internal.expand m);
  check "grew to 100 tris" (Higl.Mesh.Internal.index_count m = 300)

(* --- Mat4 ortho --- *)

let apply m x y =
  (m.{0} *. x) +. m.{12}, (m.{5} *. y) +. m.{13}

let test_ortho () =
  let m =
    Higl.Mat4.ortho ~left:0. ~right:100. ~bottom:0. ~top:50. ~near:(-1.) ~far:1.
  in
  let x0, y0 = apply m 0. 0. in
  let x1, y1 = apply m 100. 50. in
  let xc, yc = apply m 50. 25. in
  check "ortho maps (0,0) to (-1,-1)" (close x0 (-1.) && close y0 (-1.));
  check "ortho maps (w,h) to (1,1)" (close x1 1. && close y1 1.);
  check "ortho maps center to (0,0)" (close xc 0. && close yc 0.);
  check "ortho identity-ish fields"
    (m.{15} = 1. && m.{1} = 0. && m.{4} = 0.);
  let mz0 = (m.{10} *. 0.) +. m.{14} in
  check "ortho maps z=0 (mid depth) to 0" (close mz0 0.);
  let mz1 = (m.{10} *. 1.) +. m.{14} in
  check "ortho maps z=+far (1) to -1" (close mz1 (-1.))

let () =
  test_counts ();
  test_contents ();
  test_growth ();
  test_ortho ();
  if !failures > 0 then (Printf.printf "%d failures\n" !failures; exit 1);
  print_endline "all tests passed"
