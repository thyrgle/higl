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

(* --- Glsl: emitted fragment source --- *)

(* First index of [sub] in [s] at or after [from]. *)
let index_sub s sub from =
  let n = String.length s and m = String.length sub in
  if m = 0 then Some (max 0 from)
  else
    let rec go i =
      if i + m > n then None
      else if String.sub s i m = sub then Some i
      else go (i + 1)
    in
    go (max 0 from)

let count_sub s sub =
  let n = ref 0 in
  let pos = ref 0 in
  (try
     while true do
       match index_sub s sub !pos with
       | Some i ->
           pos := i + String.length sub;
           incr n
       | None -> raise Not_found
     done
   with Not_found -> ());
  !n

let src ?(r = 1) ?(st = 1) ?(sh = `Square) ?(c = false) () =
  let open Higl.Glsl in
  let shape = match sh with `Square -> Square | `Cross -> Cross in
  fragment_source
    (program (fun s -> avg (nbhd ~radius:r ~stride:st ~shape ~center:c s)))

let test_glsl_source () =
  let open Higl.Glsl in
  let tex_count s = count_sub s "texture(u_prev" in
  let starts_with p s = String.length s >= String.length p
      && String.sub s 0 (String.length p) = p in
  let contains a b = count_sub a b > 0 in
  check "glsl: version header"
    (starts_with "#version 330 core" (src ()));
  check "glsl: uniform contract"
    (contains (src ()) "uniform sampler2D u_prev;"
     && contains (src ()) "uniform vec2 u_resolution;"
     && contains (src ()) "uniform vec2 u_mouse;"
     && contains (src ()) "uniform float u_time;"
     && contains (src ()) "out vec4 frag_color;");
  check "glsl: px helper declared when sampling"
    (contains (src ()) "vec2 px = 1.0 / u_resolution;");
  check "glsl: avg (nbhd self) fetches 8 texels"
    (tex_count (src ()) = 8);
  check "glsl: avg scales by 1/8" (contains (src ()) "0.125");
  check "glsl: stride 2 doubles offsets"
    (contains (src ~st:2 ()) "vec2(-2.0, -2.0)"
     && contains (src ~st:2 ()) "vec2(2.0, 2.0)"
     && not (contains (src ~st:2 ()) "vec2(-1.0"));
  check "glsl: cross shape fetches 4 texels"
    (tex_count (src ~sh:`Cross ()) = 4);
  check "glsl: center adds the pixel itself"
    (tex_count (src ~c:true ()) = 9);
  check "glsl: radius 2 fetches 24 texels"
    (tex_count (src ~r:2 ()) = 24);
  check "glsl: no px when not sampling"
    (not (contains
            (fragment_source (program (fun _ -> v4 (f 0.) (f 0.) (f 0.) (f 1.))))
            "px"));
  check "glsl: peek offset"
    (contains (fragment_source (program (fun _ -> peek ~dx:3 ~dy:0)))
        "texture(u_prev, v_uv + vec2(3.0, 0.0) * px)");
  check "glsl: literals"
    (contains (fragment_source (program (fun _ -> v4 (f 2.) (f 0.) (f 0.) (f 1.))))
        "vec4(2.0, 0.0, 0.0, 1.0)");
  check "glsl: swizzle"
    (contains
       (fragment_source (program (fun s -> v4 (x s) (y s) (f 0.) (f 1.))))
       "(T0).x");
  check "glsl: if_ / comparison"
    (contains
       (fragment_source
          (program (fun s -> if_ (x s >. f 0.5) s (v4 (f 0.) (f 0.) (f 0.) (f 1.)))))
       "?");
  check "glsl: min_of folds min"
    (contains (fragment_source (program (fun s -> min_of (nbhd s)))) "min(min(");
  check "glsl: count_nonzero uses a per-neighbor predicate"
    (contains (fragment_source (program (fun s ->
         scale (count_nonzero (nbhd s)) s)))
        "0.001");
  check "glsl: weighted gaussian3 has its weights"
    (contains (fragment_source (program (fun s ->
         weighted gaussian3 (include_center (nbhd s))))) "0.0625"
     && contains (fragment_source (program (fun s ->
         weighted gaussian3 (include_center (nbhd s))))) "0.25")

let test_glsl_offsets () =
  let open Higl.Glsl in
  (* offsets are documented to come out sorted by dy then dx. *)
  let sort l = List.sort (fun (ax, ay) (bx, by) -> compare (ay, ax) (by, bx)) l in
  let mem off l = List.mem off l in
  let default = offsets (nbhd self) in
  check "offsets: 8 Moore neighbors" (List.length default = 8);
  check "offsets: no center by default" (not (mem (0, 0) default));
  check "offsets: corners present"
    (mem (-1, -1) default && mem (1, -1) default && mem (-1, 1) default
     && mem (1, 1) default);
  check "offsets: sorted"
    (default = sort default);
  check "offsets: cross is von Neumann"
    (offsets (cross (nbhd self))
     = sort [ (-1, 0); (0, -1); (1, 0); (0, 1) ]);
  check "offsets: stride doubles"
    (offsets (stride 2 (nbhd self)) = List.map (fun (x, y) -> (2 * x, 2 * y)) default);
  check "offsets: center included on request"
    (mem (0, 0) (offsets (include_center (nbhd self))));
  check "offsets: radius 2 square has 24"
    (List.length (offsets (radius 2 (nbhd self))) = 24)

let test_glsl_cse () =
  let open Higl.Glsl in
  let s =
    fragment_source
      (program (fun self ->
         let blur = avg (nbhd self) in
         mix (mix self blur (f 0.25)) blur (f 0.25)))
  in
  check "cse: repeated subterm hoisted" (count_sub s "vec4 T0 = " = 1);
  check "cse: hoisted local referenced twice" (count_sub s "T0" = 3);
  let s2 = fragment_source (program (fun self -> self +. self)) in
  check "cse: self sampled once"
    (count_sub s2 "texture(u_prev, v_uv)" = 1 (* the hoisted decl *)
     && count_sub s2 "texture(u_prev, v_uv + " = 0)

let test_glsl_errors () =
  let open Higl.Glsl in
  let raises f = try f (); false with Invalid_argument _ -> true in
  check "errors: nbhd radius 0" (raises (fun () -> ignore (nbhd ~radius:0 self)));
  check "errors: stride 0" (raises (fun () -> ignore (stride 0 (nbhd self))));
  check "errors: kernel must be square"
    (raises (fun () -> ignore (kernel [|[|1.|]; [|2.; 3.|]|])));
  check "errors: kernel must be odd-sized"
    (raises (fun () -> ignore (kernel [| [| 1. |]; [| 2. |] |])));
  check "errors: weighted size mismatch"
    (raises (fun () -> ignore (weighted gaussian3 (radius 2 (nbhd self)))))

let () =
  test_counts ();
  test_contents ();
  test_growth ();
  test_ortho ();
  test_glsl_source ();
  test_glsl_offsets ();
  test_glsl_cse ();
  test_glsl_errors ();
  if !failures > 0 then (Printf.printf "%d failures\n" !failures; exit 1);
  print_endline "all tests passed"
