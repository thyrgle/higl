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

(* --- Geom: rect and sprite constructors --- *)

let test_geom_sprite () =
  let tex = { Higl.Texture.id = 1; width = 4; height = 2 } in
  let s = Higl.sprite tex (Higl.rect 10.0 20.0 8.0 4.0) in
  check "sprite default src (whole texture)"
    (s.src = { Higl.x = 0.0; y = 0.0; w = 4.0; h = 2.0 });
  check "sprite default pivot (dst center)"
    (close (Gg.V2.x s.pivot) 14.0 && close (Gg.V2.y s.pivot) 22.0);
  check "sprite default tint" (s.tint = Higl.white);
  check "sprite default rotation" (s.rotation = 0.0);
  let s2 =
    Higl.sprite ~src:(Higl.rect 1.0 0.0 2.0 1.0) ~tint:(Higl.color 1.0 0.0 0.0 1.0)
      tex (Higl.rect 0.0 0.0 1.0 1.0)
  in
  check "sprite explicit src"
    (s2.src = { Higl.x = 1.0; y = 0.0; w = 2.0; h = 1.0 });
  check "sprite explicit tint" (s2.tint = Higl.color 1.0 0.0 0.0 1.0)

(* --- Mesh: sprite expansion --- *)

let tex_a = { Higl.Texture.id = 7; width = 4; height = 2 }
let tex_b = { Higl.Texture.id = 9; width = 2; height = 2 }

let test_mesh_sprite () =
  let m = Higl.Mesh.create () in
  let tint = Higl.color 0.5 0.25 1.0 0.5 in
  Higl.Mesh.add_sprite m
    { (Higl.sprite tex_a (Higl.rect 0.0 0.0 2.0 1.0)) with tint };
  check "sprite prim count" (Higl.Mesh.prim_count m = 1);
  ignore (Higl.Mesh.Internal.expand m);
  check "sprite floats (36)" (Higl.Mesh.Internal.vertex_floats m = 36);
  check "sprite indices (6)" (Higl.Mesh.Internal.index_count m = 6);
  let ba = Higl.Mesh.Internal.vertices m in
  (* corners tl, tr, br, bl of dst (0,0)-(2,1) *)
  check "sprite tl" (close ba.{0} 0.0 && close ba.{1} 1.0);
  check "sprite tr" (close ba.{9} 2.0 && close ba.{10} 1.0);
  check "sprite br" (close ba.{18} 2.0 && close ba.{19} 0.0);
  check "sprite bl" (close ba.{27} 0.0 && close ba.{28} 0.0);
  (* uv: tw=4, th=2 -> ul=0, ur=1, vt=1, vb=0 *)
  check "sprite tl uv" (close ba.{7} 0.0 && close ba.{8} 1.0);
  check "sprite tr uv" (close ba.{16} 1.0 && close ba.{17} 1.0);
  check "sprite br uv" (close ba.{25} 1.0 && close ba.{26} 0.0);
  check "sprite bl uv" (close ba.{34} 0.0 && close ba.{35} 0.0);
  check "sprite tint applied" (close ba.{3} 0.5 && close ba.{4} 0.25);
  (* sub-rect src (1,0)-(3,1): u 0.25..0.75 *)
  let m = Higl.Mesh.create () in
  Higl.Mesh.add_sprite m
    (Higl.sprite ~src:(Higl.rect 1.0 0.0 2.0 1.0) tex_a (Higl.rect 0.0 0.0 1.0 1.0));
  ignore (Higl.Mesh.Internal.expand m);
  let ba = Higl.Mesh.Internal.vertices m in
  check "sprite sub-rect u" (close ba.{7} 0.25 && close ba.{16} 0.75);
  check "sprite sub-rect v" (close ba.{8} 1.0 && close ba.{26} 0.5)

let test_sprite_rotation () =
  let m = Higl.Mesh.create () in
  (* 90 degrees CCW about the center of (0,0)-(2,2) *)
  Higl.Mesh.add_sprite m
    (Higl.sprite ~rotation:(Float.pi /. 2.0) tex_a (Higl.rect 0.0 0.0 2.0 2.0));
  ignore (Higl.Mesh.Internal.expand m);
  let ba = Higl.Mesh.Internal.vertices m in
  check "rot tl -> (0,0)" (close ba.{0} 0.0 && close ba.{1} 0.0);
  check "rot tr -> (0,2)" (close ba.{9} 0.0 && close ba.{10} 2.0);
  check "rot br -> (2,2)" (close ba.{18} 2.0 && close ba.{19} 2.0);
  check "rot bl -> (2,0)" (close ba.{27} 2.0 && close ba.{28} 0.0)

(* --- Mesh: texture runs --- *)

let test_runs () =
  let m = Higl.Mesh.create () in
  Higl.Mesh.add_triangle m
    (Higl.tri (Higl.point 0. 0. 0.) (Higl.point 1. 0. 0.) (Higl.point 0. 1. 0.));
  Higl.Mesh.add_sprite m (Higl.sprite tex_a (Higl.rect 0.0 0.0 1.0 1.0));
  Higl.Mesh.add_sprite m (Higl.sprite tex_a (Higl.rect 1.0 0.0 1.0 1.0));
  Higl.Mesh.add_sprite m (Higl.sprite tex_b (Higl.rect 2.0 0.0 1.0 1.0));
  ignore (Higl.Mesh.Internal.expand m);
  let runs = Higl.Mesh.Internal.runs m in
  check "run count (white, a, b)" (Array.length runs = 3);
  let r0 = runs.(0) and r1 = runs.(1) and r2 = runs.(2) in
  check "run0 untextured first 0 count 3"
    (r0.tex = None && r0.first = 0 && r0.count = 3);
  check "run1 tex a first 3 count 12"
    ((match r1.tex with Some t -> t.id = 7 | None -> false)
     && r1.first = 3 && r1.count = 12);
  check "run2 tex b first 15 count 6"
    ((match r2.tex with Some t -> t.id = 9 | None -> false)
     && r2.first = 15 && r2.count = 6);
  let m = Higl.Mesh.create () in
  ignore (Higl.Mesh.Internal.expand m);
  check "empty mesh no runs" (Array.length (Higl.Mesh.Internal.runs m) = 0)

(* --- Mat4: composition --- *)

let test_mat4_ops () =
  let open Higl.Mat4 in
  let r = rotate_z (Float.pi /. 2.0) in
  check "rotate_z x-hat to y-hat" (close r.{0} 0.0 && close r.{1} 1.0);
  let tr = translate 2.0 3.0 0.0 in
  let m = mul tr r in
  (* apply r first, then tr: (1,0) -> (0,1) -> (2,4) *)
  let x = (m.{0} *. 1.0) +. (m.{4} *. 0.0) +. m.{12} in
  let y = (m.{1} *. 1.0) +. (m.{5} *. 0.0) +. m.{13} in
  check "mul applies right first" (close x 2.0 && close y 4.0);
  let s = scale 2.0 3.0 1.0 in
  check "scale diag" (s.{0} = 2.0 && s.{5} = 3.0 && s.{10} = 1.0)

(* --- Camera2d --- *)

let vclose a b =
  Float.abs (Gg.V2.x a -. Gg.V2.x b) < 1e-5
  && Float.abs (Gg.V2.y a -. Gg.V2.y b) < 1e-5

let test_camera2d () =
  let raises f = try f (); false with Invalid_argument _ -> true in
  let open Higl.Camera2d in
  let id = create () in
  check "cam identity to_screen"
    (vclose (world_to_screen id (Gg.V2.v 3.0 4.0)) (Gg.V2.v 3.0 4.0));
  check "cam identity to_world"
    (vclose (screen_to_world id (Gg.V2.v 3.0 4.0)) (Gg.V2.v 3.0 4.0));
  let c = create ~zoom:2.0 () in
  check "cam zoom to_screen"
    (vclose (world_to_screen c (Gg.V2.v 1.0 2.0)) (Gg.V2.v 2.0 4.0));
  check "cam zoom roundtrip"
    (vclose (screen_to_world c (world_to_screen c (Gg.V2.v (-3.0) 5.0)))
       (Gg.V2.v (-3.0) 5.0));
  let r = create ~rotation:(Float.pi /. 2.0) () in
  check "cam rot90 (1,0) -> (0,1)"
    (vclose (world_to_screen r (Gg.V2.v 1.0 0.0)) (Gg.V2.v 0.0 1.0));
  let pan =
    create ~target:(Gg.V2.v 10.0 10.0) ~offset:(Gg.V2.v 320.0 240.0) ~zoom:2.0 ()
  in
  check "cam pan target lands on offset"
    (vclose (world_to_screen pan (Gg.V2.v 10.0 10.0)) (Gg.V2.v 320.0 240.0));
  check "cam pan roundtrip"
    (vclose (screen_to_world pan (Gg.V2.v 320.0 240.0)) (Gg.V2.v 10.0 10.0));
  check "cam zero zoom raises"
    (raises (fun () -> ignore (create ~zoom:0.0 ())));
  (* matrix: identity cam over a 100x50 viewport maps (0,0) to (-1,-1)
     and (100,50) to (1,1). *)
  let m = matrix id ~width:100.0 ~height:50.0 in
  let x0 = m.{12} and y0 = m.{13} in
  check "cam matrix origin to ndc" (close x0 (-1.0) && close y0 (-1.0));
  let x1 = (m.{0} *. 100.0) +. m.{12} and y1 = (m.{5} *. 50.0) +. m.{13} in
  check "cam matrix corner to ndc" (close x1 1.0 && close y1 1.0);
  (* zoom 2: world (50,25) lands at screen (100,50) = ndc (1,1) *)
  let mz = matrix c ~width:100.0 ~height:50.0 in
  let x = (mz.{0} *. 50.0) +. mz.{12} and y = (mz.{5} *. 25.0) +. mz.{13} in
  check "cam matrix zoom" (close x 1.0 && close y 1.0)

(* --- Shape --- *)

let test_shape () =
  let raises f = try f (); false with Invalid_argument _ -> true in
  let red = Higl.color 1.0 0.0 0.0 1.0 in
  let prims = Higl.Shape.rect ~color:red (Higl.rect 0.0 0.0 2.0 2.0) in
  check "rect = 2 triangles" (List.length prims = 2);
  let all_red =
    List.for_all
      (function
        | Higl.Triangle t ->
            t.v1.color = red && t.v2.color = red && t.v3.color = red
        | _ -> false)
      prims
  in
  check "rect colored" all_red;
  check "rect_outline = 4 lines"
    (List.length (Higl.Shape.rect_outline (Higl.rect 0.0 0.0 2.0 2.0)) = 4);
  let circ = Higl.Shape.circle ~segments:6 (Gg.V2.v 0.0 0.0) 1.0 in
  check "circle 6 segments = 6 tris" (List.length circ = 6);
  let circ_def = Higl.Shape.circle (Gg.V2.v 0.0 0.0) 10.0 in
  check "circle adaptive segments"
    (List.length circ_def = Float.to_int (Float.ceil (10.0 *. 1.5)));
  check "circle colored"
    (List.for_all
       (function Higl.Triangle t -> t.v2.color = red | _ -> false)
       (Higl.Shape.circle ~color:red ~segments:5 (Gg.V2.v 0.0 0.0) 1.0));
  check "polyline 4 pts = 3 lines"
    (List.length
       (Higl.Shape.polyline
          [ Gg.V2.v 0.0 0.0; Gg.V2.v 1.0 0.0; Gg.V2.v 1.0 1.0; Gg.V2.v 2.0 1.0 ])
     = 3);
  check "polyline <2 pts empty" (Higl.Shape.polyline [ Gg.V2.v 0.0 0.0 ] = []);
  check "pentagon = 5 tris"
    (List.length (Higl.Shape.polygon ~sides:5 (Gg.V2.v 0.0 0.0) 1.0) = 5);
  check "polygon sides<3 raises"
    (raises (fun () -> ignore (Higl.Shape.polygon ~sides:2 (Gg.V2.v 0.0 0.0) 1.0)));
  check "ellipse negative radius raises"
    (raises
       (fun () ->
         ignore (Higl.Shape.ellipse ~center:(Gg.V2.v 0.0 0.0) ~rx:(-1.0) ~ry:1.0 ())));
  check "circle segments<3 raises"
    (raises
       (fun () ->
         ignore (Higl.Shape.circle ~segments:2 (Gg.V2.v 0.0 0.0) 1.0)));
  (* mesh adders append the decomposition *)
  let m = Higl.Mesh.create () in
  Higl.Mesh.add_circle m ~color:red (Gg.V2.v 5.0 5.0) 3.0;
  check "mesh add_circle bakes 12 tris" (Higl.Mesh.prim_count m = 12);
  Higl.Mesh.add_polyline m [ Gg.V2.v 0.0 0.0; Gg.V2.v 1.0 1.0 ];
  check "mesh add_polyline appends" (Higl.Mesh.prim_count m = 13)

let () =
  test_counts ();
  test_contents ();
  test_growth ();
  test_ortho ();
  test_geom_sprite ();
  test_mesh_sprite ();
  test_sprite_rotation ();
  test_runs ();
  test_mat4_ops ();
  test_camera2d ();
  test_shape ();
  test_glsl_source ();
  test_glsl_offsets ();
  test_glsl_cse ();
  test_glsl_errors ();
  if !failures > 0 then (Printf.printf "%d failures\n" !failures; exit 1);
  print_endline "all tests passed"
