(* Mesh: ordered CPU-side queue of primitives.

   Storage is at primitive granularity; expansion into vertex/index buffers
   happens lazily (Renderer.flush), guarded by a dirty flag. All expansion
   lives here in one place so it can be moved to queue-time later without
   touching the public API.

   Expansion rules (all drawn as GL_TRIANGLES):
   - Triangle -> 3 vertices, 3 indices
   - Line     -> thin quad, 4 vertices, 6 indices; thickness expands in the
                  XY plane
   - Point    -> square quad, 4 vertices, 6 indices, centered at p
   - Sprite   -> textured quad, 4 vertices, 6 indices; [src] maps to
                  [dst] (uv computed at expand time), rotated about the
                  pivot

   Vertex layout: pos3 color4 uv2 (uv zero except on sprites).

   Primitives are also grouped into contiguous *runs* by texture so the
   renderer can batch draw calls: one run per stretch of primitives
   that share a texture (untextured primitives share the renderer's
   1x1 white texture).    Runs follow insertion order, preserving paint
   order under blending. *)

module V2 = Gg.V2

(* A contiguous stretch of primitives sharing one texture. [first] and
   [count] are index positions; [tex] is [None] for untextured runs. *)
type tex_run = { tex : Texture.t option; first : int; count : int }

type t = {
  mutable rev : Geom.primitive list;  (* insertion order, reversed *)
  mutable nprims : int;
  mutable dirty : bool;
  mutable verts : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  mutable nverts : int;  (* floats used in [verts] *)
  mutable indices : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  mutable nindices : int;
  mutable runs : tex_run array;  (* texture runs, insertion order *)
}

let floats_per_vertex = 9  (* pos3 color4 uv2 *)
let stride_bytes = floats_per_vertex * 4

let tri_v = 3 and tri_i = 3
let quad_v = 4 and quad_i = 6

let create ?(capacity = 1024) () =
  let maxv = capacity * quad_v * floats_per_vertex in
  { rev = []
  ; nprims = 0
  ; dirty = true
  ; verts =
      Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (max 64 maxv)
  ; nverts = 0
  ; indices =
      Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout
        (max 64 (capacity * quad_i))
  ; nindices = 0
  ; runs = [||] }

let prim_count t = t.nprims
let is_dirty t = t.dirty

let add_prim t prim =
  t.rev <- prim :: t.rev;
  t.nprims <- t.nprims + 1;
  t.dirty <- true

let add_primitives t prims = List.iter (add_prim t) prims
let add_triangle t tr = add_prim t (Geom.Triangle tr)
let add_line t l = add_prim t (Geom.Line l)
let add_point t p = add_prim t (Geom.Point p)
let add_sprite t s = add_prim t (Geom.Sprite s)

(* Retained wrappers around the [Shape] constructors: append the
   shape's primitives to [t]. *)

let add_rect t ?color r =
  List.iter (add_prim t) (Shape.rect ?color r)

let add_rect_outline t ?color ?thickness r =
  List.iter (add_prim t) (Shape.rect_outline ?color ?thickness r)

let add_circle t ?color ?segments center radius =
  List.iter (add_prim t) (Shape.circle ?color ?segments center radius)

let add_circle_outline t ?color ?thickness ?segments center radius =
  List.iter (add_prim t) (Shape.circle_outline ?color ?thickness ?segments center radius)

let add_ellipse t ?color ?segments ?rotation ~center ~rx ~ry () =
  List.iter (add_prim t) (Shape.ellipse ?color ?segments ?rotation ~center ~rx ~ry ())

let add_ellipse_outline t ?color ?thickness ?segments ?rotation ~center ~rx ~ry () =
  List.iter (add_prim t)
    (Shape.ellipse_outline ?color ?thickness ?segments ?rotation ~center ~rx ~ry ())

let add_polygon t ?color ~sides ?rotation center radius =
  List.iter (add_prim t) (Shape.polygon ?color ~sides ?rotation center radius)

let add_polygon_outline t ?color ?thickness ~sides ?rotation center radius =
  List.iter (add_prim t)
    (Shape.polygon_outline ?color ?thickness ~sides ?rotation center radius)

let add_polyline t ?color ?width pts =
  List.iter (add_prim t) (Shape.polyline ?color ?width pts)

let clear t =
  t.rev <- [];
  t.nprims <- 0;
  t.nverts <- 0;
  t.nindices <- 0;
  t.runs <- [||];
  t.dirty <- true

(* Note: contents are not preserved when growing; expand rewrites the whole
   cache from the primitive list anyway. *)
let grow kind ba needed =
  let cap = Bigarray.Array1.dim ba in
  let rec next c = if c >= needed then c else next (2 * c) in
  if needed > cap then
    Bigarray.Array1.create kind Bigarray.c_layout (next (max 64 (2 * cap)))
  else ba

let ensure t ~verts_needed ~indices_needed =
  t.verts <-
    grow Bigarray.float32 t.verts verts_needed;
  t.indices <-
    grow Bigarray.int32 t.indices indices_needed

(* Write one vertex (9 floats) with explicit position, color and uv. *)
let write_vertex_uv ba i x y z (c : Geom.color) u v =
  ba.{i} <- x;
  ba.{i + 1} <- y;
  ba.{i + 2} <- z;
  ba.{i + 3} <- c.Geom.r;
  ba.{i + 4} <- c.Geom.g;
  ba.{i + 5} <- c.Geom.b;
  ba.{i + 6} <- c.Geom.a;
  ba.{i + 7} <- u;
  ba.{i + 8} <- v

(* Write one full vertex (9 floats) at float index [i]; uv zero. *)
let write_vertex ba i (v : Geom.vertex) =
  write_vertex_uv ba i v.Geom.pos.Geom.x v.Geom.pos.Geom.y v.Geom.pos.Geom.z
    v.Geom.color 0.0 0.0

(* Write raw position + color as a vertex at float index [i]; uv zero. *)
let write_xyz_rgba ba i x y z (c : Geom.color) =
  write_vertex_uv ba i x y z c 0.0 0.0

let write_quad_indices ba idx base =
  ba.{idx} <- Int32.of_int base;
  ba.{idx + 1} <- Int32.of_int (base + 1);
  ba.{idx + 2} <- Int32.of_int (base + 2);
  ba.{idx + 3} <- Int32.of_int base;
  ba.{idx + 4} <- Int32.of_int (base + 2);
  ba.{idx + 5} <- Int32.of_int (base + 3)

let prim_tex = function
  | Geom.Sprite s -> Some s.Geom.tex
  | Geom.Triangle _ | Geom.Line _ | Geom.Point _ -> None

let expand t =
  if not t.dirty then false
  else begin
    let prims = List.rev t.rev in
    let verts_needed = ref 0 and indices_needed = ref 0 and nruns = ref 0 in
    let prev = ref None and first = ref true in
    List.iter
      (fun p ->
        let v, i =
          match p with
          | Geom.Triangle _ -> (tri_v, tri_i)
          | Geom.Line _ | Geom.Point _ | Geom.Sprite _ -> (quad_v, quad_i)
        in
        verts_needed := !verts_needed + (v * floats_per_vertex);
        indices_needed := !indices_needed + i;
        let tex = prim_tex p in
        if !first || tex <> !prev then begin
          incr nruns;
          prev := tex;
          first := false
        end)
      prims;
    ensure t ~verts_needed:!verts_needed ~indices_needed:!indices_needed;
    let ba = t.verts and ib = t.indices in
    let runs = Array.make !nruns { tex = None; first = 0; count = 0 } in
    let ri = ref (-1) and open_ = ref false and run_first = ref 0 and run_tex = ref None in
    let close now =
      if !open_ then begin
        runs.(!ri) <-
          { tex = !run_tex; first = !run_first; count = now - !run_first };
        open_ := false
      end
    in
    t.nverts <- 0;
    t.nindices <- 0;
    List.iter
      (fun p ->
        let tex = prim_tex p in
        if (not !open_) || tex <> !run_tex then begin
          close t.nindices;
          incr ri;
          open_ := true;
          run_first := t.nindices;
          run_tex := tex
        end;
        match p with
        | Geom.Triangle tr ->
          let base = t.nverts / floats_per_vertex in
          write_vertex ba t.nverts tr.Geom.v1;
          write_vertex ba (t.nverts + floats_per_vertex) tr.Geom.v2;
          write_vertex ba (t.nverts + (2 * floats_per_vertex)) tr.Geom.v3;
          t.nverts <- t.nverts + (3 * floats_per_vertex);
          ib.{t.nindices} <- Int32.of_int base;
          ib.{t.nindices + 1} <- Int32.of_int (base + 1);
          ib.{t.nindices + 2} <- Int32.of_int (base + 2);
          t.nindices <- t.nindices + 3
        | Geom.Line l ->
          let a = l.Geom.a and b = l.Geom.b in
          let dx = b.Geom.pos.Geom.x -. a.Geom.pos.Geom.x in
          let dy = b.Geom.pos.Geom.y -. a.Geom.pos.Geom.y in
          let len = sqrt ((dx *. dx) +. (dy *. dy)) in
          (* zero-length lines expand to nothing *)
          if len >= 1e-6 then begin
            let hw = l.Geom.width /. 2.0 in
            let nx = (-.dy /. len) *. hw and ny = (dx /. len) *. hw in
            let base = t.nverts / floats_per_vertex in
            write_xyz_rgba ba t.nverts
              (a.Geom.pos.Geom.x -. nx) (a.Geom.pos.Geom.y -. ny) a.Geom.pos.Geom.z
              a.Geom.color;
            write_xyz_rgba ba (t.nverts + floats_per_vertex)
              (a.Geom.pos.Geom.x +. nx) (a.Geom.pos.Geom.y +. ny) a.Geom.pos.Geom.z
              a.Geom.color;
            write_xyz_rgba ba (t.nverts + (2 * floats_per_vertex))
              (b.Geom.pos.Geom.x +. nx) (b.Geom.pos.Geom.y +. ny) b.Geom.pos.Geom.z
              b.Geom.color;
            write_xyz_rgba ba (t.nverts + (3 * floats_per_vertex))
              (b.Geom.pos.Geom.x -. nx) (b.Geom.pos.Geom.y -. ny) b.Geom.pos.Geom.z
              b.Geom.color;
            t.nverts <- t.nverts + (4 * floats_per_vertex);
            write_quad_indices ib t.nindices base;
            t.nindices <- t.nindices + 6
          end
        | Geom.Point pp ->
          let p = pp.Geom.p in
          let h = pp.Geom.size /. 2.0 in
          let x = p.Geom.pos.Geom.x and y = p.Geom.pos.Geom.y and z = p.Geom.pos.Geom.z in
          let base = t.nverts / floats_per_vertex in
          write_xyz_rgba ba t.nverts (x -. h) (y +. h) z p.Geom.color;
          write_xyz_rgba ba (t.nverts + floats_per_vertex) (x +. h) (y +. h) z p.Geom.color;
          write_xyz_rgba ba (t.nverts + (2 * floats_per_vertex)) (x +. h) (y -. h) z p.Geom.color;
          write_xyz_rgba ba (t.nverts + (3 * floats_per_vertex)) (x -. h) (y -. h) z p.Geom.color;
          t.nverts <- t.nverts + (4 * floats_per_vertex);
          write_quad_indices ib t.nindices base;
          t.nindices <- t.nindices + 6
        | Geom.Sprite s ->
          let base = t.nverts / floats_per_vertex in
          let tw = float_of_int s.Geom.tex.Texture.width in
          let th = float_of_int s.Geom.tex.Texture.height in
          let d = s.Geom.dst in
          let cos_r = cos s.Geom.rotation and sin_r = sin s.Geom.rotation in
          let px = V2.x s.Geom.pivot and py = V2.y s.Geom.pivot in
          let rot x y =
            let dx = x -. px and dy = y -. py in
            ( px +. (dx *. cos_r) -. (dy *. sin_r)
            , py +. (dx *. sin_r) +. (dy *. cos_r) )
          in
          (* src rect (pixels, top-left origin) -> uv (bottom-left origin) *)
          let ul = s.Geom.src.Geom.x /. tw
          and ur = (s.Geom.src.Geom.x +. s.Geom.src.Geom.w) /. tw
          and vt = 1.0 -. (s.Geom.src.Geom.y /. th)
          and vb = 1.0 -. ((s.Geom.src.Geom.y +. s.Geom.src.Geom.h) /. th) in
          (* corners in the same order as points: tl, tr, br, bl *)
          let corner = function
            | 0 -> (d.Geom.x, d.Geom.y +. d.Geom.h)
            | 1 -> (d.Geom.x +. d.Geom.w, d.Geom.y +. d.Geom.h)
            | 2 -> (d.Geom.x +. d.Geom.w, d.Geom.y)
            | _ -> (d.Geom.x, d.Geom.y)
          in
          let uv = function
            | 0 -> (ul, vt) | 1 -> (ur, vt) | 2 -> (ur, vb) | _ -> (ul, vb)
          in
          for i = 0 to 3 do
            let cx, cy = corner i and u, v = uv i in
            let x, y = rot cx cy in
            write_vertex_uv ba t.nverts x y 0.0 s.Geom.tint u v;
            t.nverts <- t.nverts + floats_per_vertex
          done;
          write_quad_indices ib t.nindices base;
          t.nindices <- t.nindices + 6)
      prims;
    close t.nindices;
    t.runs <- runs;
    t.dirty <- false;
    true
  end

module Internal = struct
  let floats_per_vertex = floats_per_vertex
  let stride_bytes = stride_bytes
  let expand = expand
  let runs t = t.runs
  let vertices t = t.verts
  let vertex_floats t = t.nverts
  let indices t = t.indices
  let index_count t = t.nindices
end
