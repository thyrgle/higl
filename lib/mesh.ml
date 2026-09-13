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

   Vertex layout: pos3 color4 uv2 (uv zero-filled until textures exist). *)

type t = {
  mutable rev : Geom.primitive list;  (* insertion order, reversed *)
  mutable nprims : int;
  mutable dirty : bool;
  mutable verts : (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  mutable nverts : int;  (* floats used in [verts] *)
  mutable indices : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  mutable nindices : int;
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
  ; nindices = 0 }

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

let clear t =
  t.rev <- [];
  t.nprims <- 0;
  t.nverts <- 0;
  t.nindices <- 0;
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

(* Write one full vertex (9 floats) at float index [i]. *)
let write_vertex ba i (v : Geom.vertex) =
  let p = v.Geom.pos and c = v.Geom.color in
  ba.{i} <- p.Geom.x;
  ba.{i + 1} <- p.Geom.y;
  ba.{i + 2} <- p.Geom.z;
  ba.{i + 3} <- c.Geom.r;
  ba.{i + 4} <- c.Geom.g;
  ba.{i + 5} <- c.Geom.b;
  ba.{i + 6} <- c.Geom.a;
  ba.{i + 7} <- 0.0;  (* uv, reserved *)
  ba.{i + 8} <- 0.0

(* Write raw position + color as a vertex at float index [i]. *)
let write_xyz_rgba ba i x y z (c : Geom.color) =
  ba.{i} <- x;
  ba.{i + 1} <- y;
  ba.{i + 2} <- z;
  ba.{i + 3} <- c.Geom.r;
  ba.{i + 4} <- c.Geom.g;
  ba.{i + 5} <- c.Geom.b;
  ba.{i + 6} <- c.Geom.a;
  ba.{i + 7} <- 0.0;
  ba.{i + 8} <- 0.0

let write_quad_indices ba idx base =
  ba.{idx} <- Int32.of_int base;
  ba.{idx + 1} <- Int32.of_int (base + 1);
  ba.{idx + 2} <- Int32.of_int (base + 2);
  ba.{idx + 3} <- Int32.of_int base;
  ba.{idx + 4} <- Int32.of_int (base + 2);
  ba.{idx + 5} <- Int32.of_int (base + 3)

let expand t =
  if not t.dirty then false
  else begin
    let prims = List.rev t.rev in
    let verts_needed = ref 0 and indices_needed = ref 0 in
    List.iter
      (fun p ->
        let v, i =
          match p with
          | Geom.Triangle _ -> (tri_v, tri_i)
          | Geom.Line _ | Geom.Point _ -> (quad_v, quad_i)
        in
        verts_needed := !verts_needed + (v * floats_per_vertex);
        indices_needed := !indices_needed + i)
      prims;
    ensure t ~verts_needed:!verts_needed ~indices_needed:!indices_needed;
    let ba = t.verts and ib = t.indices in
    t.nverts <- 0;
    t.nindices <- 0;
    List.iter
      (fun p ->
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
          t.nindices <- t.nindices + 6)
      prims;
    t.dirty <- false;
    true
  end

module Internal = struct
  let floats_per_vertex = floats_per_vertex
  let stride_bytes = stride_bytes
  let expand = expand
  let vertices t = t.verts
  let vertex_floats t = t.nverts
  let indices t = t.indices
  let index_count t = t.nindices
end
