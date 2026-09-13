(* Renderer: owns all GPU state (VAO/VBO/EBO, default shader, ortho camera).
   Geometry lives in CPU-side [Mesh.t] values; [flush] uploads and draws. *)

open Tgl4

type camera = {
  left : float;
  right : float;
  bottom : float;
  top : float;
  near : float;
  far : float;
}

type t = {
  vao : int;
  vbo : int;
  ebo : int;
  program : int;
  camera_loc : int;
  mutable camera : camera;
  mutable camera_dirty : bool;
  mutable gpu_vcap : int;  (* bytes allocated in the VBO *)
  mutable gpu_ecap : int;  (* bytes allocated in the EBO *)
  default_mesh : Mesh.t;
}

let default_camera ~width ~height =
  { left = 0.0; right = width; bottom = 0.0; top = height; near = -1.0; far = 1.0 }

let gen_one gen =
  let ba = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in
  gen 1 ba;
  Int32.to_int (Bigarray.Array1.get ba 0)

let create ?(camera = default_camera ~width:100.0 ~height:100.0) () =
  let vao = gen_one Gl.gen_vertex_arrays in
  let vbo = gen_one Gl.gen_buffers in
  let ebo = gen_one Gl.gen_buffers in
  Gl.bind_vertex_array vao;
  Gl.bind_buffer Gl.array_buffer vbo;
  Gl.bind_buffer Gl.element_array_buffer ebo;
  let stride = Mesh.Internal.stride_bytes in
  Gl.vertex_attrib_pointer 0 3 Gl.float false stride (`Offset 0);
  Gl.enable_vertex_attrib_array 0;
  Gl.vertex_attrib_pointer 1 4 Gl.float false stride (`Offset 12);
  Gl.enable_vertex_attrib_array 1;
  Gl.vertex_attrib_pointer 2 2 Gl.float false stride (`Offset 28);
  Gl.enable_vertex_attrib_array 2;
  let program = Shader.create_default () in
  let camera_loc = Gl.get_uniform_location program "u_camera" in
  Gl.enable Gl.blend;
  Gl.blend_func Gl.src_alpha Gl.one_minus_src_alpha;
  { vao; vbo; ebo; program; camera_loc
  ; camera; camera_dirty = true
  ; gpu_vcap = 0; gpu_ecap = 0
  ; default_mesh = Mesh.create () }

let get_camera t = t.camera

let set_camera t camera =
  t.camera <- camera;
  t.camera_dirty <- true

let set_viewport t ~width ~height = Gl.viewport 0 0 width height

let default_mesh t = t.default_mesh

(** [queue t prim] adds a primitive to the renderer's default mesh. *)
let queue t prim = Mesh.add_prim t.default_mesh prim

let clear_screen t (c : Geom.color) =
  Gl.clear_color c.Geom.r c.Geom.g c.Geom.b c.Geom.a;
  Gl.clear Gl.color_buffer_bit

(* Upload a bigarray (of float32/int32) to [target], growing the GPU buffer
   only when needed. [bytes] is the exact size to upload. *)
let upload target data ~bytes ~gpu_cap =
  if bytes > gpu_cap then begin
    Gl.buffer_data target bytes (Some data) Gl.dynamic_draw;
    bytes
  end else begin
    Gl.buffer_sub_data target 0 bytes (Some data);
    gpu_cap
  end

(** [flush ?mesh t] uploads [mesh] (default: the renderer's default mesh) if
    dirty and draws it. Non-destructive: the mesh keeps its primitives.
    Flush order = paint order; blending is on, so order matters. *)
let flush ?mesh t =
  let mesh = match mesh with Some m -> m | None -> t.default_mesh in
  Gl.bind_vertex_array t.vao;
  if Mesh.Internal.expand mesh then begin
    let vf = Mesh.Internal.vertex_floats mesh in
    if vf > 0 then
      t.gpu_vcap <-
        upload Gl.array_buffer (Mesh.Internal.vertices mesh)
          ~bytes:(vf * 4) ~gpu_cap:t.gpu_vcap;
    let ic = Mesh.Internal.index_count mesh in
    if ic > 0 then
      t.gpu_ecap <-
        upload Gl.element_array_buffer (Mesh.Internal.indices mesh)
          ~bytes:(ic * 4) ~gpu_cap:t.gpu_ecap
  end;
  Gl.use_program t.program;
  if t.camera_dirty then begin
    let c = t.camera in
    Gl.uniform_matrix4fv t.camera_loc 1 false
      (Mat4.ortho ~left:c.left ~right:c.right ~bottom:c.bottom ~top:c.top
         ~near:c.near ~far:c.far);
    t.camera_dirty <- false
  end;
  let ic = Mesh.Internal.index_count mesh in
  if ic > 0 then Gl.draw_elements Gl.triangles ic Gl.unsigned_int (`Offset 0)
