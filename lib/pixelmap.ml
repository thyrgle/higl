(* Pixelmap: the runner for Higl.Glsl fragment programs.

   A pixel map is a persistent, self-updating pixel buffer: each [step]
   renders [prog] (which samples the previous frame via [Higl.Glsl.self]
   and [nbhd]) into the back texture, then swaps the two ping-pong
   textures. [draw] blits the current texture to the screen.

   Channels are 8-bit (RGBA8), so stored colors live in [0, 1]. *)

open Tgl4

(* Shared vertex shader: fullscreen quad, a_pos in clip space; provides
   the v_uv that Higl.Glsl programs expect. *)
let vertex_source =
  {|#version 330 core
layout (location = 0) in vec2 a_pos;

out vec2 v_uv;

void main ()
{
  v_uv = (a_pos * 0.5) + 0.5;
  gl_Position = vec4 (a_pos, 0.0, 1.0);
}|}

(* Pass-through fragment shader for blitting a texture to the screen. *)
let blit_fragment_source =
  {|#version 330 core
in vec2 v_uv;

out vec4 frag_color;

uniform sampler2D u_tex;

void main ()
{
  frag_color = texture (u_tex, v_uv);
}|}

type t = {
  width : int;
  height : int;
  texs : int array;           (* ping-pong textures; [src] holds the last frame *)
  fbos : int array;           (* one FBO per texture *)
  mutable src : int;          (* index of the texture to read from next *)
  quad_vao : int;
  mutable last_prog : Glsl.program option;  (* identity-checked with == *)
  mutable prog_handle : int;
  mutable loc_prev : int;
  mutable loc_time : int;
  mutable loc_res : int;
  mutable loc_mouse : int;
  blit_prog : int;
  mutable blit_loc_tex : int;
  mutable time : float;       (* seconds *)
  mutable mouse_x : float;    (* 0..1, origin bottom-left *)
  mutable mouse_y : float;
}

let gen_one gen =
  let ba = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in
  gen 1 ba;
  Int32.to_int (Bigarray.Array1.get ba 0)

(** [create ~width ~height ()] allocates a [width x height] RGBA8 pixel
    map. Requires a current GL 3.3+ context. The initial contents are
    undefined; see [clear]. *)
let create ~width ~height () =
  if width < 1 || height < 1 then invalid_arg "Pixelmap.create: bad size";
  (* The quad VBO below is bound to the global GL_ARRAY_BUFFER target,
     which is CONTEXT state, not VAO state. Save the caller's binding
     and put it back, or their next buffer upload lands in the wrong
     buffer (a renderer that uploads lazily on first flush is hit
     hardest: nothing renders at all). *)
  let saved = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in
  Gl.get_integerv Gl.array_buffer_binding saved;
  (* Ping-pong textures: nearest filtering, edge clamping (so nbhd
     samples beyond the border stick to the edge texels). *)
  let texs = Array.make 2 0 in
  for i = 0 to 1 do
    let tex = gen_one Gl.gen_textures in
    Gl.bind_texture Gl.texture_2d tex;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.nearest;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
    Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;
    Gl.tex_image2d Gl.texture_2d 0 Gl.rgba8 width height 0 Gl.rgba
      Gl.unsigned_byte (`Offset 0);
    texs.(i) <- tex
  done;
  (* One FBO per texture. *)
  let fbos = Array.make 2 0 in
  for i = 0 to 1 do
    let fbo = gen_one Gl.gen_framebuffers in
    Gl.bind_framebuffer Gl.framebuffer fbo;
    Gl.framebuffer_texture2d Gl.framebuffer Gl.color_attachment0
      Gl.texture_2d texs.(i) 0;
    if Gl.check_framebuffer_status Gl.framebuffer <> Gl.framebuffer_complete then
      failwith "Pixelmap.create: framebuffer incomplete";
    fbos.(i) <- fbo
  done;
  Gl.bind_framebuffer Gl.framebuffer 0;
  (* Fullscreen quad as a triangle strip. *)
  let quad_vao = gen_one Gl.gen_vertex_arrays in
  let quad_vbo = gen_one Gl.gen_buffers in
  Gl.bind_vertex_array quad_vao;
  Gl.bind_buffer Gl.array_buffer quad_vbo;
  let verts = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 8 in
  let fill i x y =
    Bigarray.Array1.set verts i x;
    Bigarray.Array1.set verts (i + 1) y
  in
  fill 0 (-1.0) (-1.0);
  fill 2 1.0 (-1.0);
  fill 4 (-1.0) 1.0;
  fill 6 1.0 1.0;
  Gl.buffer_data Gl.array_buffer 32 (Some verts) Gl.static_draw;
  Gl.enable_vertex_attrib_array 0;
  Gl.vertex_attrib_pointer 0 2 Gl.float false 8 (`Offset 0);
  (* hand the global ARRAY_BUFFER binding back (see the note above) *)
  Gl.bind_buffer Gl.array_buffer (Int32.to_int (Bigarray.Array1.get saved 0));
  (* Blit program. *)
  let blit_prog =
    let vs = Shader.compile ~kind:Gl.vertex_shader "pixelmap blit vertex" vertex_source in
    let fs =
      Shader.compile ~kind:Gl.fragment_shader "pixelmap blit fragment"
        blit_fragment_source
    in
    Shader.link ~vertex:vs ~fragment:fs
  in
  let blit_loc_tex = Gl.get_uniform_location blit_prog "u_tex" in
  { width; height; texs; fbos; src = 0
  ; quad_vao
  ; last_prog = None; prog_handle = 0
  ; loc_prev = 0; loc_time = 0; loc_res = 0; loc_mouse = 0
  ; blit_prog; blit_loc_tex
  ; time = 0.0; mouse_x = 0.5; mouse_y = 0.5 }

(** [width t] / [height t] are the buffer's texel dimensions. *)
let width t = t.width
let height t = t.height

(** [clear t color] fills both ping-pong textures with [color]. *)
let clear t (c : Geom.color) =
  Gl.clear_color c.Geom.r c.Geom.g c.Geom.b c.Geom.a;
  for i = 0 to 1 do
    Gl.bind_framebuffer Gl.framebuffer t.fbos.(i);
    Gl.clear Gl.color_buffer_bit
  done;
  Gl.bind_framebuffer Gl.framebuffer 0

(** [set_time t seconds] sets the [u_time] uniform. *)
let set_time t seconds = t.time <- seconds

(** [set_mouse t ~x ~y] sets [u_mouse] to the normalized position
    [(x, y)] in 0..1, origin at the bottom-left of the buffer. *)
let set_mouse t ~x ~y = t.mouse_x <- x; t.mouse_y <- y

(* Compile [prog] if it is not the program [t] already holds (programs
   are compared by physical equality: keep one [Glsl.program] value per
   buffer and reuse it). *)
let ensure_program t prog =
  match t.last_prog with
  | Some p when p == prog -> ()
  | _ ->
      if t.last_prog <> None && t.prog_handle > 0 then
        Gl.delete_program t.prog_handle;
      let vs =
        Shader.compile ~kind:Gl.vertex_shader "pixelmap vertex" vertex_source
      in
      let fs =
        Shader.compile ~kind:Gl.fragment_shader "pixelmap fragment"
          (Glsl.fragment_source prog)
      in
      let handle = Shader.link ~vertex:vs ~fragment:fs in
      t.loc_prev <- Gl.get_uniform_location handle "u_prev";
      t.loc_time <- Gl.get_uniform_location handle "u_time";
      t.loc_res <- Gl.get_uniform_location handle "u_resolution";
      t.loc_mouse <- Gl.get_uniform_location handle "u_mouse";
      t.prog_handle <- handle;
      t.last_prog <- Some prog

(** [step t prog] runs one simulation step: renders [prog] over the
    whole buffer (reading the current texture as [u_prev], writing the
    other one), then swaps. Blending is disabled and the viewport is
    saved and restored. *)
let step t prog =
  ensure_program t prog;
  let dst = 1 - t.src in
  let blend_was_on = Gl.is_enabled Gl.blend in
  let vp = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 4 in
  Gl.get_integerv Gl.viewport_enum vp;
  Gl.disable Gl.blend;
  Gl.bind_framebuffer Gl.framebuffer t.fbos.(dst);
  Gl.viewport 0 0 t.width t.height;
  Gl.use_program t.prog_handle;
  Gl.active_texture Gl.texture0;
  Gl.bind_texture Gl.texture_2d t.texs.(t.src);
  Gl.uniform1i t.loc_prev 0;
  Gl.uniform1f t.loc_time t.time;
  Gl.uniform2f t.loc_res (float_of_int t.width) (float_of_int t.height);
  Gl.uniform2f t.loc_mouse t.mouse_x t.mouse_y;
  Gl.bind_vertex_array t.quad_vao;
  Gl.draw_arrays Gl.triangle_strip 0 4;
  (* Restore the state we touched. *)
  Gl.bind_framebuffer Gl.framebuffer 0;
  Gl.viewport
    (Int32.to_int (Bigarray.Array1.get vp 0))
    (Int32.to_int (Bigarray.Array1.get vp 1))
    (Int32.to_int (Bigarray.Array1.get vp 2))
    (Int32.to_int (Bigarray.Array1.get vp 3));
  if blend_was_on then Gl.enable Gl.blend;
  Gl.bind_texture Gl.texture_2d 0;
  t.src <- dst

(** [draw t] blits the current texture of [t] to the currently bound
    framebuffer, filling its viewport. Call after [step] (e.g. between
    [Renderer.clear_screen] and the swap). *)
let draw t =
  Gl.use_program t.blit_prog;
  Gl.active_texture Gl.texture0;
  Gl.bind_texture Gl.texture_2d t.texs.(t.src);
  Gl.uniform1i t.blit_loc_tex 0;
  Gl.bind_vertex_array t.quad_vao;
  Gl.draw_arrays Gl.triangle_strip 0 4;
  Gl.bind_texture Gl.texture_2d 0
