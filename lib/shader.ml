(* Default shader for the fixed vertex layout pos3 color4 uv2, with an ortho
   camera uniform. Also holds the small compile/link helpers used to build it. *)

open Tgl4

let default_vertex_source =
  {|#version 330 core
layout (location = 0) in vec3 a_pos;
layout (location = 1) in vec4 a_color;
layout (location = 2) in vec2 a_uv;

uniform mat4 u_camera;

out vec4 v_color;
out vec2 v_uv;

void main ()
{
  gl_Position = u_camera * vec4 (a_pos, 1.0);
  v_color = a_color;
  v_uv = a_uv;
}|}

let default_fragment_source =
  {|#version 330 core
in vec4 v_color;
in vec2 v_uv;

out vec4 frag_color;

void main ()
{
  frag_color = v_color;
}|}

let check_shader name shader =
  let status = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in
  Gl.get_shaderiv shader Gl.compile_status status;
  if Bigarray.Array1.get status 0 = 0l then begin
    let len = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in
    Gl.get_shaderiv shader Gl.info_log_length len;
    let n = Int32.to_int (Bigarray.Array1.get len 0) in
    if n <= 0 then failwith (Printf.sprintf "%s: compile failed (no log)" name)
    else begin
      let log = Bigarray.Array1.create Bigarray.char Bigarray.c_layout n in
      Gl.get_shader_info_log shader n None log;
      failwith (Printf.sprintf "%s: compile failed:\n%s" name
                  (Gl.string_of_bigarray log))
    end
  end

let check_program name program =
  let status = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in
  Gl.get_programiv program Gl.link_status status;
  if Bigarray.Array1.get status 0 = 0l then begin
    let len = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in
    Gl.get_programiv program Gl.info_log_length len;
    let n = Int32.to_int (Bigarray.Array1.get len 0) in
    if n <= 0 then failwith (Printf.sprintf "%s: link failed (no log)" name)
    else begin
      let log = Bigarray.Array1.create Bigarray.char Bigarray.c_layout n in
      Gl.get_program_info_log program n None log;
      failwith (Printf.sprintf "%s: link failed:\n%s" name
                  (Gl.string_of_bigarray log))
    end
  end

let compile ~kind name source =
  let shader = Gl.create_shader kind in
  Gl.shader_source shader source;
  Gl.compile_shader shader;
  check_shader name shader;
  shader

let link ~vertex ~fragment =
  let program = Gl.create_program () in
  Gl.attach_shader program vertex;
  Gl.attach_shader program fragment;
  Gl.link_program program;
  check_program "shader program" program;
  Gl.delete_shader vertex;
  Gl.delete_shader fragment;
  program

(* Returns the linked default program. *)
let create_default () =
  let vertex =
    compile ~kind:Gl.vertex_shader "default vertex shader" default_vertex_source
  in
  let fragment =
    compile ~kind:Gl.fragment_shader "default fragment shader"
      default_fragment_source
  in
  link ~vertex ~fragment
