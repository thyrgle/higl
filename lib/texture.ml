(* Texture: RGBA8 2D textures for sprites.

   [create] uploads pixel data; sprites reference the resulting [t].
   The record is transparent (like raylib's Texture2D) so geometry code
   can run without a GL context, but only use [id] values handed out by
   [create] — anything else is an invalid GL texture name. *)

open Tgl4

(** Texture magnification/minification filtering. *)
type filter = Nearest | Linear

(** Behavior for texture coordinates outside 0..1. *)
type wrap = Clamp | Repeat

(** A GPU texture. [id] is the GL texture name; [width]/[height] are in
    pixels. *)
type t = { id : int; width : int; height : int }

let gen_one gen =
  let ba = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in
  gen 1 ba;
  Int32.to_int (Bigarray.Array1.get ba 0)

(** [create ?filter ?wrap ~width ~height data] uploads [data] as an RGBA8
    texture. [data] is [width * height * 4] bytes, row-major, first pixel
    at the top-left (image-file convention); it is flipped here so it
    appears upright in world space (y is up). [filter] defaults to
    [Nearest] (crisp pixels), [wrap] to [Clamp]. Requires a current GL
    context; binds a texture, leaving the binding set. *)
let create ?(filter = Nearest) ?(wrap = Clamp) ~width ~height data =
  if width < 1 || height < 1 then invalid_arg "Texture.create: bad size";
  let expected = (width * height) * 4 in
  if Bigarray.Array1.dim data <> expected then
    invalid_arg
      (Printf.sprintf "Texture.create: expected %d bytes of RGBA data, got %d"
         expected (Bigarray.Array1.dim data));
  (* GL fills textures bottom-up; input is top-down, so copy the rows in
     reverse order. *)
  let flipped =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout expected
  in
  let row_bytes = width * 4 in
  for row = 0 to height - 1 do
    let src = row * row_bytes and dst = (height - 1 - row) * row_bytes in
    for i = 0 to row_bytes - 1 do
      flipped.{dst + i} <- data.{src + i}
    done
  done;
  let id = gen_one Gl.gen_textures in
  Gl.bind_texture Gl.texture_2d id;
  let f = match filter with Nearest -> Gl.nearest | Linear -> Gl.linear in
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter f;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter f;
  let w = match wrap with Clamp -> Gl.clamp_to_edge | Repeat -> Gl.repeat in
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s w;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t w;
  Gl.tex_image2d Gl.texture_2d 0 Gl.rgba8 width height 0 Gl.rgba
    Gl.unsigned_byte (`Data flipped);
  { id; width; height }

(** [width t] / [height t] are the texture's dimensions in pixels. *)
let width t = t.width
let height t = t.height

(** [set_filter t filter] changes magnification/minification filtering
    (binds the texture). *)
let set_filter t filter =
  let f = match filter with Nearest -> Gl.nearest | Linear -> Gl.linear in
  Gl.bind_texture Gl.texture_2d t.id;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter f;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter f

(** [set_wrap t wrap] changes the wrap mode for s/t coordinates (binds
    the texture). *)
let set_wrap t wrap =
  let w = match wrap with Clamp -> Gl.clamp_to_edge | Repeat -> Gl.repeat in
  Gl.bind_texture Gl.texture_2d t.id;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s w;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t w

(** [delete t] frees the GPU texture. The record stays valid to read
    ([id] is just a dead GL name after this). *)
let delete t =
  let ids = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 1 in
  Bigarray.Array1.set ids 0 (Int32.of_int t.id);
  Gl.delete_textures 1 ids
