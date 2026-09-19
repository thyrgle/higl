(* Font: bitmap text rendering.

   An embedded public-domain 8x8 monospace font (font8x8, Daniel Hepper /
   Marcel Sondaar / IBM VGA font, public domain) is baked into a 128x64
   RGBA atlas at [create] time; [draw] queues one sprite per glyph on a
   [Mesh.t], so text renders in one batched draw call alongside
   everything else, needs no font files or CPU text shaping, and costs a
   few quads of GPU time. With a y-down camera, (x, y) is the visual
   top-left of the string, like raylib's DrawText. *)

(* The embedded font8x8 basic-latin set, U+0000..U+007F: one entry per
   code, each a 16-char hex string of its 8 row bytes, row 0 first; in
   every byte the LSB is the leftmost pixel. *)
let glyph_rows : string array =
  [| "0000000000000000"; "0000000000000000"; "0000000000000000"; (* 00-02 *)
     "0000000000000000"; "0000000000000000"; "0000000000000000"; (* 03-05 *)
     "0000000000000000"; "0000000000000000"; "0000000000000000"; (* 06-08 *)
     "0000000000000000"; "0000000000000000"; "0000000000000000"; (* 09-11 *)
     "0000000000000000"; "0000000000000000"; "0000000000000000"; (* 12-14 *)
     "0000000000000000"; "0000000000000000"; "0000000000000000"; (* 15-17 *)
     "0000000000000000"; "0000000000000000"; "0000000000000000"; (* 18-20 *)
     "0000000000000000"; "0000000000000000"; "0000000000000000"; (* 21-23 *)
     "0000000000000000"; "0000000000000000"; "0000000000000000"; (* 24-26 *)
     "0000000000000000"; "0000000000000000"; "0000000000000000"; (* 27-29 *)
     "0000000000000000"; "0000000000000000"; "0000000000000000"; (* 30-32 *)
     "183C3C1818001800"; "3636000000000000"; "36367F367F363600"; (* ! '!' quote hash *)
     "0C3E031E301F0C00"; "006333180C666300"; "1C361C6E3B336E00"; (* $ % & *)
     "0606030000000000"; "180C0606060C1800"; "060C1818180C0600"; (* ' ( ) *)
     "00663CFF3C660000"; "000C0C3F0C0C0000"; "00000000000C0C06"; (* * + , *)
     "0000003F00000000"; "00000000000C0C00"; "6030180C06030100"; (* - . / *)
     "3E63737B6F673E00"; "0C0E0C0C0C0C3F00"; "1E33301C06333F00"; (* 0 1 2 *)
     "1E33301C30331E00"; "383C36337F307800"; "3F031F3030331E00"; (* 3 4 5 *)
     "1C06031F33331E00"; "3F3330180C0C0C00"; "1E33331E33331E00"; (* 6 7 8 *)
     "1E33333E30180E00"; "000C0C00000C0C00"; "000C0C00000C0C06"; (* 9 : ; *)
     "180C0603060C1800"; "00003F00003F0000"; "060C1830180C0600"; (* < = > *)
     "1E3330180C000C00"; "3E637B7B7B031E00"; "0C1E33333F333300"; (* ? @ A *)
     "3F66663E66663F00"; "3C66030303663C00"; "1F36666666361F00"; (* B C D *)
     "7F46161E16467F00"; "7F46161E16060F00"; "3C66030373667C00"; (* E F G *)
     "3333333F33333300"; "1E0C0C0C0C0C1E00"; "7830303033331E00"; (* H I J *)
     "6766361E36666700"; "0F06060646667F00"; "63777F7F6B636300"; (* K L M *)
     "63676F7B73636300"; "1C36636363361C00"; "3F66663E06060F00"; (* N O P *)
     "1E3333333B1E3800"; "3F66663E36666700"; "1E33070E38331E00"; (* Q R S *)
     "3F2D0C0C0C0C1E00"; "3333333333333F00"; "333333331E0C0000"; (* T U V *)
     "6363636B7F776300"; "6363361C1C366300"; "3333331E0C0C1E00"; (* W X Y *)
     "7F6331184C667F00"; "1E06060606061E00"; "03060C1830604000"; (* Z [ \ *)
     "1E18181818181E00"; "081C366300000000"; "00000000000000FF"; (* ] ^ _ *)
     "0C0C180000000000"; "00001E303E336E00"; "0706063E66663B00"; (* ` a b *)
     "00001E3303331E00"; "3830303E33336E00"; "00001E333F031E00"; (* c d e *)
     "1C36060F06060F00"; "00006E33333E301F"; "0706366E66666700"; (* f g h *)
     "0C000E0C0C0C1E00"; "300030303033331E"; "070666361E366700"; (* i j k *)
     "0E0C0C0C0C0C1E00"; "0000337F7F6B6300"; "00001F3333333300"; (* l m n *)
     "00001E3333331E00"; "00003B66663E060F"; "00006E33333E3078"; (* o p q *)
     "00003B6E66060F00"; "00003E031E301F00"; "080C3E0C0C2C1800"; (* r s t *)
     "0000333333336E00"; "00003333331E0C00"; "0000636B7F7F3600"; (* u v w *)
     "000063361C366300"; "00003333333E301F"; "00003F190C263F00"; (* x y z *)
     "380C0C070C0C3800"; "1818180018181800"; "070C0C380C0C0700"; (* { | } *)
     "6E3B000000000000"; "0000000000000000" |]                  (* ~ DEL *)

let glyph_w = 8
let glyph_h = 8

(* Atlas layout: 16 columns x 8 rows of 8x8 glyphs (128 slots, one per
   ASCII code). *)
let atlas_cols = 16
let atlas_w = atlas_cols * glyph_w
let atlas_h = 8 * glyph_h

type t = { tex : Texture.t }

let printable code = code >= 32 && code <= 126

let hex_digit = function
  | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4 | '5' -> 5
  | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9 | 'a' | 'A' -> 10
  | 'b' | 'B' -> 11 | 'c' | 'C' -> 12 | 'd' | 'D' -> 13 | 'e' | 'E' -> 14
  | _ -> 15

(* Build the atlas: glyph pixels are white with alpha, so [tint] colors
   them at draw time. Rows are written bottom-up: [Texture.create] flips
   image rows on upload, and the sprite UV convention then samples each
   glyph upright. *)
let atlas () =
  let data =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      ((atlas_w * atlas_h) * 4)
  in
  Bigarray.Array1.fill data 0;
  Array.iteri
    (fun code hex ->
      let col = code mod atlas_cols and row = code / atlas_cols in
      String.iteri
        (fun k hx ->
          (* hex chars come in pairs: two per row byte *)
          if k mod 2 = 0 then begin
            let byte = (hex_digit hx lsl 4) lor hex_digit hex.[k + 1] in
            let rowi = glyph_h - 1 - (k / 2) in
            let base =
              (4 * (((row * glyph_h) + rowi) * atlas_w))
              + (4 * (col * glyph_w))
            in
            for bit = 0 to 7 do
              (* font8x8 rows: LSB is the leftmost pixel *)
              if (byte lsr bit) land 1 = 1 then begin
                let p = base + (bit * 4) in
                Bigarray.Array1.set data p 255;
                Bigarray.Array1.set data (p + 1) 255;
                Bigarray.Array1.set data (p + 2) 255;
                Bigarray.Array1.set data (p + 3) 255
              end
            done
          end)
        hex)
    glyph_rows;
  data

(** [create ?filter ()] builds the font atlas texture. Requires a
    current GL context; create it once and share it. [filter] defaults
    to [Nearest] (crisp pixel glyphs at integer scales). *)
let create ?(filter = Texture.Nearest) () =
  { tex = Texture.create ~filter ~width:atlas_w ~height:atlas_h (atlas ()) }

(** [texture t] is the atlas texture (for custom sprite work). *)
let texture t = t.tex

(** The glyph advance in world units at scale [s]. *)
let advance ~scale = float_of_int glyph_w *. scale

(** The glyph height / line height in world units at scale [s]. *)
let line_height ~scale = float_of_int glyph_h *. scale

(** [measure ?scale ?aspect s] is the rendered width of [s] in world
    units (monospace: the [aspect]-scaled advance times the length). *)
let measure ?(scale = 1.0) ?(aspect = 1.0) s =
  advance ~scale *. aspect *. float_of_int (String.length s)

(** [draw mesh t ?scale ?aspect ?color ~x ~y s] queues [s] on [mesh]
    with the top-left of the first glyph at (x, y). [scale] is the world
    size of one font pixel (default 1); [aspect] squeezes glyph widths
    horizontally (default 1; 0.5 approximates raylib's narrow default
    font); [color] tints the glyphs (default opaque white).
    Non-printable characters advance without drawing. *)
let draw mesh t ?(scale = 1.0) ?(aspect = 1.0) ?(color = Geom.white) ~x ~y s =
  String.iteri
    (fun i c ->
      let code = Char.code c in
      if printable code then begin
        let col = code mod atlas_cols and row = code / atlas_cols in
        let src =
          Geom.rect
            (float_of_int (col * glyph_w))
            (float_of_int (row * glyph_h))
            (float_of_int glyph_w) (float_of_int glyph_h)
        in
        let gx = x +. (advance ~scale *. aspect *. float_of_int i) in
        let dst =
          Geom.rect gx y (advance ~scale *. aspect) (line_height ~scale)
        in
        Mesh.add_sprite mesh (Geom.sprite ~src ~tint:color t.tex dst)
      end)
    s
