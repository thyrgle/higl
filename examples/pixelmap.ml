(* Example: a feedback pixel map ("GLSL ORM"). Paint with the mouse;
   each step blurs the neighborhood and slowly fades, leaving warm
   smoke-like trails. Requires tsdl (window + GL context). *)

open Tsdl

let width, height = (640, 480)

let ( >>= ) = Result.bind

let die : ('a, [ `Msg of string ]) result -> 'a = function
  | Ok v -> v
  | Error (`Msg e) -> Sdl.log "fatal: %s" e; exit 1

let check = die

(* One line per pixel: paint where the mouse is, blend in the average
   of the 8 neighbors plus the pixel itself, fade a little. *)
let prog =
  let open Higl.Glsl in
  program (fun self ->
    let lit =
      if_ (length (mouse -. uv) <. f 0.03)
        (v4 (f 1.0) (f 0.85) (f 0.5) (f 1.0))
        self
    in
    let blurred = mix lit (avg (include_center (nbhd lit))) (f 0.5) in
    scale (f 0.995) blurred)

let main () =
  die (Sdl.init Sdl.Init.video);
  check (Sdl.gl_set_attribute Sdl.Gl.context_major_version 3);
  check (Sdl.gl_set_attribute Sdl.Gl.context_minor_version 3);
  let win =
    die (Sdl.create_window "higl pixelmap" ~w:width ~h:height Sdl.Window.opengl)
  in
  let ctx = die (Sdl.gl_create_context win) in
  check (Sdl.gl_make_current win ctx);

  let buf = Higl.Pixelmap.create ~width ~height () in
  Higl.Pixelmap.clear buf (Higl.color 0.0 0.0 0.0 1.0);

  let event = Sdl.Event.create () in
  let quit = ref false in
  let start = Sdl.get_ticks () in
  while not !quit do
    while Sdl.poll_event (Some event) do
      match Sdl.Event.get event Sdl.Event.typ with
      | t when t = Sdl.Event.quit -> quit := true
      | t when t = Sdl.Event.key_down ->
          if Sdl.Event.get event Sdl.Event.keyboard_keycode = Sdl.K.escape then
            quit := true
      | t when t = Sdl.Event.mouse_motion -> (
          let x = Sdl.Event.get event Sdl.Event.mouse_motion_x in
          let y = Sdl.Event.get event Sdl.Event.mouse_motion_y in
          Higl.Pixelmap.set_mouse buf
            ~x:(Float.div (float_of_int x) (float_of_int width))
            ~y:(Float.div (float_of_int (height - y)) (float_of_int height)))
      | _ -> ()
    done;
    Higl.Pixelmap.set_time buf
      (Float.div
         (Int32.to_float (Int32.sub (Sdl.get_ticks ()) start))
         1000.0);
    Higl.Pixelmap.step buf prog;
    Higl.Pixelmap.draw buf;
    Sdl.gl_swap_window win
  done;
  Sdl.quit ()

let () = main ()
