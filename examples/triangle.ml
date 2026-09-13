(* Example: a static gradient triangle mesh plus a streaming default mesh
   with a line and a point. Requires tsdl (window + GL context). *)

open Tsdl

let width, height = (640, 480)

let ( >>= ) = Result.bind

let die : ('a, [ `Msg of string ]) result -> 'a = function
  | Ok v -> v
  | Error (`Msg e) -> Sdl.log "fatal: %s" e; exit 1

let check = die

let main () =
  die (Sdl.init Sdl.Init.video);
  check (Sdl.gl_set_attribute Sdl.Gl.context_major_version 3);
  check (Sdl.gl_set_attribute Sdl.Gl.context_minor_version 3);
  let win =
    die (Sdl.create_window "higl triangle" ~w:width ~h:height Sdl.Window.opengl)
  in
  let ctx = die (Sdl.gl_create_context win) in
  check (Sdl.gl_make_current win ctx);

  let r =
    Higl.Renderer.create
      ~camera:
        (Higl.Renderer.default_camera
           ~width:(float_of_int width)
           ~height:(float_of_int height))
      ()
  in
  Higl.Renderer.set_viewport r ~width ~height;

  (* Static mesh: gradient triangle (built once, flushed every frame). *)
  let mesh = Higl.Mesh.create () in
  let red = Higl.vertex (Higl.point 320.0 100.0 0.0) (Higl.color 1.0 0.0 0.0 1.0) in
  let green = Higl.vertex (Higl.point 100.0 380.0 0.0) (Higl.color 0.0 1.0 0.0 1.0) in
  let blue = Higl.vertex (Higl.point 540.0 380.0 0.0) (Higl.color 0.0 0.0 1.0 1.0) in
  Higl.Mesh.add_triangle mesh { Higl.v1 = red; v2 = green; v3 = blue };

  let event = Sdl.Event.create () in
  let quit = ref false in
  while not !quit do
    while Sdl.poll_event (Some event) do
      match Sdl.Event.get event Sdl.Event.typ with
      | t when t = Sdl.Event.quit -> quit := true
      | t when t = Sdl.Event.key_down ->
          if Sdl.Event.get event Sdl.Event.keyboard_keycode = Sdl.K.escape then quit := true
      | _ -> ()
    done;
    Higl.Renderer.clear_screen r (Higl.color 0.08 0.08 0.10 1.0);
    Higl.Renderer.flush r ~mesh;
    (* Streaming path: default mesh, rebuilt each frame. *)
    Higl.Renderer.queue r
      (Higl.Line
         (Higl.line
            ~width:3.0
            (Higl.vertex (Higl.point 50.0 50.0 0.0) (Higl.color 1.0 1.0 0.0 1.0))
            (Higl.vertex (Higl.point 590.0 50.0 0.0) (Higl.color 1.0 0.0 1.0 1.0))));
    Higl.Renderer.queue r
      (Higl.Point
         (Higl.point_prim
            ~size:6.0
            (Higl.vertex (Higl.point 320.0 30.0 0.0) (Higl.color 0.0 1.0 1.0 1.0))));
    Higl.Renderer.flush r;
    Higl.Mesh.clear (Higl.Renderer.default_mesh r);
    Sdl.gl_swap_window win
  done;
  Sdl.quit ()

let () = main ()
