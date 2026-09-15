(* Example: sprites, shapes and a Camera2d — the retained-mode tour.

   Builds a static mesh once (background rect, circle, hexagon, circle
   outline, three sprites — one tinted, one using a sub-rectangle of
   the texture) and streams one spinning sprite per frame on the
   default mesh.

   Controls: drag with the left mouse button to pan, mouse wheel to
   zoom, Q/E to rotate, R to reset, Escape to quit. Requires tsdl. *)

open Tsdl

let width, height = (640, 480)

let ( >>= ) = Result.bind

let die : ('a, [ `Msg of string ]) result -> 'a = function
  | Ok v -> v
  | Error (`Msg e) -> Sdl.log "fatal: %s" e; exit 1

let check = die

(* A 16x16 checker with a solid border, RGBA8, top-left first. *)
let make_checker () =
  let n = 16 in
  let data =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      ((n * n) * 4)
  in
  for y = 0 to n - 1 do
    for x = 0 to n - 1 do
      let i = ((y * n) + x) * 4 in
      let border = x = 0 || y = 0 || x = n - 1 || y = n - 1 in
      let on = ((x / 4) + (y / 4)) mod 2 = 0 in
      let r, g, b =
        if border then (30, 30, 40)
        else if on then (240, 240, 240)
        else (90, 120, 200)
      in
      Bigarray.Array1.set data i r;
      Bigarray.Array1.set data (i + 1) g;
      Bigarray.Array1.set data (i + 2) b;
      Bigarray.Array1.set data (i + 3) 255
    done
  done;
  (n, data)

let main () =
  die (Sdl.init Sdl.Init.video);
  check (Sdl.gl_set_attribute Sdl.Gl.context_major_version 3);
  check (Sdl.gl_set_attribute Sdl.Gl.context_minor_version 3);
  let win =
    die (Sdl.create_window "higl sprites" ~w:width ~h:height Sdl.Window.opengl)
  in
  let ctx = die (Sdl.gl_create_context win) in
  check (Sdl.gl_make_current win ctx);

  let r = Higl.Renderer.create () in
  Higl.Renderer.set_viewport r ~width ~height;

  let n, data = make_checker () in
  let tex =
    Higl.Texture.create ~width:n ~height:n data
  in

  (* Static scene: shapes and sprites, baked once into a mesh. *)
  let scene = Higl.Mesh.create () in
  Higl.Mesh.add_rect scene ~color:(Higl.color 0.12 0.12 0.16 1.0)
    (Higl.rect (-2000.0) (-2000.0) 4000.0 4000.0);
  Higl.Mesh.add_circle scene ~color:(Higl.color 0.9 0.4 0.2 1.0)
    (Gg.V2.v 0.0 0.0) 180.0;
  Higl.Mesh.add_polygon scene ~color:(Higl.color 0.2 0.7 0.5 1.0) ~sides:6
    (Gg.V2.v 420.0 120.0) 70.0;
  Higl.Mesh.add_circle_outline scene ~color:(Higl.color 0.9 0.8 0.3 1.0)
    ~thickness:3.0 (Gg.V2.v (-320.0) 120.0) 60.0;
  Higl.Mesh.add_polyline scene ~color:(Higl.color 0.8 0.8 0.9 1.0) ~width:2.0
    [ Gg.V2.v (-300.0) (-150.0); Gg.V2.v (-150.0) (-40.0); Gg.V2.v 0.0 (-120.0);
      Gg.V2.v 150.0 (-40.0); Gg.V2.v 300.0 (-150.0) ];
  Higl.Mesh.add_sprite scene
    (Higl.sprite tex (Higl.rect (-260.0) (-60.0) 128.0 128.0));
  Higl.Mesh.add_sprite scene
    (Higl.sprite ~tint:(Higl.color 1.0 0.4 0.4 1.0) tex
       (Higl.rect 320.0 (-140.0) 96.0 96.0));
  (* Left half of the checker: a sub-rectangle of the texture. *)
  Higl.Mesh.add_sprite scene
    (Higl.sprite ~src:(Higl.rect 0.0 0.0 8.0 16.0) tex
       (Higl.rect (-380.0) (-160.0) 64.0 128.0));

  let cam = ref (Higl.Camera2d.create ~offset:(Gg.V2.v 320.0 240.0) ()) in
  let event = Sdl.Event.create () in
  let quit = ref false and spin = ref 0.0 in
  while not !quit do
    while Sdl.poll_event (Some event) do
      match Sdl.Event.get event Sdl.Event.typ with
      | t when t = Sdl.Event.quit -> quit := true
      | t when t = Sdl.Event.key_down -> (
          match Sdl.Event.get event Sdl.Event.keyboard_keycode with
          | k when k = Sdl.K.escape -> quit := true
          | k when k = Sdl.K.r ->
              cam := Higl.Camera2d.create ~offset:(Gg.V2.v 320.0 240.0) ()
          | k when k = Sdl.K.q ->
              cam :=
                { !cam with
                  Higl.Camera2d.rotation =
                    !cam.Higl.Camera2d.rotation +. 0.05 }
          | k when k = Sdl.K.e ->
              cam :=
                { !cam with
                  Higl.Camera2d.rotation =
                    !cam.Higl.Camera2d.rotation -. 0.05 }
          | _ -> ())
      | t when t = Sdl.Event.mouse_wheel ->
          let dy = Sdl.Event.get event Sdl.Event.mouse_wheel_y in
          let f = if dy > 0 then 1.15 else 1.0 /. 1.15 in
          cam := { !cam with Higl.Camera2d.zoom = !cam.Higl.Camera2d.zoom *. f }
      | t when t = Sdl.Event.mouse_motion -> (
          (* Drag the world with the mouse: px -> world units, undoing
             the camera's zoom and rotation. *)
          let dragging =
            Int32.logand (Sdl.Event.get event Sdl.Event.mouse_motion_state)
              Sdl.Button.lmask
            <> 0l
          in
          if not dragging then ()
          else begin
            let s = 1.0 /. !cam.Higl.Camera2d.zoom in
            let dx =
              float_of_int (Sdl.Event.get event Sdl.Event.mouse_motion_xrel)
              *. s
            in
            let dy =
              float_of_int
                (Sdl.Event.get event Sdl.Event.mouse_motion_yrel)
              *. (-.s)
            in
            let d =
              Gg.V2.ltr (Gg.M2.rot2 (-. (!cam.Higl.Camera2d.rotation)))
                (Gg.V2.v dx dy)
            in
            cam :=
              { !cam with
                Higl.Camera2d.target =
                  Gg.V2.sub !cam.Higl.Camera2d.target d }
          end)
      | _ -> ()
    done;

    Higl.Renderer.set_camera2d r !cam;
    Higl.Renderer.clear_screen r (Higl.color 0.08 0.08 0.10 1.0);

    (* Static scene, drawn under the camera every frame. *)
    Higl.Renderer.flush r ~mesh:scene;

    (* Spinning sprite on the streaming default mesh. *)
    spin := !spin +. 0.02;
    Higl.Renderer.queue_texture r ~rotation:!spin tex
      (Higl.rect (-60.0) 200.0 120.0 120.0);
    Higl.Renderer.flush r;
    Higl.Mesh.clear (Higl.Renderer.default_mesh r);

    Sdl.gl_swap_window win
  done;
  Higl.Texture.delete tex;
  Sdl.quit ()

let () = main ()
