# higl

A high-level 2D drawing library for OCaml on top of OpenGL (3.3 core).

higl wraps raw OpenGL with a small, friendly API: you describe colored
triangles, lines, and points, and a batched renderer turns them into
draw calls. GPU state (VAO/VBO, shader program, blending) is owned by
the renderer; your code just queues primitives and flushes once per
frame.

- **Simple queue/flush API** — queue primitives, call `flush` each frame
- **Per-vertex colors** — colors interpolate across primitives
  (GL 2.x-style smooth shading), with alpha blending enabled
- **Sprites/textures** — upload RGBA8 pixels, draw textured quads with
  source rectangles, rotation, pivot and tint (raylib's DrawTexturePro,
  retained); sprites and colored primitives interleave freely in one
  mesh, batched per texture run
- **Shapes** — rects, circles, ellipses, regular polygons and polylines
  as pure constructors that bake into meshes (retained mode: no
  immediate drawing)
- **Ortho cameras** — default camera maps 1 unit to 1 pixel; a box
  camera maps any axis-aligned world box to the viewport; a
  `Camera2d` (pan/zoom/rotation, raylib-style) with world↔screen
  conversions
- **Batched meshes** — primitives are cached CPU-side and re-uploaded
  only when dirty, so static geometry is cheap to redraw
- **Pixel maps (GLSL ORM)** — describe fragment shaders with typed
  OCaml combinators instead of raw GLSL strings; neighborhood
  operations (`avg (nbhd self)`) compile to unrolled texel fetches
- **Bitmap font** — an embedded public-domain 8x8 monospace font
  (`Higl.Font`) baked into a texture atlas; text draws as batched
  sprites with scale, aspect squeeze and tint
- **3D-friendly** — `Mat4.perspective`/`Mat4.look_at` plus
  `Renderer.set_camera_matrix` let a full projection*view matrix drive
  the same primitive pipeline, and `Renderer.create ~depth:true`
  enables the depth test

## Installation

higl depends on [tgls](https://opam.ocaml.org/packages/tgls/) for its
OpenGL 4 core bindings and [gg](https://opam.ocaml.org/packages/gg/)
for 2D vector math. The bundled examples additionally use
[tsdl](https://opam.ocaml.org/packages/tsdl/) for windowing.

```sh
opam install tgls
git clone <repo-url> && cd higl
dune build
```

## Usage

higl does not create windows or GL contexts — you bring your own (e.g.
SDL2 via tsdl, GLFW, ...). Once a 3.3+ context is current:

```ocaml
(* One-time setup, after the GL context is current. *)
let r = Higl.Renderer.create () in
Higl.Renderer.set_viewport r ~width ~height;

(* Each frame: *)
Higl.Renderer.clear_screen r (Higl.color 0.08 0.08 0.10 1.0);
Higl.Renderer.queue r
  (Higl.Triangle
     (Higl.tri (Higl.point 320.0 100.0 0.0)
        (Higl.point 100.0 380.0 0.0)
        (Higl.point 540.0 380.0 0.0)));
Higl.Renderer.flush r
```

Note that `flush` is non-destructive: the renderer's mesh keeps its
primitives between frames. Clear it explicitly when you want fresh
geometry:

```ocaml
Higl.Mesh.clear (Higl.Renderer.default_mesh r)
```

## Examples

### A gradient triangle (static mesh)

Build a mesh once, flush it every frame. Per-vertex colors interpolate
across the triangle.

```ocaml
let mesh = Higl.Mesh.create () in
let red   = Higl.vertex (Higl.point 320.0 100.0 0.0) (Higl.color 1.0 0.0 0.0 1.0) in
let green = Higl.vertex (Higl.point 100.0 380.0 0.0) (Higl.color 0.0 1.0 0.0 1.0) in
let blue  = Higl.vertex (Higl.point 540.0 380.0 0.0) (Higl.color 0.0 0.0 1.0 1.0) in
Higl.Mesh.add_triangle mesh { Higl.v1 = red; v2 = green; v3 = blue };

(* ... each frame: *)
Higl.Renderer.flush r ~mesh
```

### Lines and points

```ocaml
(* A 3-unit-wide gradient line, in world units. *)
Higl.Renderer.queue r
  (Higl.Line
     (Higl.line ~width:3.0
        (Higl.vertex (Higl.point 50.0 50.0 0.0) (Higl.color 1.0 1.0 0.0 1.0))
        (Higl.vertex (Higl.point 590.0 50.0 0.0) (Higl.color 1.0 0.0 1.0 1.0))));

(* A 6x6 square point. *)
Higl.Renderer.queue r
  (Higl.Point
     (Higl.point_prim ~size:6.0
        (Higl.vertex (Higl.point 320.0 30.0 0.0) (Higl.color 0.0 1.0 1.0 1.0))))
```

### Streaming vs. static geometry

Use the renderer's own mesh for per-frame ("streaming") geometry, and
separate `Mesh.t` values for geometry that rarely changes — dirty meshes
are only re-expanded and re-uploaded when modified.

```ocaml
(* Static: built once. *)
let static = Higl.Mesh.create ~capacity:4096 () in
List.iter (Higl.Mesh.add_prim static) scene_geometry;

(* Each frame: rebuild streaming geometry, draw everything. *)
Higl.Renderer.clear_screen r background;
Higl.Renderer.flush r ~mesh:static;
Higl.Mesh.clear (Higl.Renderer.default_mesh r);
List.iter (Higl.Renderer.queue r) particles;
Higl.Renderer.flush r
```

### A custom ortho camera

The default camera maps `(0,0)` to the bottom-left of the window with
1 unit = 1 pixel. Any axis-aligned world box works:

```ocaml
let camera =
  Higl.Renderer.{ left = -10.0; right = 10.0
                ; bottom = -5.0; top = 5.0
                ; near = -1.0; far = 1.0 }
in
let r = Higl.Renderer.create ~camera () in
(* ... or swap it later: *)
Higl.Renderer.set_camera r camera
```

### Solid-color triangles

`Higl.tri` tints all three vertices at once; `Higl.tri_solid` repaints an
existing triangle:

```ocaml
let t =
  Higl.tri ~color:(Higl.color 0.9 0.3 0.2 1.0)
    (Higl.point 0.0 0.0 0.0) (Higl.point 4.0 0.0 0.0) (Higl.point 0.0 3.0 0.0)
in
let blue_t = Higl.tri_solid (Higl.color 0.2 0.4 0.9 1.0) t in
Higl.Mesh.add_triangle mesh blue_t
```

### Sprites: textures, source rects, tint

Upload RGBA8 pixels (top-left first; rows are flipped for you) and add
sprites — the retained counterpart of raylib's `DrawTexturePro`. Source
rectangles are in pixel coordinates from the top-left; destination
rects live in world space (y up), and sprites rotate about a pivot in
CCW radians.

```ocaml
let tex = Higl.Texture.create ~width:16 ~height:16 pixels in
let full = Higl.sprite tex (Higl.rect 40.0 40.0 64.0 64.0) in
let half =
  (* left half of the texture, tinted red, rotated 90 degrees *)
  Higl.sprite ~src:(Higl.rect 0.0 0.0 8.0 16.0)
    ~rotation:(Float.pi /. 2.0)
    ~tint:(Higl.color 1.0 0.3 0.3 1.0)
    tex (Higl.rect 120.0 40.0 64.0 64.0)
in
Higl.Mesh.add_sprite mesh full;
Higl.Mesh.add_sprite mesh half;

(* Streaming equivalent on the renderer's default mesh: *)
Higl.Renderer.queue_texture r tex (Higl.rect 200.0 40.0 32.0 32.0)
```

Untextured primitives sample a 1x1 white texture, so sprites and colored
geometry mix freely in one mesh; each contiguous run of same-texture
primitives becomes one draw call, in paint order.

### Shapes

`Higl.Shape` constructors decompose shapes into plain triangles and
lines — nothing is drawn at construction time. The `Mesh.add_*`
wrappers bake them into a mesh; static shapes are re-uploaded only
when the mesh changes.

```ocaml
Higl.Mesh.add_circle mesh ~color:(Higl.color 0.9 0.4 0.2 1.0)
  (Gg.V2.v 0.0 0.0) 180.0;
Higl.Mesh.add_polygon mesh ~sides:6 (Gg.V2.v 420.0 120.0) 70.0;
Higl.Mesh.add_circle_outline mesh ~thickness:3.0 (Gg.V2.v (-320.0) 120.0) 60.0;
Higl.Mesh.add_polyline mesh
  [ Gg.V2.v 0.0 0.0; Gg.V2.v 40.0 30.0; Gg.V2.v 80.0 10.0 ]
```

### A 2D camera (pan/zoom/rotation)

`Higl.Camera2d` is raylib's Camera2D: the world point `target` appears
at the screen point `offset`, with `zoom` and CCW `rotation`. The
screen origin is the bottom-left of the viewport.

```ocaml
let cam =
  Higl.Camera2d.create
    ~target:(Gg.V2.v 100.0 100.0)
    ~offset:(Gg.V2.v 320.0 240.0)
    ~zoom:2.0 ()
in
Higl.Renderer.set_camera2d r cam;

(* Pixel-perfect picking: *)
let world = Higl.Camera2d.screen_to_world cam (Gg.V2.v 100.0 50.0)
```

A complete runnable program — window setup, event loop, static and
streaming meshes — is in [`examples/triangle.ml`](examples/triangle.ml);
a sprites + shapes + Camera2d tour (drag to pan, wheel to zoom) is in
[`examples/sprites.ml`](examples/sprites.ml). Examples live in their
own dune project so that tsdl stays an optional dependency. Build and
run them with:

```sh
# from the repo root (installs higl into your switch):
opam install .
# from examples/:
dune build --root . ./triangle.exe
./_build/default/triangle.exe
```

## Pixel maps: a GLSL ORM

`Higl.Glsl` is a typed expression language for fragment programs — you
compose OCaml values instead of writing GLSL strings, and there are no
loops in the interface: neighborhood operations expand to unrolled
texel fetches in the generated shader. `Higl.Pixelmap` is the runner
that makes the feedback real: a ping-ponged texture pair, rendered to
each `step`, that your program reads as `self`.

```ocaml
(* A program: one line of OCaml per pixel. *)
let prog =
  let open Higl.Glsl in
  program (fun color_frag ->
    let smooth = avg (nbhd color_frag) in         (* 8 Moore neighbors *)
    let coarse = avg (stride 2 (nbhd color_frag)) in  (* every 2nd texel *)
    mix color_frag (min smooth coarse) (f 0.5))
```

A `program`'s argument is the previous frame's color at this pixel, so
`color_frag = avg (nbhd color_frag)` is the box blur you'd expect.
Intermediates are plain OCaml `let`s; the compiler hoists repeated
subexpressions into GLSL locals automatically. Neighborhoods
transform (`stride`, `radius`, `cross`, `include_center`) and reduce
(`sum`, `avg`, `min_of`, `max_of`, `count_nonzero`, `weighted`
convolution with kernels like `gaussian3`), plus the generic
`map_reduce`. The usual math builtins (`mix`, `clamp`, `smoothstep`,
`dot`, `length`, ...) and swizzles are available, with `time`, `res`,
`mouse`, and `uv` as inputs.

Running it each frame:

```ocaml
let buf = Higl.Pixelmap.create ~width ~height () in
Higl.Pixelmap.clear buf (Higl.color 0.0 0.0 0.0 1.0);

(* each frame: *)
Higl.Pixelmap.set_time buf seconds;
Higl.Pixelmap.set_mouse buf ~x ~y;      (* normalized, bottom-left origin *)
Higl.Pixelmap.step buf prog;             (* buf <- prog (buf) *)
Higl.Pixelmap.draw buf                   (* blit to the screen *)
```

Buffers are RGBA8, so stored colors live in `0..1`. A complete demo
(mouse-painted, blurring, fading trails) is
[`examples/pixelmap.ml`](examples/pixelmap.ml):

```sh
dune build --root . ./pixelmap.exe   # from examples/
./_build/default/pixelmap.exe
```

Use `Higl.Glsl.fragment_source` to inspect the generated shader — handy
for debugging.

## Documentation

Generate the API reference with [odoc](https://odoc.docs.ocaml.org/):

```sh
dune build @doc
```

Then open `_build/default/_doc/_html/higl/index.html` in a browser. The
entry point is the [`Higl`](lib/higl.ml) module; see `Higl.Renderer`,
`Higl.Mesh`, `Higl.Texture`, `Higl.Shape`, `Higl.Camera2d`, `Higl.Mat4`,
`Higl.Glsl`, `Higl.Pixelmap`, and the geometry types re-exported from
`Higl` itself.

## Testing

```sh
dune runtest
```
