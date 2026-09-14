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
- **Ortho cameras** — default camera maps 1 unit to 1 pixel; custom
  cameras map any axis-aligned world box to the viewport
- **Batched meshes** — primitives are cached CPU-side and re-uploaded
  only when dirty, so static geometry is cheap to redraw
- **Pixel maps (GLSL ORM)** — describe fragment shaders with typed
  OCaml combinators instead of raw GLSL strings; neighborhood
  operations (`avg (nbhd self)`) compile to unrolled texel fetches

## Installation

higl depends on [tgls](https://opam.ocaml.org/packages/tgls/) for its
OpenGL 4 core bindings. The bundled example additionally uses
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

A complete runnable program — window setup, event loop, static and
streaming meshes — is in [`examples/triangle.ml`](examples/triangle.ml).
Examples live in their own dune project so that tsdl stays an optional
dependency. Build and run them with:

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
`Higl.Mesh`, `Higl.Mat4`, `Higl.Glsl`, `Higl.Pixelmap`, and the
geometry types re-exported from `Higl` itself.

## Testing

```sh
dune runtest
```
