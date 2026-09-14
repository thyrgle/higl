(* Glsl: a typed GLSL expression DSL for fragment programs over a feedback
   pixel buffer ("GLSL ORM").

   You describe one pixel's new color as an OCaml expression; [program]
   compiles it to a GLSL fragment shader. There are no loops in the
   OCaml interface: neighborhoods reduce to unrolled sequences of texel
   fetches in the generated shader.

   The generated shader's contract (see [fragment_source]):

   - [in vec2 v_uv]         — this pixel's texture coordinate (0..1)
   - [uniform sampler2D u_prev] — the previous frame's buffer
   - [uniform vec2 u_resolution] — buffer size in texels
   - [uniform vec2 u_mouse] — mouse position, normalized to 0..1
   - [uniform float u_time] — seconds since the buffer was created
   - [out vec4 frag_color]  — the program's output

   [self] is the previous frame at the current pixel; [nbhd] samples the
   previous frame at neighboring texels. Higl.Pixelmap owns the render-
   to-texture plumbing that makes the feedback real. *)

(** Phantom types marking a GLSL value's type: [f] float, [i] int,
    [b] bool, [v2]/[v3]/[v4] vectors. *)
type f
type i
type b
type v2
type v3
type v4

(** Neighborhood shape: {!Square} is the full [2r+1 x 2r+1] block around
    the pixel; {!Cross} keeps only the axis-aligned neighbors. *)
type shape = Square | Cross

(** A convolution kernel (square, row-major). See [weighted]. *)
type kernel = float array array

(* --- AST (internal) --- *)

type ty = TyF | TyI | TyB | TyV2 | TyV3 | TyV4

type raw =
  | RF of float
  | RI of int
  | RB of bool
  | RVar of string * ty
  | RSwiz of raw * string
  | RBin of string * raw * raw
  | RRel of string * raw * raw
  | RLogic of string * raw * raw
  | RUn of string * raw
  | RCall of string * raw array
  | RTex of int * int  (* sample of u_prev at texel offset (dx, dy) *)
  | RIf of raw * raw * raw

(** A GLSL expression of phantom type ['a]. *)
type 'a expr = E of raw

(** A set of texel offsets around the current pixel. *)
type neighborhood = { n_shape : shape; n_radius : int; n_stride : int; n_center : bool }

(** A fragment program: compiled to GLSL on first use. *)
type program = { body : raw }

let rec ty_of : raw -> ty = function
  | RF _ -> TyF
  | RI _ -> TyI
  | RB _ -> TyB
  | RVar (_, ty) -> ty
  | RSwiz (_, s) ->
      (match String.length s with
       | 1 -> TyF | 2 -> TyV2 | 3 -> TyV3 | 4 -> TyV4
       | n -> invalid_arg (Printf.sprintf "Glsl: bad swizzle length %d" n))
  | RBin (_, a, b) -> promote (ty_of a) (ty_of b)
  | RRel _ | RLogic _ -> TyB
  | RUn (op, r) -> if op = "!" then TyB else ty_of r
  | RCall (name, args) ->
      if Array.length args = 0 then invalid_arg "Glsl: call with no arguments";
      (match name with "dot" | "length" -> TyF | _ -> ty_of args.(0))
  | RTex _ -> TyV4
  | RIf (_, a, _) -> ty_of a

and promote a b =
  match a, b with
  | TyF, t | t, TyF -> t
  | a', b' when a' = b' -> a'
  | _ -> invalid_arg "Glsl: operands have incompatible types"

let e : raw -> 'a expr = fun raw -> E raw
let er : 'a expr -> raw = fun (E r) -> r
let un : string -> 'a expr -> 'b expr = fun op (E r) -> E (RUn (op, r))
let bin : string -> 'a expr -> 'a expr -> 'b expr = fun op (E a) (E b) ->
  E (RBin (op, a, b))
let rel : string -> 'a expr -> 'a expr -> b expr = fun op (E a) (E b) ->
  E (RRel (op, a, b))
let logic : string -> 'a expr -> 'a expr -> b expr = fun op (E a) (E b) ->
  E (RLogic (op, a, b))
(* [call] takes pre-unwrapped raws: a heterogeneous argument list (as in
   [mix], whose blend factor is always a float) must not share one list
   type variable, or the wrapper's phantom types collapse together. *)
let call : string -> raw list -> 'a expr = fun name args ->
  E (RCall (name, Array.of_list args))

(* --- Literals and constructors --- *)

(** [f x] is the float literal [x]. *)
let f : float -> f expr = fun x -> e (RF x)

(** [int_ n] is the int literal [n]. *)
let int_ : int -> i expr = fun n -> e (RI n)

(** [bool_ v] is the bool literal [v]. *)
let bool_ : bool -> b expr = fun v -> e (RB v)

(** [v2 x y] is [vec2 (x, y)]. *)
let v2 : f expr -> f expr -> v2 expr = fun x y -> call "vec2" [ er x; er y ]

(** [v3 x y z] is [vec3 (x, y, z)]. *)
let v3 : f expr -> f expr -> f expr -> v3 expr = fun x y z ->
  call "vec3" [ er x; er y; er z ]

(** [v4 x y z w] is [vec4 (x, y, z, w)]. *)
let v4 : f expr -> f expr -> f expr -> f expr -> v4 expr = fun x y z w ->
  call "vec4" [ er x; er y; er z; er w ]

(* --- Built-in inputs --- *)

(** This pixel's texture coordinate ([v_uv]), 0..1. *)
let uv : v2 expr = e (RVar ("v_uv", TyV2))

(** Seconds since the pixel buffer was created ([u_time]). *)
let time : f expr = e (RVar ("u_time", TyF))

(** Buffer size in texels ([u_resolution]). *)
let res : v2 expr = e (RVar ("u_resolution", TyV2))

(** Mouse position normalized to 0..1, origin at the bottom-left
    ([u_mouse]). *)
let mouse : v2 expr = e (RVar ("u_mouse", TyV2))

(** The previous frame's value at the current pixel: in
    [program (fun color_frag -> ...)], this is what [color_frag] means. *)
let self : v4 expr = e (RTex (0, 0))

(** [peek ~dx ~dy] is the previous frame's value at the texel [(dx, dy)]
    away from the current pixel. *)
let peek : dx:int -> dy:int -> v4 expr = fun ~dx ~dy -> e (RTex (dx, dy))

(* --- Operators --- *)

let ( +. ) : 'a expr -> 'a expr -> 'a expr = fun a b -> bin "+" a b
let ( -. ) : 'a expr -> 'a expr -> 'a expr = fun a b -> bin "-" a b
let ( *. ) : 'a expr -> 'a expr -> 'a expr = fun a b -> bin "*" a b
let ( /. ) : 'a expr -> 'a expr -> 'a expr = fun a b -> bin "/" a b
let ( ~- ) : 'a expr -> 'a expr = fun r -> un "-" r

(** [scale k v] multiplies a scalar with a value of any numeric type
    (GLSL's overloaded [vec * float]). *)
let scale : f expr -> 'a expr -> 'a expr = fun (E k) (E v) -> e (RBin ("*", k, v))

let ( <. ) : 'a expr -> 'a expr -> b expr = fun a b -> rel "<" a b
let ( >. ) : 'a expr -> 'a expr -> b expr = fun a b -> rel ">" a b
let ( <=. ) : 'a expr -> 'a expr -> b expr = fun a b -> rel "<=" a b
let ( >=. ) : 'a expr -> 'a expr -> b expr = fun a b -> rel ">=" a b
let ( ==. ) : 'a expr -> 'a expr -> b expr = fun a b -> rel "==" a b
let ( !=. ) : 'a expr -> 'a expr -> b expr = fun a b -> rel "!=" a b
let ( &&. ) : b expr -> b expr -> b expr = fun a b -> logic "&&" a b
let ( ||. ) : b expr -> b expr -> b expr = fun a b -> logic "||" a b

(** [not_ b] negates a bool expression. *)
let not_ : b expr -> b expr = fun r -> un "!" r

(** [if_ c a b] is GLSL's [c ? a : b]. *)
let if_ : b expr -> 'a expr -> 'a expr -> 'a expr = fun (E c) (E a) (E b) ->
  e (RIf (c, a, b))

(* --- Built-in functions --- *)

let sin : 'a expr -> 'a expr = fun r -> call "sin" [ er r ]
let cos : 'a expr -> 'a expr = fun r -> call "cos" [ er r ]
let tan : 'a expr -> 'a expr = fun r -> call "tan" [ er r ]
let exp : 'a expr -> 'a expr = fun r -> call "exp" [ er r ]
let sqrt : 'a expr -> 'a expr = fun r -> call "sqrt" [ er r ]
let abs : 'a expr -> 'a expr = fun r -> call "abs" [ er r ]
let floor : 'a expr -> 'a expr = fun r -> call "floor" [ er r ]
let fract : 'a expr -> 'a expr = fun r -> call "fract" [ er r ]
let sign : 'a expr -> 'a expr = fun r -> call "sign" [ er r ]
let pow : 'a expr -> 'a expr -> 'a expr = fun a b -> call "pow" [ er a; er b ]
let min : 'a expr -> 'a expr -> 'a expr = fun a b -> call "min" [ er a; er b ]
let max : 'a expr -> 'a expr -> 'a expr = fun a b -> call "max" [ er a; er b ]

(** [clamp x lo hi] clamps component-wise with scalar bounds. *)
let clamp : 'a expr -> f expr -> f expr -> 'a expr = fun x lo hi ->
  call "clamp" [ er x; er lo; er hi ]

(** [mix a b t] is the linear interpolation [a * (1 - t) + b * t], [t] a
    scalar. *)
let mix : 'a expr -> 'a expr -> f expr -> 'a expr = fun a b t ->
  call "mix" [ er a; er b; er t ]

let smoothstep : 'a expr -> 'a expr -> 'a expr -> 'a expr =
  fun e0 e1 x -> call "smoothstep" [ er e0; er e1; er x ]
let step : 'a expr -> 'a expr -> 'a expr = fun edge x ->
  call "step" [ er edge; er x ]
let dot : 'a expr -> 'a expr -> f expr = fun a b -> call "dot" [ er a; er b ]
let length : 'a expr -> f expr = fun r -> call "length" [ er r ]
let normalize : 'a expr -> 'a expr = fun r -> call "normalize" [ er r ]

(* --- Swizzles --- *)

(** [swiz s e] is GLSL's [e.s] (e.g. [swiz "zy" v]). Unchecked: [s] must
    be a valid GLSL swizzle for [e]'s type; its length sets the result
    type (1 = float, 2/3/4 = vec2/3/4). *)
let swiz : string -> 'a expr -> 'b expr = fun s (E r) -> e (RSwiz (r, s))

let x : 'a expr -> f expr = fun r -> swiz "x" r
let y : 'a expr -> f expr = fun r -> swiz "y" r
let z : 'a expr -> f expr = fun r -> swiz "z" r
let w : 'a expr -> f expr = fun r -> swiz "w" r
let xy : 'a expr -> v2 expr = fun r -> swiz "xy" r
let xyz : 'a expr -> v3 expr = fun r -> swiz "xyz" r
let rgb : 'a expr -> v3 expr = fun r -> swiz "rgb" r

(* --- Neighborhoods --- *)

(** [nbhd ?radius ?stride ?shape ?center v] is the neighborhood around
    the pixel [v] belongs to (pass [self]; [v] is only for readability,
    samples always come from the previous frame). Defaults: [radius =
    1], [stride = 1], [shape = Square] (the 8 Moore neighbors),
    [center = false]. *)
let nbhd :
  ?radius:int -> ?stride:int -> ?shape:shape -> ?center:bool ->
  'a expr -> neighborhood =
  fun ?(radius = 1) ?(stride = 1) ?(shape = Square) ?(center = false) _v ->
  if radius < 1 then invalid_arg "Glsl.nbhd: radius must be >= 1";
  if stride < 1 then invalid_arg "Glsl.nbhd: stride must be >= 1";
  { n_shape = shape; n_radius = radius; n_stride = stride; n_center = center }

(** [stride k nb] samples every [k]-th texel instead of every texel
    (multiplies [nb]'s offsets). *)
let stride : int -> neighborhood -> neighborhood = fun k nb ->
  if k < 1 then invalid_arg "Glsl.stride: must be >= 1";
  { nb with n_stride = nb.n_stride * k }

(** [radius r nb] resizes [nb] to radius [r]. *)
let radius : int -> neighborhood -> neighborhood = fun r nb ->
  if r < 1 then invalid_arg "Glsl.radius: must be >= 1" else
  { nb with n_radius = r }

let set_shape : shape -> neighborhood -> neighborhood = fun shape nb ->
  { nb with n_shape = shape }

(** [cross nb] keeps only the axis-aligned neighbors (von Neumann). *)
let cross : neighborhood -> neighborhood = fun nb -> { nb with n_shape = Cross }

(** [square nb] keeps the full square of neighbors (Moore). *)
let square : neighborhood -> neighborhood = fun nb -> { nb with n_shape = Square }

(** [include_center nb] adds the pixel itself to [nb]. *)
let include_center : neighborhood -> neighborhood = fun nb ->
  { nb with n_center = true }

(** [exclude_center nb] removes the pixel itself from [nb]. *)
let exclude_center : neighborhood -> neighborhood = fun nb ->
  { nb with n_center = false }

(** [offsets nb] are [nb]'s texel offsets, sorted by [dy] then [dx]. *)
let offsets : neighborhood -> (int * int) list = fun { n_shape; n_radius; n_stride; n_center } ->
  let range = List.init ((2 * n_radius) + 1) (fun i -> i - n_radius) in
  let offs =
    List.concat_map
      (fun j ->
        List.filter_map
          (fun i ->
            if (i, j) = (0, 0) then if n_center then Some (0, 0) else None
            else
              match n_shape with
              | Square -> Some (i * n_stride, j * n_stride)
              | Cross when i = 0 || j = 0 -> Some (i * n_stride, j * n_stride)
              | Cross -> None)
          range)
      range
  in
  List.sort (fun (ax, ay) (bx, by) -> compare (ay, ax) (by, bx)) offs

(** [sample (dx, dy)] is the previous frame at the texel [(dx, dy)] from
    the current pixel. *)
let sample : int * int -> v4 expr = fun (dx, dy) -> e (RTex (dx, dy))

(** [map_reduce map op nb] folds [op] over [map] applied to each of
    [nb]'s neighbor samples, in offset order. The generic neighborhood
    combinator: [sum], [avg], [min_of], ... are special cases. *)
let map_reduce :
  (v4 expr -> 'a expr) -> ('a expr -> 'a expr -> 'a expr) ->
  neighborhood -> 'a expr =
  fun map op nb ->
  match offsets nb with
  | [] -> invalid_arg "Glsl.map_reduce: empty neighborhood"
  | first :: rest ->
      List.fold_left
        (fun acc off -> op acc (map (sample off)))
        (map (sample first))
        rest

(** [reduce op nb] folds [op] over the raw neighbor samples. *)
let reduce : ('a expr -> 'a expr -> 'a expr) -> neighborhood -> 'a expr =
  fun op nb -> map_reduce (fun s -> s) op nb

(** [sum nb] is the component-wise sum of the neighbor samples. *)
let sum : neighborhood -> v4 expr = fun nb -> reduce ( +. ) nb

(** [avg nb] is the component-wise mean of the neighbor samples: the
    box blur [avg (nbhd self)]. *)
let avg : neighborhood -> v4 expr = fun nb ->
  let n = float_of_int (List.length (offsets nb)) in
  scale (f (Float.div 1.0 n)) (sum nb)

(** [min_of nb] is the component-wise minimum over the neighbors. *)
let min_of : neighborhood -> v4 expr = fun nb -> reduce min nb

(** [max_of nb] is the component-wise maximum over the neighbors. *)
let max_of : neighborhood -> v4 expr = fun nb -> reduce max nb

(** [count_nonzero nb] is how many neighbors are not (almost) black:
    the life-like-CA head count when "alive" means any channel lit. *)
let count_nonzero : neighborhood -> f expr = fun nb ->
  map_reduce
    (fun s -> if_ (length s >. f 0.001) (f 1.0) (f 0.0))
    ( +. )
    nb

(** [kernel rows] checks and returns a convolution kernel: non-empty,
    square, with odd side length (so it has a center). *)
let kernel : float array array -> kernel = fun k ->
  let rows = Array.length k in
  if rows = 0 || Array.exists (fun r -> Array.length r <> rows) k then
    invalid_arg "Glsl.kernel: must be a non-empty square matrix";
  if rows mod 2 = 0 then invalid_arg "Glsl.kernel: must have odd side length";
  k

(** Normalized 3x3 Gaussian blur kernel (sums to 1). *)
let gaussian3 : kernel =
  [| [| 0.0625; 0.125; 0.0625 |]
   ; [| 0.125; 0.25; 0.125 |]
   ; [| 0.0625; 0.125; 0.0625 |] |]

(** Normalized 3x3 box blur kernel (sums to 1). *)
let box3 : kernel =
  let w = Float.div 1.0 9.0 in
  [| [| w; w; w |]; [| w; w; w |]; [| w; w; w |] |]

(** [weighted k nb] convolves [nb]'s samples with the square kernel
    [k]: the sum of [scale w (sample off)] over [nb]'s offsets. [k]'s
    side must be [2 * radius * stride + 1]; entry [k.(row).(col)]
    weights the offset [(col - half, row - half)]. *)
let weighted : kernel -> neighborhood -> v4 expr = fun (k : kernel) nb ->
  let { n_radius; n_stride; _ } = nb in
  let side = (2 * n_radius * n_stride) + 1 in
  let rows = Array.length k in
  if rows <> side then
    invalid_arg
      (Printf.sprintf "Glsl.weighted: kernel side %d but neighborhood needs %d"
         rows side);
  let half = side / 2 in
  List.fold_left
    (fun acc (dx, dy) ->
      let w = k.(dy + half).(dx + half) in
      acc +. scale (f w) (sample (dx, dy)))
    (e (RF 0.0))
    (offsets nb)

(* --- Programs --- *)

(** [program body] is the fragment program assigning [frag_color] at
    each pixel to [body self], where [self] is the previous frame's
    color there. Intermediates are ordinary OCaml [let]s; identical
    subexpressions used more than once are hoisted into named GLSL
    locals, so [let s = avg (nbhd self) in ...] costs one chain of
    fetches however many times [s] appears. *)
let program : (v4 expr -> v4 expr) -> program = fun body ->
  { body = (match body self with E r -> r) }

(* --- GLSL emission --- *)

let ty_name = function
  | TyF -> "float" | TyI -> "int" | TyB -> "bool"
  | TyV2 -> "vec2" | TyV3 -> "vec3" | TyV4 -> "vec4"

let float_lit x =
  let s = Printf.sprintf "%.7g" x in
  if String.contains s '.' || String.contains s 'e' || String.contains s 'n'
  then s
  else s ^ ".0"

let rec render : raw -> string = fun r ->
  match r with
  | RF x -> float_lit x
  | RI n -> string_of_int n
  | RB v -> if v then "true" else "false"
  | RVar (name, _) -> name
  | RSwiz (a, s) -> Printf.sprintf "(%s).%s" (render a) s
  | RBin (op, a, b) -> Printf.sprintf "((%s) %s (%s))" (render a) op (render b)
  | RRel (op, a, b) -> Printf.sprintf "((%s) %s (%s))" (render a) op (render b)
  | RLogic (op, a, b) -> Printf.sprintf "((%s) %s (%s))" (render a) op (render b)
  | RUn (op, a) -> Printf.sprintf "(%s (%s))" op (render a)
  | RCall (name, args) ->
      Printf.sprintf "%s(%s)" name
        (String.concat ", " (Array.to_list (Array.map render args)))
  | RTex (0, 0) -> "texture(u_prev, v_uv)"
  | RTex (dx, dy) ->
      Printf.sprintf "texture(u_prev, v_uv + vec2(%s, %s) * px)"
        (float_lit (float_of_int dx)) (float_lit (float_of_int dy))
  | RIf (c, a, b) ->
      Printf.sprintf "((%s) ? (%s) : (%s))" (render c) (render a) (render b)

let children : raw -> raw list = function
  | RF _ | RI _ | RB _ | RVar _ | RTex _ -> []
  | RSwiz (a, _) | RUn (_, a) -> [ a ]
  | RBin (_, a, b) | RRel (_, a, b) | RLogic (_, a, b) -> [ a; b ]
  | RCall (_, args) -> Array.to_list args
  | RIf (c, a, b) -> [ c; a; b ]

(* Trivial nodes never pay off as hoisted locals; a texture fetch does
   (sampling [self] twice should fetch once). *)
let hoist_worthy = function
  | RF _ | RI _ | RB _ | RVar _ -> false
  | _ -> true

let uses_px : raw -> bool = fun r ->
  let rec go = function
    | [] -> false
    | r :: rest ->
        (match r with
         | RTex (_, _) -> true
         | _ -> go (children r @ rest))
  in
  go [ r ]

(** [fragment_source prog] is [prog]'s fragment shader source (for
    debugging; Higl.Pixelmap compiles it for you). Common
    subexpressions referenced more than once become locals
    [T0, T1, ...] declared at the top of [main]. *)
let fragment_source : program -> string = fun { body } ->
  let counts : (string, int) Hashtbl.t = Hashtbl.create 64 in
  let rec visit r =
    let k = render r in
    match Hashtbl.find_opt counts k with
    | Some n -> Hashtbl.replace counts k (n + 1)
    | None ->
        List.iter visit (children r);
        Hashtbl.add counts k 1
  in
  visit body;
  let subst : (string, string) Hashtbl.t = Hashtbl.create 16 in
  let decls = Buffer.create 256 in
  let counter = ref 0 in
  let hoistable r k =
    hoist_worthy r
    && String.length k >= 12
    && (try Hashtbl.find counts k >= 2 with Not_found -> false)
  in
  let rec emit r =
    let k = render r in
    if hoistable r k then
      match Hashtbl.find_opt subst k with
      | Some name -> name
      | None ->
          let name = Printf.sprintf "T%d" !counter in
          incr counter;
          let rhs = render_with r in
          Hashtbl.replace subst k name;
          Buffer.add_string decls
            (Printf.sprintf "  %s %s = %s;\n" (ty_name (ty_of r)) name rhs);
          name
    else render_with r
  and render_with r =
    match r with
    | RF x -> float_lit x
    | RI n -> string_of_int n
    | RB v -> if v then "true" else "false"
    | RVar (name, _) -> name
    | RSwiz (a, s) -> Printf.sprintf "(%s).%s" (emit a) s
    | RBin (op, a, b) -> Printf.sprintf "((%s) %s (%s))" (emit a) op (emit b)
    | RRel (op, a, b) -> Printf.sprintf "((%s) %s (%s))" (emit a) op (emit b)
    | RLogic (op, a, b) -> Printf.sprintf "((%s) %s (%s))" (emit a) op (emit b)
    | RUn (op, a) -> Printf.sprintf "(%s (%s))" op (emit a)
    | RCall (name, args) ->
        Printf.sprintf "%s(%s)" name
          (String.concat ", " (Array.to_list (Array.map emit args)))
    | RTex _ -> render r
    | RIf (c, a, b) ->
        Printf.sprintf "((%s) ? (%s) : (%s))" (emit c) (emit a) (emit b)
  in
  let final = emit body in
  let px = if uses_px body then "  vec2 px = 1.0 / u_resolution;\n" else "" in
  Printf.sprintf
    {|#version 330 core

in vec2 v_uv;

out vec4 frag_color;

uniform sampler2D u_prev;
uniform vec2 u_resolution;
uniform vec2 u_mouse;
uniform float u_time;

void main ()
{
%s%s  frag_color = %s;
}|}
    px (Buffer.contents decls) final
