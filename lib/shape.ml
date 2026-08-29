open Tgl4

type point =
{
  x: float;
  y: float;
  z: float;
}

type line =
{
  x1: float; y1: float; z1: float;
  x2: float; y2: float; z2: float;
}

type triangle =
{
  x1: float; y1: float; z1: float;
  x2: float; y2: float; z2: float;
  x3: float; y3: float; z3: float;
}

type primitive = Point of point | Line of line | Triangle of triangle

type geom_manager =
{
  shapes: primitive list;
}

(**
let draw (shapes: primitive list) =

let to_screen_buffer (manager: geom_manager) =
  Gl.clear_color 0. 0. 0. 1.;
  Gl.clear Gl.color_buffer_bit;
  Gl.use_program pid;
  Gl.bnd_vertex_array gid;
  draw manager.shapes;
  Gl.bind_vertex_array 0
*)
