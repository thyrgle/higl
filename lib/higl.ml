(* Main module of higl: geometry types plus entry-point modules.

   Usage: [Higl.Renderer.create], add primitives to a [Higl.Mesh.t],
   [Higl.Renderer.flush] every frame. *)

include Geom

module Mesh = Mesh
module Renderer = Renderer
module Mat4 = Mat4
