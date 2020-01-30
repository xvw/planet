open Bedrock
open Baremetal

val path : string
val path_of : Shapes.Gallery.kind -> string
val create : Shapes.Gallery.t -> Shapes.Gallery.t Validation.t
val update : Shapes.Gallery.t -> Shapes.Gallery.t Validation.t
val get : unit -> File.name list Validation.t
val read : File.name -> Shapes.Gallery.t Validation.t

val to_hakyll
  :  Shapes.Gallery.t
  -> (Shapes.Gallery.t * string * string * string) Validation.t
