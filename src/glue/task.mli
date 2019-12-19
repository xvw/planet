open Bedrock
open Baremetal

val database : Shapes.Task.t Database.t
val files : unit -> File.name list Result.t
val fresh_id : unit -> string Result.t
val tasks : unit -> Shapes.Task.t list Validation.t
