open Bedrock
open Baremetal
open Paperwork

val database : Shapes.Task.t Database.t
val files : unit -> File.name list Result.t
val fresh_id : unit -> string Result.t
val tasks : unit -> Shapes.Task.t list Validation.t

val init
  :  string option
  -> string list
  -> string
  -> string
  -> string list
  -> string list
  -> Timetable.Day.t option
  -> Shapes.Task.t Result.t
