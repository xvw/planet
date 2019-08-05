(** Deal with Planet's project *)

open Bedrock
open Baremetal

(** Get the database *)
val database : Shapes.Project.t Database.t

(** Read a project from a file *)
val read
  :  Shapes.Update_table.t
  -> File.name
  -> (Shapes.Project.t * Paperwork.Timetable.Day.t option)
     Validation.t
     * File.name

(** Get a list of potential projects *)
val inspect
  :  unit
  -> ((Shapes.Project.t * Paperwork.Timetable.Day.t option)
      Validation.t
     * File.name)
     list
     Result.t

(** Get list of project *)
val all
  :  unit
  -> (Shapes.Project.t * Paperwork.Timetable.Day.t option) list
     Validation.t

(** Get list of project as [Json.t] *)
val to_json : unit -> Paperwork.Json.t Validation.t

(** Convert project to Hakyll file *)
val to_hakyll_string
  :  Shapes.Project.t * Paperwork.Timetable.Day.t option
  -> (Shapes.Project.t * File.name * string * string) Validation.t
