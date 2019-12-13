(** Deal with Planet's project *)

open Bedrock
open Baremetal

val database : Shapes.Project.t Database.t
(** Get the database *)

val read :
     Shapes.Context.Projects.t
  -> File.name
  -> ( Shapes.Project.t
     * Paperwork.Timetable.Day.t option
     * Shapes.Context.Projects.context option
     )
     Validation.t
     * File.name
(** Read a project from a file *)

val inspect :
     ?rctx:Shapes.Context.t
  -> unit
  -> ( Shapes.Context.Projects.t
     * ( ( Shapes.Project.t
         * Paperwork.Timetable.Day.t option
         * Shapes.Context.Projects.context option
         )
         Validation.t
       * File.name
       )
       list
     )
     Validation.t
(** Get a list of potential projects *)

val all :
     ?rctx:Shapes.Context.t
  -> unit
  -> ( Shapes.Context.Projects.t
     * ( Shapes.Project.t
       * Paperwork.Timetable.Day.t option
       * Shapes.Context.Projects.context option
       )
       list
     )
     Validation.t
(** Get list of project *)

val to_json : unit -> Paperwork.Json.t Validation.t
(** Get list of project as [Json.t] *)

val to_hakyll_string :
     Shapes.Project.t
     * Paperwork.Timetable.Day.t option
     * Shapes.Context.Projects.context option
  -> (Shapes.Project.t * File.name * string * string) Validation.t
(** Convert project to Hakyll file *)
