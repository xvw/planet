(** Deal with Planet's logs *)

open Bedrock
open Baremetal

(** Get the database *)
val database : Shapes.Log.t Database.t

val whereami_file : File.name
val log_pattern : File.name list

(** Create the bucket for logs *)
val create_file
  :  Paperwork.Timetable.Day.t
  -> (File.name * bool) Result.t

(** Create the bucket for whereami logs *)
val create_whereami_file : unit -> (File.name * bool) Result.t

(** Create update table for projects *)
val create_update_table_projects
  :  unit
  -> (File.name * bool) Result.t

(** Read all log for a bucket *)
val read_logs : string -> Shapes.Log.t list Validation.t

(** Read update table for projects *)
val read_project_updates : unit -> Shapes.Update_table.t Result.t

(** Push projects in file *)
val push_project_updates : Shapes.Update_table.t -> unit Result.t

(** Converts logs to [Json.t] *)
val logs_to_json : Shapes.Log.t list -> Paperwork.Json.t

(** Convert whereami location into [Json.t] *)
val whereami_to_json
  :  ?reverse:bool
  -> unit
  -> Paperwork.Json.t Validation.t

(** Fetch all logs in [Json.t] *)
val collect_all_log_in_json
  :  ?reverse:bool
  -> unit
  -> Paperwork.Json.t Validation.t

(** Fetch all logs using a reducer *)
val traverse
  :  ?reverse:bool
  -> ('a -> Shapes.Log.t -> 'a)
  -> 'a
  -> 'a Validation.t

val context : unit -> Shapes.Context.t Validation.t
