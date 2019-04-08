(** Deal with Planet's logs *)

open Bedrock
open Baremetal

(** Get the database *)
val database : Shapes.Log.t Database.t

(** Create the bucket for logs *)
val create_file :
  Paperwork.Timetable.Day.t -> (File.name * bool) Result.t

(** Create the bucket for whereami logs *)
val create_whereami_file : unit -> (File.name * bool) Result.t

(** Read all log for a bucket *)
val read_logs : string -> Shapes.Log.t list Validation.t

(** Converts logs to [Json.t] *)
val logs_to_json : Shapes.Log.t list -> Paperwork.Json.t

(** Convert whereami location into [Json.t] *)
val whereami_to_json : unit -> Paperwork.Json.t Validation.t

(** Fetch all logs in [Json.t] *)
val collect_all_log_in_json : unit -> Paperwork.Json.t Validation.t
