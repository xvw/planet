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
