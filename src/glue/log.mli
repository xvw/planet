(** Deal with Planet's logs *)

open Bedrock
open Baremetal

(** Get the database *)
val database : Shapes.Log.t Database.t

(** Create the bucket for logs *)
val create_file :
  Paperwork.Timetable.Day.t -> (File.name * bool) Result.t
