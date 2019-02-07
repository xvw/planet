(** Deal with Planet's sectors *)

open Bedrock

(** Get the database *)
val database : Shapes.Sector.t Database.t

(** Fetch sectors list *)
val all : unit -> (string, Shapes.Sector.t) Hashtbl.t Validation.t
