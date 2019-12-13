(** Deal with Planet's sectors *)

open Bedrock

val database : Shapes.Sector.t Database.t
(** Get the database *)

val all : unit -> (string, Shapes.Sector.t) Hashtbl.t Validation.t
(** Fetch sectors list *)

val to_json : unit -> Paperwork.Json.t Validation.t
(** Fetch sectors list as [Json.t] *)

val to_html : unit -> string
(** Render sectors as html nodes *)
