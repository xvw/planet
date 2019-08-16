(** Deal with Planet's sectors *)

open Bedrock

(** Get the database *)
val database : Shapes.Sector.t Database.t

(** Fetch sectors list *)
val all : unit -> (string, Shapes.Sector.t) Hashtbl.t Validation.t

(** Fetch sectors list as [Json.t] *)
val to_json : unit -> Paperwork.Json.t Validation.t

(** Render sectors as html nodes *)
val to_html : unit -> string
