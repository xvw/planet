(** Deal with Planet's publication *)

open Bedrock

(** Collect all post (long or short) *)
val collect : unit -> Shapes.Story.t list Validation.t

(** Convert project to Hakyll file *)
val to_hakyll
  :  Shapes.Story.t
  -> (Shapes.Story.t * string * string * (string -> string) * string)
     Validation.t
