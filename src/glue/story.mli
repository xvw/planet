(** Deal with Planet's publication *)

open Bedrock

val collect : unit -> Shapes.Story.t list Validation.t
(** Collect all post (long or short) *)

val to_hakyll :
     Shapes.Story.t
  -> (Shapes.Story.t * string * string * string * string) Validation.t
(** Convert project to Hakyll file *)
