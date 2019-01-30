(** Terminal related UI. *)

open Bedrock

(** {2 Prompt errors} *)

val prompt_errors : ?intro:bool -> Error.t list -> unit
val prompt_error : ?intro:bool -> Error.t -> unit
