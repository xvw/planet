(** Express a minmal template system.
*)

open Bedrock

(** {2 Types} *)

type ruleset = (string, string option -> string Result.t) Hashtbl.t

(** {2 Api} *)

val ruleset
  :  (string * (string option -> string Result.t)) list
  -> ruleset

val apply : ruleset -> char Stream.t -> string Result.t
