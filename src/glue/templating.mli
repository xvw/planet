(** Express a minmal template system. *)

(** {2 Types} *)

type ruleset = (string, string option -> string) Hashtbl.t

(** {2 Api} *)

val ruleset : (string * (string option -> string)) list -> ruleset
val apply : ruleset -> char Stream.t -> string
