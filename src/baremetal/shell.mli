(** Deal with the shell. *)

(** {2 Types} *)

(** A shell command fragment *)
type fragment

(** A shell command *)
type command

(** {2 Utils} *)

val flag : ?short:bool -> ?value:fragment -> string -> fragment
val subcommand : string -> fragment
val string : string -> fragment
val atom : string -> fragment
val command : string -> fragment list -> command

(** {2 Serializer} *)

val pp : Format.formatter -> command -> unit
val to_string : command -> string
