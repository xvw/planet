(** Deal with the shell. *)

(** {2 Types} *)

type fragment
(** A shell command fragment *)

type command
(** A shell command *)

(** {2 Utils} *)

val flag : ?short:bool -> ?value:fragment -> string -> fragment
val subcommand : string -> fragment
val string : ?escaped:bool -> string -> fragment
val atom : string -> fragment
val command : string -> fragment list -> command

(** {2 Serializer} *)

val pp : Format.formatter -> command -> unit
val to_string : command -> string

(** {2 Execution} *)

val run : command -> int

val run_to_stream :
  (command -> char Stream.t -> 'a) -> command -> Unix.process_status * 'a

val run_to_string : command -> Unix.process_status * string

val capture :
  (unit -> 'a Bedrock.Result.t) -> Unix.process_status -> 'a Bedrock.Result.t
