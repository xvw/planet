(** Try to generate Json in a very manual way. *)

type t =
  | String of string
  | Bool of bool
  | Int of int
  | Float of float
  | Nullable of t option
  | Array of t list
  | Object of (string * t) list

(** {2 Builder} *)

val string : string -> t
val bool : bool -> t
val int : int -> t
val float : float -> t
val nullable : t option -> t
val array : t list -> t
val obj : (string * t) list -> t

(** {2 Serialization} *)

(** Format for [printf] *)
val pp : Format.formatter -> t -> unit

(** From [Json.t] to [string] *)
val to_string : t -> string
