(** Extension for [Stdlib.String]. *)

type t = string
(** Alias for type *)

val md5 : string -> string
(** Produce the md5-hash of a string. *)

val start_with : string -> string -> bool
(** [start_with x y] checks if [x] start with [y]. *)

val end_with : string -> string -> bool
(** [end_with x y] checks if [x] end with [y]. *)

val has_extension : string -> string -> bool
(** [has_extension x ext] checks if [x] end with [.ext]. *)

val lines : string -> string list
(** Produce each lines of a string *)

val super_trim : string -> string
(** Remove all spaces of a string. *)

(** {2 Stdlib} *)

include module type of Stdlib.String with type t := t
