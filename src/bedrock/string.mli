(** Extension for [Stdlib.String]. *)

(** Alias for type *)
type t = string

(** Produce the md5-hash of a string. *)
val md5 : string -> string

(** [start_with x y] checks if [x] start with [y]. *)
val start_with : string -> string -> bool

(** [end_with x y] checks if [x] end with [y]. *)
val end_with : string -> string -> bool

(** [has_extension x ext] checks if [x] end with [.ext]. *)
val has_extension : string -> string -> bool

(** Produce each lines of a string *)
val lines : string -> string list

(** {2 Stdlib} *)

include module type of Stdlib.String with type t := t
