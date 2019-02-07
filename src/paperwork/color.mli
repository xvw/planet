(** Describe color *)

(* open Bedrock *)

(** {2 Types} *)

type red = int
type green = int
type blue = int
type alpha = float

type t =
  { red : red
  ; green : green
  ; blue : blue
  ; alpha : alpha option }

(** {2 Creation} *)

val create : ?alpha:alpha -> red -> green -> blue -> t

(** {2 Projection} *)

val to_rgb : t -> string
val to_hex : t -> string
val to_string : t -> string

(** {2 Parsing} *)
(* val from_string : string -> t Result.t *)
