(** Describe links *)

open Bedrock
open Paperwork

type uri = string
type name = string

(** Describe a simple link. *)
type simple = name * uri

(** Dated link. *)
type dated = name * Timetable.Day.t * uri

(** {2 Mappers} *)

val mapper_simple : Qexp.t -> simple Validation.t
val mapper_dated : Qexp.t -> dated Validation.t

(** {2 Utils} *)

val pp_simple : Format.formatter -> simple -> unit
val pp_dated : Format.formatter -> dated -> unit
val eq_simple : simple -> simple -> bool
val eq_dated : dated -> dated -> bool
val dated_to_json : dated -> Json.t
val simple_to_json : simple -> Json.t
