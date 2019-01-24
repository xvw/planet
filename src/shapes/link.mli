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
