(** Structured logs*)

open Bedrock
open Paperwork

(** {2 Types} *)

type t =
  { uuid : string
  ; day : Timetable.Day.t
  ; duration : int
  ; sector : string
  ; project : string option
  ; label : string }

(** {2 Projections} *)

val to_qexp : t -> Qexp.t
val from_qexp : Qexp.t -> t Validation.t

(** {2 Utils} *)

val pp : Format.formatter -> t -> unit
val eq : t -> t -> bool
