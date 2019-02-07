(** Structured logs*)

open Paperwork

(** {2 Types} *)

type t =
  { uuid : string
  ; day : Timetable.Day.t
  ; duration : int
  ; sector : Sector.t
  ; project : Project.t option
  ; label : string }

(** {2 Utils} *)

val pp : Format.formatter -> t -> unit
val eq : t -> t -> bool
