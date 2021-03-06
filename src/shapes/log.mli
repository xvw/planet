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
  ; label : string
  }

(** {2 Projections} *)

val new_log
  :  string
  -> Timetable.Day.t
  -> int
  -> string
  -> string option
  -> string
  -> t

val to_qexp : t -> Qexp.t
val from_qexp : Qexp.t -> t Validation.t

(** {2 Utils} *)

val pp : Format.formatter -> t -> unit
val eq : t -> t -> bool
val to_json : t -> Json.t
