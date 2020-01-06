(** Describe Task *)

open Bedrock
open Paperwork

type state =
  | Backlog
  | Opened
  | InProgress
  | Done
  | Blocked

type t =
  { state : state
  ; uuid : string
  ; project : string option
  ; sectors : string list
  ; name : string
  ; description : string
  ; checklist : (bool * string) list
  ; tags : string list
  ; date : Paperwork.Timetable.Day.t
  ; opening_date : Paperwork.Timetable.Day.t option
  ; closing_date : Paperwork.Timetable.Day.t option
  ; engagement_date : Paperwork.Timetable.Day.t option
  }

val state_from_string : string -> state Validation.t
val state_to_string : state -> string
val to_qexp : t -> Qexp.t
val from_qexp : Qexp.t -> t Validation.t
val to_json : t -> Json.t
val pp : Format.formatter -> t -> unit
val eq : t -> t -> bool

(** {2 Properties on task} *)

val all_checked : t -> bool
val all_unchecked : t -> bool
val has_checked : t -> bool
val need_opening_date : t -> bool
val need_closing_date : t -> bool

val need_state_changement
  :  Paperwork.Timetable.Day.t
  -> t
  -> (state option * Timetable.Day.t option * Timetable.Day.t option) * bool
