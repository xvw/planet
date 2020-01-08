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

type board =
  { backlog : int * t list
  ; opened : int * t list
  ; in_progress : int * t list
  ; done_ : int * t list
  ; blocked : int * t list
  }

val new_task
  :  state
  -> string
  -> string option
  -> string list
  -> string
  -> string
  -> (bool * string) list
  -> string list
  -> Paperwork.Timetable.Day.t
  -> Paperwork.Timetable.Day.t option
  -> Paperwork.Timetable.Day.t option
  -> Paperwork.Timetable.Day.t option
  -> t

val state_from_string : string -> state Validation.t
val state_to_string : state -> string
val to_qexp : t -> Qexp.t
val from_qexp : Qexp.t -> t Validation.t
val to_json : t -> Json.t
val pp : Format.formatter -> t -> unit
val eq : t -> t -> bool
val smart_sorter : ?desc:bool -> t -> t -> int

(** {2 Board} *)

val board_create : t list -> board
val board_to_json : board -> Json.t

val new_board
  :  int * t list
  -> int * t list
  -> int * t list
  -> int * t list
  -> int * t list
  -> board

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
