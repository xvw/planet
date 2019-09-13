type state =
  | Backlog
  | Open
  | InProgress
  | Done

type t =
  { state : state
  ; uuid : string
  ; project : string option
  ; main_sector : string
  ; priority : int
  ; name : string
  ; description : string
  ; tags : string list
  ; date : Paperwork.Timetable.Day.t
  ; opening_date : Paperwork.Timetable.Day.t option
  ; closing_date : Paperwork.Timetable.Day.t option
  }
