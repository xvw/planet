type state =
  | Backlog
  | Open
  | InProgress
  | Done

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
