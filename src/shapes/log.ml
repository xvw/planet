open Paperwork

type t =
  { uuid : string
  ; day : Timetable.Day.t
  ; duration : int
  ; sector : Sector.t
  ; project : Project.t option
  ; label : string }
