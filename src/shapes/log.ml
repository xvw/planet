open Paperwork

type t =
  { uuid : string
  ; day : Timetable.Day.t
  ; duration : int
  ; sector : Sector.t
  ; project : Project.t option
  ; label : string }

let pp ppf log =
  let project =
    match log.project with
    | None ->
      ""
    | Some x ->
      Format.sprintf " (%s)" x.Project.name
  in
  Format.fprintf
    ppf
    "Log[%s][%s][%s+%dm] - %s%s"
    log.uuid
    log.sector.name
    (Timetable.Day.to_string log.day)
    log.duration
    log.label
    project
;;
