open Bedrock
open Util
open Paperwork

type t =
  { uuid : string
  ; day : Timetable.Day.t
  ; duration : int
  ; sector : string
  ; project : string option
  ; label : string
  }

let qexp_project = function
  | None -> []
  | Some x -> [ Qexp.kv "project" x ]
;;

let to_qexp t =
  let open Qexp in
  [ kv "uuid" t.uuid
  ; kv "day" $ Timetable.Day.to_string t.day
  ; kv "duration" $ string_of_int t.duration
  ; kv "sector" t.sector
  ; kv "label" t.label
  ]
  @ qexp_project t.project
  |> node
;;

let new_log uuid day duration sector project label =
  { uuid; day; duration; sector; project; label }
;;

module Fetch = Table.Fetch

let from_qexp expr =
  match Table.configuration expr with
  | Ok config ->
    let open Validation.Syntax in
    let+ uuid = Fetch.string config "uuid"
    and+ day = Fetch.day config "day"
    and+ duration = Fetch.int config "duration"
    and+ sector = Fetch.string config "sector"
    and+ project = Fetch.(option string config "project")
    and+ label = Fetch.string config "label" in
    new_log uuid day duration sector project label
  | Error _ as e -> Validation.from_result e
;;

let pp ppf log =
  let project =
    match log.project with
    | None -> ""
    | Some x -> Format.sprintf " (%s)" x
  in
  Format.fprintf
    ppf
    "Log[%s][%s][%s+%dm] - %s%s"
    log.uuid
    log.sector
    (Timetable.Day.to_string log.day)
    log.duration
    log.label
    project
;;

let eq left right =
  left.uuid = right.uuid
  && Timetable.Day.eq left.day right.day
  && left.duration = right.duration
  && left.sector = right.sector
  && left.project = right.project
  && left.label = right.label
;;

let to_json log =
  let open Json in
  obj
    [ "uuid", string log.uuid
    ; "date", string $ Timetable.Day.to_string log.day
    ; "duration", int log.duration
    ; "sector", string log.sector
    ; "project", nullable Option.(log.project >|= string)
    ; "label", string log.label
    ]
;;
