open Bedrock
open Util
open Paperwork

type uri = string
type name = string
type simple = name * uri
type dated = name * Timetable.Day.t * uri

let mapper_simple =
  let open Table.Mapper in
  couple $ token (fun x -> Ok x) $ string
;;

let mapper_dated =
  let open Table.Mapper in
  triple
  $ token (fun x -> Ok x)
  $ token (Timetable.Day.from_string %> Validation.from_result)
  $ string
;;

let pp_simple ppf (name, uri) = Format.fprintf ppf "<a href='%s'>%s</a>" uri name

let pp_dated ppf (name, time, uri) =
  Format.fprintf ppf "<a href='%s'>[%s] %s</a>" uri (Timetable.Day.to_string time) name
;;

let eq_simple (a, b) (x, y) = a = x && b = y
let eq_dated (a, b, c) (a1, b1, c1) = a = a1 && Timetable.Day.eq b b1 && c = c1

let simple_to_json (name, uri) =
  let open Json in
  obj [ "name", string name; "uri", string uri ]
;;

let dated_to_json (name, day, uri) =
  let open Json in
  obj
    [ "name", string name
    ; "uri", string uri
    ; "date", string $ Timetable.Day.to_string day
    ]
;;

let simple_to_qexp (name, uri) =
  let open Qexp in
  node [ string name; string uri ]
;;

let dated_to_qexp (name, day, uri) =
  let open Qexp in
  node [ string name; keyword (Timetable.Day.to_string day); string uri ]
;;
