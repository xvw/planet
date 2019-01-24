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
