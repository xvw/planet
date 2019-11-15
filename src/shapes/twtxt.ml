open Paperwork
open Bedrock
open Util
open Error

type t =
  { date : Timetable.Moment.t
  ; message : string
  }

let make date message = { date; message }

let to_qexp twtxt =
  Qexp.(
    node
      [ keyword $ Timetable.Moment.to_string twtxt.date
      ; string ~quote:back_tick twtxt.message
      ])
;;

let from_qexp qexp =
  let open Qexp in
  match qexp with
  | Node [ Keyword p_date; String (_, message) ] ->
    let open Validation.Infix in
    p_date |> Timetable.Moment.from_string |> Validation.from_result
    >|= fun date -> make date message
  | _ ->
    Error [ Of "Invalid twtxt" ]
;;

let to_string twtxt =
  Format.asprintf
    "%a\t%s"
    Timetable.Moment.pp_twtxt
    twtxt.date
    twtxt.message
;;
