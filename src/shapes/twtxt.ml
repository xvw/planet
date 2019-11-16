open Paperwork
open Bedrock
open Util
open Error

type t =
  { date : Timetable.Moment.t
  ; seconds : int
  ; message : string
  }

let make date seconds message = { date; seconds; message }

let to_qexp twtxt =
  Qexp.(
    node
      [ string $ Timetable.Moment.to_string twtxt.date
      ; keyword $ string_of_int twtxt.seconds
      ; string ~quote:back_tick twtxt.message
      ])
;;

let from_qexp qexp =
  let open Qexp in
  match qexp with
  | Node [ String (_, p_date); Keyword p_sec; String (_, message) ] ->
    let open Validation.Infix in
    p_date |> Timetable.Moment.from_string |> Validation.from_result
    >>= (fun date ->
          int_of_string_opt p_sec
          |> Option.map (fun sec -> date, sec)
          |> Validation.from_option (Of "Invalid seconds"))
    >|= fun (date, sec) -> make date sec message
  | _ ->
    Error [ Of "Invalid twtxt" ]
;;

let to_string twtxt =
  Format.asprintf
    "%a\t%s"
    (Timetable.Moment.pp_twtxt twtxt.seconds)
    twtxt.date
    twtxt.message
;;

let cmp a b =
  let res = Timetable.Moment.cmp a.date b.date in
  if res = 0 then compare a.seconds b.seconds else res
;;
