open Js_of_ocaml
open Bedrock
open Paperwork
open Util

type timestamp = float
type date = Js.date Js.t

let now () = new%js Js.date_now
let time_of date = date##getTime

let from_day day =
  let y, m, d = Timetable.Day.unfold day in
  new%js Js.date_min y (m - 1) d 9 0
;;

let from_month month =
  let y, m = Timetable.Month.unfold month in
  new%js Js.date_month y (m - 1)
;;

let from_year year =
  let y = Timetable.Year.unfold year in
  new%js Js.date_month y 0
;;

let from_moment moment =
  let y, mo, d, h, min = Timetable.Moment.unfold moment in
  new%js Js.date_min y (mo - 1) d h min
;;

let to_day date =
  let y = date##getFullYear - 2000 in
  let m = Timetable.Month.from_int (date##getMonth + 1) in
  let d = date##getDate in
  Result.Infix.(m >>= fun m -> Timetable.Day.make_with y m d)
;;

module Ago = struct
  type t =
    | Today
    | Yesterday
    | Days of int
    | Weeks of int

  type direction =
    | Past
    | Future

  let compute ?(reference = now ()) d =
    let tmin = time_of d in
    let tmax = time_of reference in
    let direction = if reference > d then Past else Future in
    let diff = tmax -. tmin in
    let days_f = Stdlib.(diff /. 86400000.) in
    let k = Stdlib.(int_of_float %> abs) in
    let days = k days_f in
    ( (if days = 0
      then Today
      else if days = 1
      then Yesterday
      else if days < 7
      then Days days
      else Weeks (k (days_f /. 7.)))
    , direction )
  ;;

  let in_past = function Past -> true | Future -> false

  let stringify ?(since = "il y a") ?(since_f = "dans") = function
    | Today, _ ->
      "aujourd'hui"
    | Yesterday, dir ->
      if in_past dir then "hier" else "demain"
    | Days i, dir ->
      Format.asprintf
        "%s %d jours"
        (if in_past dir then since else since_f)
        i
    | Weeks i, dir ->
      Format.asprintf
        "%s %d semaine%s"
        (if in_past dir then since else since_f)
        i
        (if i > 1 then "s" else "")
  ;;
end
