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
  new%js Js.date_day y (m - 1) d
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

  let stringify = function
    | Today, _ ->
      "aujourd'hui"
    | Yesterday, dir ->
      if in_past dir then "hier" else "demain"
    | Days i, dir ->
      Format.asprintf
        "%s %d jours"
        (if in_past dir then "il y a" else "dans")
        i
    | Weeks i, dir ->
      Format.asprintf
        "%s %d semaine%s"
        (if in_past dir then "il y a" else "dans")
        i
        (if i > 1 then "s" else "")
  ;;
end
