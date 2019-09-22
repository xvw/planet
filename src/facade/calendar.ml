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

let to_day date =
  let y = date##getFullYear - 2000 in
  let m = Timetable.Month.from_int (date##getMonth + 1) in
  let d = date##getDate in
  Result.Infix.(m >>= fun m -> Timetable.Day.make_with y m d)
;;

let iso_week target =
  let dnbr = (target##getDay + 6) mod 7 in
  let _ = target##setDate (target##getDate - dnbr + 3) in
  let first = target##valueOf in
  let _ = target##setMonth 0 in
  let _ = target##setDate 1 in
  let _ =
    if target##getDay <> 4
    then (
      let _ = target##setMonth 0 in
      let _ =
        let k = 4 - target##getDay in
        let p = k + 7 in
        let m = p mod 7 in
        target##setDate (1 + m)
      in
      ())
  in
  let v = first -. target##valueOf in
  let r = Stdlib.ceil (v /. 604800000.) in
  1 + int_of_float r
;;

let to_monday date =
  let ts = date##valueOf in
  let target = new%js Js.date_fromTimeValue ts in
  let day = date##getDay in
  let factor = if day = 0 then -6 else 1 in
  let diff = date##getDate - day + factor in
  let _ = target##setDate diff in
  target
;;

let weeks_between d1 d2 =
  let one = float_of_int (1000 * 60 * 60 * 24 * 7) in
  let ms1 = d1##getTime in
  let ms2 = d2##getTime in
  let ms = abs_float (ms1 -. ms2) in
  let r = Stdlib.ceil (ms /. one) in
  int_of_float r
;;

let years_ago years date =
  let ts = date##valueOf in
  let target = new%js Js.date_fromTimeValue ts in
  let _ = target##setFullYear (date##getFullYear - years) in
  if target##getDay = 1 then target else to_monday target
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

  let precise_label a b =
    if a##getDate = b##getDate then Today else Yesterday
  ;;

  let compute ?(in_day = false) ?(reference = now ()) d =
    let tmin = time_of d in
    let tmax = time_of reference in
    let direction = if reference > d then Past else Future in
    let diff = tmax -. tmin in
    let days_f = Stdlib.(diff /. 86400000.) in
    let k = Stdlib.(abs_float) in
    let days = k days_f in
    let res =
      if days < 2.0
      then precise_label reference d
      else if days < 7. || in_day
      then Days (int_of_float days)
      else Weeks (int_of_float (k (days_f /. 7.)))
    in
    res, direction
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
