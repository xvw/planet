open Bedrock
open Util
open Error

module Month = struct
  type t =
    | Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec

  let to_string = function
    | Jan ->
      "jan"
    | Feb ->
      "feb"
    | Mar ->
      "mar"
    | Apr ->
      "apr"
    | May ->
      "may"
    | Jun ->
      "jun"
    | Jul ->
      "jul"
    | Aug ->
      "aug"
    | Sep ->
      "sep"
    | Oct ->
      "oct"
    | Nov ->
      "nov"
    | Dec ->
      "dec"
  ;;

  let from_int = function
    | 1 ->
      Ok Jan
    | 2 ->
      Ok Feb
    | 3 ->
      Ok Mar
    | 4 ->
      Ok Apr
    | 5 ->
      Ok May
    | 6 ->
      Ok Jun
    | 7 ->
      Ok Jul
    | 8 ->
      Ok Aug
    | 9 ->
      Ok Sep
    | 10 ->
      Ok Oct
    | 11 ->
      Ok Nov
    | 12 ->
      Ok Dec
    | n ->
      Error (Invalid_month n)
  ;;

  let to_char = function
    | Jan ->
      'a'
    | Feb ->
      'b'
    | Mar ->
      'c'
    | Apr ->
      'd'
    | May ->
      'e'
    | Jun ->
      'f'
    | Jul ->
      'g'
    | Aug ->
      'h'
    | Sep ->
      'i'
    | Oct ->
      'j'
    | Nov ->
      'k'
    | Dec ->
      'l'
  ;;

  let from_char char =
    let c = Char.lowercase_ascii char |> int_of_char in
    let a = int_of_char 'a' in
    from_int (c - a + 1)
  ;;
end

type year = Year of int
type month = Month of (year * Month.t)
type day = Day of (month * int)
type hour = Hour of (int * int)
type moment = day * hour

let year value =
  if value < 0 || value > 999
  then Error (Invalid_year value)
  else Ok (Year value)
;;

let month year_value month_value =
  Ok (Month (year_value, month_value))
;;

let is_leap (Year value) =
  let y = 2000 + value in
  if y mod 100 = 0 then y mod 400 = 0 else y mod 4 = 0
;;

let days_in (Month (y, m)) =
  match m with
  | Jan | Mar | May | Jul | Aug | Oct | Dec ->
    31
  | Feb ->
    if is_leap y then 29 else 28
  | _ ->
    30
;;

let day month_value day_value =
  let d = days_in month_value in
  if day_value < 1 || day_value > d
  then Error (Invalid_day day_value)
  else Ok (Day (month_value, day_value))
;;

let day_with year_value month_value day_value =
  let open Result.Infix in
  year_value |> year
  >>= fun y -> month y month_value >>= fun m -> day m day_value
;;

let hour h m =
  if h < 0 || h > 23
  then Error (Invalid_hour h)
  else if m < 0 || m > 59
  then Error (Invalid_min m)
  else Ok (Hour (h, m))
;;

let moment d h = d, h

let moment_with year_value month_value day_value hour_value min_value
    =
  let open Result.Infix in
  day_value
  |> day_with year_value month_value
  >>= fun d -> hour hour_value min_value >|= fun h -> d, h
;;

let year_to_string (Year n) = Format.sprintf "%03d" n

let month_to_string (Month (y, m)) =
  let ystr = year_to_string y in
  Format.sprintf "%s%c" ystr (Month.to_char m |> Char.uppercase_ascii)
;;

let day_to_string (Day (m, d)) =
  let mstr = month_to_string m in
  Format.sprintf "%s%02d" mstr d
;;

let hour_to_string (Hour (h, m)) =
  let hr = h mod 12 in
  let hm = if hr = 0 then 12 else hr in
  let fl = if h > 11 then "PM" else "AM" in
  Format.sprintf "%02d%s%02d" hm fl m
;;

let moment_to_string (d, h) =
  let a = day_to_string d in
  let b = hour_to_string h in
  a ^ ":" ^ b
;;

let year_from_string str =
  try Scanf.sscanf str "%03d" year with _ -> Error (Unparsable str)
;;

let month_from_string str =
  try
    let open Result.Infix in
    Scanf.sscanf str "%03d%c" (fun year_value char ->
        year year_value >>= fun y -> Month.from_char char >>= month y
    )
  with _ -> Error (Unparsable str)
;;

let day_from_string str =
  try
    let open Result.Infix in
    Scanf.sscanf str "%03d%c%02d" (fun yv mc dv ->
        year yv
        >>= fun y -> Month.from_char mc >>= month y >>= flip day dv
    )
  with _ -> Error (Unparsable str)
;;

let hour_from_string str =
  try
    let open Result.Infix in
    Scanf.sscanf str "%2d%2s%2d" (fun h flag m ->
        let hr =
          match String.lowercase_ascii flag with
          | "am" ->
            Ok (if h = 12 then 0 else h)
          | "pm" ->
            Ok ((h + 12) mod 24)
          | _ ->
            Error (Unparsable (str ^ ": unknown " ^ flag))
        in
        hr >>= fun h -> hour h m )
  with _ -> Error (Unparsable str)
;;

let moment_from_string str =
  try
    let open Result.Infix in
    Scanf.sscanf str "%6s:%6s" (fun d h ->
        day_from_string d
        >>= fun left ->
        hour_from_string h >|= fun right -> moment left right )
  with _ -> Error (Unparsable str)
;;
