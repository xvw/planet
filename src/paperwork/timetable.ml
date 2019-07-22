open Bedrock
open Error

module Year = struct
  type t = Year of int

  let make value =
    if value < 0 || value > 999
    then Error (Invalid_year value)
    else Ok (Year value)
  ;;

  let to_string (Year n) = Format.sprintf "%03d" n

  let from_string str =
    try Scanf.sscanf str "%03d%!" make with
    | _ ->
      Error (Unparsable str)
  ;;

  let is_leap (Year value) =
    let y = 2000 + value in
    if y mod 100 = 0 then y mod 400 = 0 else y mod 4 = 0
  ;;

  let pp ppf x = Format.fprintf ppf "%s" (to_string x)
  let eq (Year x) (Year y) = x = y
  let cmp (Year x) (Year y) = compare x y
end

module Month = struct
  type month =
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

  type t = Month of (Year.t * month)

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

  let make year_value month_value =
    Ok (Month (year_value, month_value))
  ;;

  let days_in (Month (y, m)) =
    match m with
    | Jan | Mar | May | Jul | Aug | Oct | Dec ->
      31
    | Feb ->
      if Year.is_leap y then 29 else 28
    | _ ->
      30
  ;;

  let to_string (Month (y, m)) =
    let ystr = Year.to_string y in
    Format.sprintf "%s%c" ystr (to_char m |> Char.uppercase_ascii)
  ;;

  let from_string str =
    try
      let open Result.Syntax in
      Scanf.sscanf str "%03d%c%!" (fun year_value char ->
          let* y = Year.make year_value in
          let* m = from_char char in
          make y m)
    with
    | _ ->
      Error (Unparsable str)
  ;;

  let pp ppf x = Format.fprintf ppf "%s" (to_string x)

  let eq (Month (y, m)) (Month (y2, m2)) =
    Year.eq y y2 && to_char m = to_char m2
  ;;

  let cmp (Month (y, m)) (Month (y2, m2)) =
    let r = Year.cmp y y2 in
    if r = 0 then compare m m2 else r
  ;;

  let to_year (Month (y, _)) = y
end

module Day = struct
  type t = Day of (Month.t * int)

  let make month_value day_value =
    let d = Month.days_in month_value in
    if day_value < 1 || day_value > d
    then Error (Invalid_day day_value)
    else Ok (Day (month_value, day_value))
  ;;

  let make_with year_value month_value day_value =
    let open Result.Syntax in
    let* y = Year.make year_value in
    let* m = Month.make y month_value in
    make m day_value
  ;;

  let to_string (Day (m, d)) =
    let mstr = Month.to_string m in
    Format.sprintf "%s%02d" mstr d
  ;;

  let from_string str =
    try
      let open Result.Syntax in
      Scanf.sscanf str "%03d%c%02d%!" (fun yv mc dv ->
          let* y = Year.make yv in
          let* c = Month.from_char mc in
          let* m = Month.make y c in
          make m dv)
    with
    | _ ->
      Error (Unparsable str)
  ;;

  let pp ppf x = Format.fprintf ppf "%s" (to_string x)
  let eq (Day (m, d)) (Day (m2, d2)) = Month.eq m m2 && d = d2

  let cmp (Day (m, d)) (Day (m2, d2)) =
    let r = Month.cmp m m2 in
    if r = 0 then compare d d2 else r
  ;;

  let to_month (Day (m, _)) = m
  let to_year x = Month.to_year (to_month x)
end

module Hour = struct
  type t = Hour of (int * int)

  let make h m =
    if h < 0 || h > 23
    then Error (Invalid_hour h)
    else if m < 0 || m > 59
    then Error (Invalid_min m)
    else Ok (Hour (h, m))
  ;;

  let to_string (Hour (h, m)) =
    let hr = h mod 12 in
    let hm = if hr = 0 then 12 else hr in
    let fl = if h > 11 then "PM" else "AM" in
    Format.sprintf "%02d%s%02d" hm fl m
  ;;

  let from_string str =
    try
      let open Result.Syntax in
      Scanf.sscanf str "%2d%2s%2d%!" (fun h flag m ->
          let hr =
            match String.lowercase_ascii flag with
            | "am" ->
              Ok (if h = 12 then 0 else h)
            | "pm" ->
              Ok (if h = 12 then h else h + 12)
            | _ ->
              Error (Unparsable (str ^ ": unknown " ^ flag))
          in
          let* h = hr in
          make h m)
    with
    | _ ->
      Error (Unparsable str)
  ;;

  let pp ppf x = Format.fprintf ppf "%s" (to_string x)
  let eq (Hour (h, m)) (Hour (h2, m2)) = h = h2 && m = m2

  let cmp (Hour (h, m)) (Hour (h2, m2)) =
    let r = compare h h2 in
    if r = 0 then compare m m2 else r
  ;;
end

module Moment = struct
  type t = Day.t * Hour.t

  let make d h = d, h

  let make_with year_value month_value day_value hour_value min_value
    =
    let open Result.Syntax in
    let* d = Day.make_with year_value month_value day_value in
    let* c = Hour.make hour_value min_value in
    Ok (make d c)
  ;;

  let to_string (d, h) =
    let a = Day.to_string d in
    let b = Hour.to_string h in
    a ^ ":" ^ b
  ;;

  let from_string str =
    try
      let open Result.Syntax in
      Scanf.sscanf str "%6s:%6s%!" (fun d h ->
          let* left = Day.from_string d in
          let* right = Hour.from_string h in
          Ok (make left right))
    with
    | _ ->
      Error (Unparsable str)
  ;;

  let pp ppf x = Format.fprintf ppf "%s" (to_string x)
  let eq (d, h) (d2, h2) = Day.eq d d2 && Hour.eq h h2

  let cmp (d, h) (d2, h2) =
    let r = Day.cmp d d2 in
    if r = 0 then Hour.cmp h h2 else r
  ;;

  let extract
      ( Day.Day (Month.Month (Year.Year year, month), day)
      , Hour.Hour (hour, min) )
    =
    let yr = Year.Year year in
    let mt = Month.Month (yr, month) in
    let dy = Day.Day (mt, day) in
    let hr = Hour.Hour (hour, min) in
    yr, mt, dy, hr
  ;;
end
