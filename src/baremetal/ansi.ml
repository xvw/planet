type color =
  | Default
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Bright_black
  | Bright_red
  | Bright_green
  | Bright_yellow
  | Bright_blue
  | Bright_magenta
  | Bright_cyan
  | Bright_white

let default = Default
let black = Black
let red = Red
let green = Green
let yellow = Yellow
let blue = Blue
let magenta = Magenta
let cyan = Cyan
let white = White
let bright_black = Bright_black
let bright_red = Bright_red
let bright_green = Bright_green
let bright_yellow = Bright_yellow
let bright_blue = Bright_blue
let bright_magenta = Bright_magenta
let bright_cyan = Bright_cyan
let bright_white = Bright_white

type fragment =
  | Reset
  | Bold
  | Underline
  | Blink
  | Inverse
  | Hidden
  | Foreground of color
  | Background of color
  | Text of string

type fragments = fragment list

let foreground c = Foreground c
let fg = foreground
let background c = Background c
let bg = background
let reset = Reset
let bold = Bold
let underline = Underline
let blink = Blink
let inverse = Inverse
let hidden = Hidden
let text x = Text x

let to_int_fg = function
  | Default ->
    39
  | Black ->
    30
  | Red ->
    31
  | Green ->
    32
  | Yellow ->
    33
  | Blue ->
    34
  | Magenta ->
    35
  | Cyan ->
    36
  | White ->
    37
  | Bright_black ->
    90
  | Bright_red ->
    91
  | Bright_green ->
    92
  | Bright_yellow ->
    93
  | Bright_blue ->
    94
  | Bright_magenta ->
    95
  | Bright_cyan ->
    96
  | Bright_white ->
    97
;;

let to_int_bg color = 10 + to_int_fg color

let to_string_aux s =
  let aux = function
    | Reset ->
      `I 0
    | Bold ->
      `I 1
    | Underline ->
      `I 4
    | Blink ->
      `I 5
    | Inverse ->
      `I 7
    | Hidden ->
      `I 8
    | Foreground c ->
      `I (to_int_fg c)
    | Background c ->
      `I (to_int_bg c)
    | Text txt ->
      `S txt
  in
  match aux s with `I i -> string_of_int i | `S i -> i
;;

let ( ! ) = text

let seq_to_string = function
  | None ->
    ""
  | Some fragments ->
    fragments
    |> List.rev_map to_string_aux
    |> String.concat ";"
    |> Format.sprintf "\027[%sm"
;;

let to_string ?(scoped = true) =
  let rec aux sequence acc fragment =
    match sequence, fragment with
    | x, [] ->
      acc ^ seq_to_string x
      ^ if scoped then seq_to_string (Some [reset]) else ""
    | x, Text str :: xs ->
      aux None (acc ^ seq_to_string x ^ str) xs
    | None, frg :: xs ->
      aux (Some [frg]) acc xs
    | Some x, frg :: xs ->
      aux (Some (frg :: x)) acc xs
  in
  aux None ""
;;

let pp ppf fragment =
  Format.fprintf ppf "%s" (to_string ~scoped:false fragment)
;;

let pps ppf fragment =
  Format.fprintf ppf "%s" (to_string ~scoped:true fragment)
;;

let only_style = List.filter (function Text _ -> false | _ -> true)
