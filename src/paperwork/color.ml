open Bedrock

type red = int
type green = int
type blue = int
type alpha = float

type t =
  { red : red
  ; green : green
  ; blue : blue
  ; alpha : alpha option }

let create ?alpha red green blue =
  { red = Util.bound red 0 255
  ; green = Util.bound green 0 255
  ; blue = Util.bound blue 0 255
  ; alpha = Option.map (fun x -> Util.bound 0. 1. x) alpha }
;;

let to_rgb color =
  let left =
    Format.sprintf "%d, %d, %d" color.red color.green color.blue
  in
  match color.alpha with
  | None | Some 1.0 ->
    Format.sprintf "rgb(%s)" left
  | Some right ->
    Format.sprintf "rgba(%s, %g)" left right
;;

let to_hex color =
  Format.sprintf "#%02x%02x%02x" color.red color.green color.blue
;;

let to_string = to_rgb
