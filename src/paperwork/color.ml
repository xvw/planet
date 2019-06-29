open Bedrock
open Error

type red = int
type green = int
type blue = int
type alpha = float

type t =
  { red : red
  ; green : green
  ; blue : blue
  ; alpha : alpha option
  }

let create ?alpha red green blue =
  { red = Util.bound red 0 255
  ; green = Util.bound green 0 255
  ; blue = Util.bound blue 0 255
  ; alpha = Option.map (fun x -> Util.bound 0. 1. x) alpha
  }
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

let pp ppf color =
  let alpha =
    match color.alpha with
    | None | Some 1.0 ->
      ""
    | Some x ->
      Format.sprintf " ,a:%g" x
  in
  Format.fprintf
    ppf
    "Color(r:%d, g:%d, b:%d%s)"
    color.red
    color.green
    color.blue
    alpha
;;

let eq a b =
  let f x = match x.alpha with None -> 1.0 | Some x -> x in
  a.red = b.red && a.green = b.green && a.blue = b.blue && f a = f b
;;

let rgb r g b = Ok (create r g b)
let rgba r g b a = Ok (create ~alpha:a r g b)

let from_string str =
  let s = str |> String.super_trim |> String.lowercase_ascii in
  try Scanf.sscanf s "rgb(%d,%d,%d)" rgb with
  | _ ->
    (try Scanf.sscanf s "rgba(%d,%d,%d,%g)" rgba with
    | _ ->
      (try Scanf.sscanf s "#%02x%02x%02x" rgb with
      | _ ->
        Error (Unparsable_color str)))
;;
