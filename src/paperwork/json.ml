type t =
  | String of string
  | Bool of bool
  | Int of int
  | Float of float
  | Nullable of t option
  | Array of t list
  | Object of (string * t) list

let string x = String x
let bool x = Bool x
let int x = Int x
let float x = Float x
let nullable x = Nullable x
let array x = Array x
let obj x = Object x

let rec pp ppf = function
  | String value ->
    Format.fprintf ppf "\"%s\"" value
  | Bool true ->
    Format.fprintf ppf "true"
  | Bool false ->
    Format.fprintf ppf "false"
  | Int value ->
    Format.fprintf ppf "%d" value
  | Float value ->
    Format.fprintf ppf "%f" value
  | Nullable (Some x) ->
    Format.fprintf ppf "%a" pp x
  | Nullable None ->
    Format.fprintf ppf "null"
  | Array x ->
    Format.fprintf ppf "[@[<hov 1>%a@]]" ppa x
  | Object x ->
    Format.fprintf ppf "{@[<hov 1>%a@]}" ppo x

and ppa ppf = function
  | x :: (_ :: _ as xs) ->
    let () = Format.fprintf ppf "%a,@ " pp x in
    ppa ppf xs
  | x :: xs ->
    let () = Format.fprintf ppf "%a" pp x in
    ppa ppf xs
  | [] ->
    ()

and ppo ppf = function
  | (key, x) :: (_ :: _ as xs) ->
    let () = Format.fprintf ppf "\"%s\": %a,@ " key pp x in
    ppo ppf xs
  | (key, x) :: xs ->
    let () = Format.fprintf ppf "\"%s\": %a" key pp x in
    ppo ppf xs
  | [] ->
    ()
;;

let to_string = Format.asprintf "%a" pp
