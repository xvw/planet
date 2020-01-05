open Bedrock
open Error

type fragment =
  | Flag of (bool * string * fragment option)
  | Subcommand of string
  | String of (bool * string)
  | Atom of string

type command = string * fragment list

let flag ?(short = true) ?value flag_name = Flag (short, flag_name, value)
let subcommand cmd = Subcommand cmd
let string ?(escaped = false) str = String (escaped, str)
let atom str = Atom str
let command cmd fragments = cmd, fragments

let rec fragment_mapper = function
  | String (e, str) ->
    let f = if e then String.escaped else Util.id in
    "\"" ^ f str ^ "\""
  | Atom str -> str
  | Subcommand str -> str
  | Flag (is_short, flag, value) ->
    let prefix = if is_short then "-" else "--" in
    let value = map_value value in
    Format.asprintf "%s%s%s" prefix flag value

and map_value = function
  | None -> ""
  | Some fragment -> " " ^ fragment_mapper fragment
;;

let fragments_to_string fragments =
  fragments |> List.map fragment_mapper |> String.concat " "
;;

let pp_fragments ppf = function
  | _ :: _ as fragments ->
    Format.fprintf ppf " %s" (fragments_to_string fragments)
  | [] -> ()
;;

let pp ppf (cmd, fragments) =
  Format.fprintf ppf "%s%a" cmd pp_fragments fragments
;;

let to_string = Format.asprintf "%a" pp
let run command = command |> to_string |> Sys.command

let run_to_stream f command =
  let cmd = to_string command in
  let channel = Unix.open_process_in cmd in
  let stream = Stream.of_channel channel in
  let result = f command stream in
  let status = Unix.close_process_in channel in
  status, result
;;

let run_to_string command =
  let cmd = to_string command in
  let channel = Unix.open_process_in cmd in
  let rec aux acc =
    try aux (Format.asprintf "%s%c" acc (input_char channel)) with
    | End_of_file -> acc
  in
  let result = aux "" in
  let status = Unix.close_process_in channel in
  status, result
;;

let capture f = function
  | Unix.WEXITED 0 -> f ()
  | Unix.WEXITED x -> Error (Wexited x)
  | Unix.WSIGNALED x -> Error (Wsignaled x)
  | Unix.WSTOPPED x -> Error (Wstopped x)
;;
