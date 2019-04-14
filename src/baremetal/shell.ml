type fragment =
  | Flag of (bool * string * fragment option)
  | Subcommand of string
  | String of string
  | Atom of string

type command = string * fragment list

let flag ?(short = true) ?value flag_name =
  Flag (short, flag_name, value)
;;

let subcommand cmd = Subcommand cmd
let string str = String str
let atom str = Atom str
let command cmd fragments = cmd, fragments

let rec fragment_mapper = function
  | String str ->
    "\"" ^ String.escaped str ^ "\""
  | Atom str ->
    str
  | Subcommand str ->
    str
  | Flag (is_short, flag, value) ->
    let prefix = if is_short then "-" else "--" in
    let value = map_value value in
    Format.asprintf "%s%s%s" prefix flag value

and map_value = function
  | None ->
    ""
  | Some fragment ->
    " " ^ fragment_mapper fragment
;;

let fragments_to_string fragments =
  fragments |> List.map fragment_mapper |> String.concat " "
;;

let pp_fragments ppf = function
  | _ :: _ as fragments ->
    Format.fprintf ppf " %s" (fragments_to_string fragments)
  | [] ->
    ()
;;

let pp ppf (cmd, fragments) =
  Format.fprintf ppf "%s%a" cmd pp_fragments fragments
;;

let to_string = Format.asprintf "%a" pp
