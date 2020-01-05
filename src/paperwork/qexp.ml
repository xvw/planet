open Bedrock
open Util
open Error

type quote =
  | Double
  | Backtick

type t =
  | Atom of string
  | Tag of string
  | Keyword of string
  | String of (quote * string)
  | Node of t list
  | Block of t list

let atom value = Atom value
let tag value = Tag value
let keyword value = Keyword value
let string ?(quote = Double) value = String (quote, value)
let node children = Node children
let block children = Block children
let double_quote = Double
let back_tick = Backtick

let kv ?(k = tag) ?(v = string ~quote:double_quote) key value =
  node [ k key; v value ]
;;

let fpeek stream =
  match Stream.next stream with
  | chr -> Some chr
  | exception Stream.Failure -> None
;;

let consume_line stream =
  let rec parse () =
    match fpeek stream with
    | Some '\n' | None -> ()
    | _ -> parse ()
  in
  parse ()
;;

let is_token_char = function
  | '-' | '_' | '[' | ']' | '`' | '\'' | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' ->
    true
  | _ -> false
;;

let check_bracket char = function
  | `Void -> Error (Unmatched_character char)
  | `Brace -> Error (Unmatched_character '{')
  | `Parenthesis -> Error (Unmatched_character '(')
;;

let parse_member f stream =
  let rec parse acc =
    match fpeek stream with
    | (Some (' ' | '\t' | '\n' | ')' | '(' | ';' | '{' | '}') | None) as chr ->
      Ok (f acc, chr)
    | Some chr when is_token_char chr -> parse $ Format.sprintf "%s%c" acc chr
    | Some chr -> Error (Illegal_character chr)
  in
  parse ""
;;

let parse_tag = parse_member tag
let parse_keyword = parse_member keyword

let parse_atom prefix stream =
  let open Result.Monad in
  parse_member id stream
  >|= fun (acc, c) -> atom (Format.sprintf "%c%s" prefix acc), c
;;

let parse_string stream quote delimiter =
  let rec parse escaped acc =
    match fpeek stream with
    | Some '\\' -> parse true acc
    | Some chr when chr = delimiter && not escaped -> Ok (string ~quote acc)
    | Some chr -> parse false (Format.sprintf "%s%c" acc chr)
    | None -> Error (Unclosed_string acc)
  in
  parse false ""
;;

let from_stream input =
  let open Result.Monad in
  let rec parse last_char last_bracket acc =
    let current_char =
      match last_char with
      | None -> fpeek input
      | x -> x
    in
    match current_char with
    | Some (' ' | '\t' | '\n') -> parse None last_bracket acc
    | Some ';' ->
      let () = consume_line input in
      parse None last_bracket acc
    | Some '(' ->
      let* node = parse None `Parenthesis [] in
      parse None last_bracket (node :: acc)
    | Some ')' ->
      (match last_bracket with
      | `Parenthesis -> return (node $ List.rev acc)
      | placeholder -> check_bracket ')' placeholder)
    | Some '{' ->
      let* node = parse None `Brace [] in
      parse None last_bracket (node :: acc)
    | Some '}' ->
      (match last_bracket with
      | `Brace -> return (block $ List.rev acc)
      | placeholder -> check_bracket '}' placeholder)
    | Some '"' ->
      let* str = parse_string input Double '"' in
      parse None last_bracket (str :: acc)
    | Some '`' ->
      let* str = parse_string input Backtick '`' in
      parse None last_bracket (str :: acc)
    | Some ':' ->
      let* token, chr = parse_tag input in
      parse chr last_bracket (token :: acc)
    | Some '#' ->
      let* token, chr = parse_keyword input in
      parse chr last_bracket (token :: acc)
    | Some chr when is_token_char chr ->
      let* token, chr = parse_atom chr input in
      parse chr last_bracket (token :: acc)
    | Some chr -> Error (Illegal_character chr)
    | None ->
      (match last_bracket with
      | `Void -> return (node $ List.rev acc)
      | placeholder -> check_bracket '\n' placeholder)
  in
  parse None `Void []
;;

let from_string str_value = str_value |> Stream.of_string |> from_stream
let from_bytes bytes = bytes |> Stream.of_bytes |> from_stream

let rec pp ppf = function
  | String (Double, value) -> Format.fprintf ppf "\"%s\"" value
  | String (Backtick, value) -> Format.fprintf ppf "`%s`" value
  | Atom value -> Format.fprintf ppf "%s" value
  | Tag value -> Format.fprintf ppf ":%s" value
  | Keyword value -> Format.fprintf ppf "#%s" value
  | Node elements -> Format.fprintf ppf "(@[<hov 1>%a@])" ppl elements
  | Block elements -> Format.fprintf ppf "{@[<hov 1>%a@]}" ppl elements

and ppl ppf = function
  | x :: (_ :: _ as xs) ->
    let () = Format.fprintf ppf "%a@ " pp x in
    ppl ppf xs
  | x :: xs ->
    let () = Format.fprintf ppf "%a" pp x in
    ppl ppf xs
  | [] -> ()
;;

let to_string = function
  | Node xs -> Format.asprintf "%a" ppl xs
  | expr -> Format.asprintf "%a" pp expr
;;

let to_bytes qexp = qexp |> to_string |> Bytes.of_string
let to_stream qexp = qexp |> to_string |> Stream.of_string

let extract_root = function
  | Node li -> Ok li
  | qexp -> Error (No_root_element (to_string qexp))
;;
