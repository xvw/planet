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

let fpeek stream =
  match Stream.next stream with
  | chr ->
    Some chr
  | exception Stream.Failure ->
    None
;;

let consume_line stream =
  let rec parse () =
    match fpeek stream with Some '\n' | None -> () | _ -> parse ()
  in
  parse ()
;;

let is_token_char = function
  | '-'
  | '_'
  | '['
  | ']'
  | '`'
  | '\''
  | '0' .. '9'
  | 'A' .. 'Z'
  | 'a' .. 'z' ->
    true
  | _ ->
    false
;;

let check_bracket char = function
  | `Void ->
    Error (Unmatched_character char)
  | `Brace ->
    Error (Unmatched_character '{')
  | `Parenthesis ->
    Error (Unmatched_character '(')
;;

let parse_member f stream =
  let rec parse acc =
    match fpeek stream with
    | (Some (' ' | '\t' | '\n' | ')' | '(' | ';' | '{' | '}') | None)
      as chr ->
      Ok (f acc, chr)
    | Some chr when is_token_char chr ->
      parse $ Format.sprintf "%s%c" acc chr
    | Some chr ->
      Error (Illegal_character chr)
  in
  parse ""
;;

let parse_tag = parse_member tag
let parse_keyword = parse_member keyword

let parse_atom prefix stream =
  let open Monads.Result in
  parse_member id stream
  >|= fun (acc, c) -> atom (Format.sprintf "%c%s" prefix acc), c
;;

let parse_string stream quote delimiter =
  let rec parse escaped acc =
    match fpeek stream with
    | Some '\\' ->
      parse true acc
    | Some chr when chr = delimiter && not escaped ->
      Ok (string ~quote acc)
    | Some chr ->
      parse false (Format.sprintf "%s%c" acc chr)
    | None ->
      Error (Unclosed_string acc)
  in
  parse false ""
;;

let from_stream input =
  let open Monads.Result in
  let rec parse last_char last_bracket acc =
    let current_char =
      match last_char with None -> fpeek input | x -> x
    in
    match current_char with
    | Some (' ' | '\t' | '\n') ->
      parse None last_bracket acc
    | Some ';' ->
      let () = consume_line input in
      parse None last_bracket acc
    | Some '(' ->
      parse None `Parenthesis []
      >>= fun node -> parse None last_bracket (node :: acc)
    | Some ')' ->
      (match last_bracket with
      | `Parenthesis ->
        return (node $ List.rev acc)
      | placeholder ->
        check_bracket ')' placeholder)
    | Some '{' ->
      parse None `Brace []
      >>= fun node -> parse None last_bracket (node :: acc)
    | Some '}' ->
      (match last_bracket with
      | `Brace ->
        return (node $ List.rev acc)
      | placeholder ->
        check_bracket '}' placeholder)
    | Some '"' ->
      parse_string input Double '"'
      >>= fun str -> parse None last_bracket (str :: acc)
    | Some '`' ->
      parse_string input Backtick '`'
      >>= fun str -> parse None last_bracket (str :: acc)
    | Some ':' ->
      parse_tag input
      >>= fun (token, chr) -> parse chr last_bracket (token :: acc)
    | Some '#' ->
      parse_keyword input
      >>= fun (token, chr) -> parse chr last_bracket (token :: acc)
    | Some chr when is_token_char chr ->
      parse_atom chr input
      >>= fun (token, chr) -> parse chr last_bracket (token :: acc)
    | Some chr ->
      Error (Illegal_character chr)
    | None ->
      (match last_bracket with
      | `Void ->
        return (node $ List.rev acc)
      | placeholder ->
        check_bracket '\n' placeholder)
  in
  parse None `Void []
;;

let from_string str_value =
  str_value |> Stream.of_string |> from_stream
;;

let from_bytes bytes = bytes |> Stream.of_bytes |> from_stream

let to_string qexp =
  let open Format in
  let rec fold acc = function
    | String (Double, str) ->
      sprintf "%s\"%s\" " acc str
    | String (Backtick, str) ->
      sprintf "%s`%s` " acc str
    | Atom atom ->
      sprintf "%s%s " acc atom
    | Tag tag ->
      sprintf "%s:%s " acc tag
    | Keyword kwd ->
      sprintf "%s#%s " acc kwd
    | Node [] ->
      sprintf "%s() " acc
    | Block [] ->
      sprintf "%s{} " acc
    | Block children ->
      let res = List.fold_left fold "" children in
      let len = String.length res in
      let sub = String.sub res 0 (len - 1) in
      sprintf "%s{%s} " acc sub
    | Node children ->
      let res = List.fold_left fold "" children in
      let len = String.length res in
      let sub = String.sub res 0 (len - 1) in
      sprintf "%s(%s) " acc sub
  in
  (match qexp with
  | Node children ->
    List.fold_left fold "" children
  | exp ->
    fold "" exp)
  |> String.trim
;;

let to_bytes qexp = qexp |> to_string |> Bytes.of_string
let to_stream qexp = qexp |> to_string |> Stream.of_string
