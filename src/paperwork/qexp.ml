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

let atom value = Atom value
let tag value = Tag value
let keyword value = Keyword value
let string ?(quote = Double) value = String (quote, value)
let node children = Node children
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

let parse_member f stream =
  let rec parse acc =
    let () = print_endline acc in
    match fpeek stream with
    | (Some (' ' | '\t' | '\n' | ')' | '(' | ';') | None) as chr ->
      Ok (f acc, chr)
    | Some chr when is_token_char chr ->
      let () = print_endline "foobar" in
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
  let () = print_endline "foo" in
  let rec parse last_char acc counter =
    let () = print_endline "bar" in
    let current_char =
      match last_char with None -> fpeek input | x -> x
    in
    let () =
      match current_char with
      | Some x ->
        Printf.sprintf "Some(%c)" x |> print_endline
      | None ->
        print_endline "None"
    in
    match current_char with
    | Some (' ' | '\t' | '\n') ->
      parse None acc counter
    | Some ';' ->
      let () = consume_line input in
      parse None acc counter
    | Some '(' ->
      parse None [] (succ counter)
      >>= fun (node, counter) -> parse None (node :: acc) counter
    | Some ')' ->
      return (node $ List.rev acc, pred counter)
    | Some '"' ->
      parse_string input Double '"'
      >>= fun str -> parse None (str :: acc) counter
    | Some '`' ->
      parse_string input Backtick '`'
      >>= fun str -> parse None (str :: acc) counter
    | Some ':' ->
      parse_tag input
      >>= fun (token, chr) -> parse chr (token :: acc) counter
    | Some '#' ->
      parse_keyword input
      >>= fun (token, chr) -> parse chr (token :: acc) counter
    | Some chr when is_token_char chr ->
      parse_atom chr input
      >>= fun (token, chr) -> parse chr (token :: acc) counter
    | Some chr ->
      Error (Illegal_character chr)
    | None ->
      return (node $ List.rev acc, counter)
  in
  bind
  $ (fun (result, counter) ->
      if counter = 0
      then return result
      else Error Unmatched_parenthesis )
  $ parse None [] 0
;;
