type t =
  | Unknown of string
  | Unmatched_parenthesis
  | Illegal_character of char
  | Unclosed_string of string

module Exn = struct
  type t = exn

  exception Unknown of string
  exception Unmatched_parenthesis
  exception Illegal_character of char
  exception Unclosed_string of string
end

let to_exception = function
  | Unknown message ->
    Exn.Unknown message
  | Unmatched_parenthesis ->
    Exn.Unmatched_parenthesis
  | Illegal_character char ->
    Exn.Illegal_character char
  | Unclosed_string string ->
    Exn.Unclosed_string string
;;

let from_exception = function
  | Exn.Unknown message ->
    Unknown message
  | Exn.Unmatched_parenthesis ->
    Unmatched_parenthesis
  | Exn.Illegal_character char ->
    Illegal_character char
  | Exn.Unclosed_string string ->
    Unclosed_string string
  | _ ->
    Unknown "unsupported exception"
;;

let to_string = function
  | Unknown message ->
    Format.sprintf "[Unknown] %s" message
  | Unmatched_parenthesis ->
    "[Unmatched_parenthesis] for [parsed expression]"
  | Illegal_character char ->
    Format.sprintf "[Illegal_character] [%c]" char
  | Unclosed_string string ->
    Format.sprintf "[Unclosed_string] [%s]" string
;;

let raise_ error =
  let exn = to_exception error in
  raise exn
;;
