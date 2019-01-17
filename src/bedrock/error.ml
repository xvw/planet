type t =
  | Unknown of string
  | Unmatched_character of char
  | Illegal_character of char
  | Unclosed_string of string
  | NoRootElement of string

module Exn = struct
  type t = exn

  exception Unknown of string
  exception Unmatched_character of char
  exception Illegal_character of char
  exception Unclosed_string of string
  exception NoRootElement of string
end

let to_exception = function
  | Unknown message ->
    Exn.Unknown message
  | Unmatched_character char ->
    Exn.Unmatched_character char
  | Illegal_character char ->
    Exn.Illegal_character char
  | Unclosed_string string ->
    Exn.Unclosed_string string
  | NoRootElement string ->
    Exn.NoRootElement string
;;

let from_exception = function
  | Exn.Unknown message ->
    Unknown message
  | Exn.Unmatched_character char ->
    Unmatched_character char
  | Exn.Illegal_character char ->
    Illegal_character char
  | Exn.Unclosed_string string ->
    Unclosed_string string
  | Exn.NoRootElement string ->
    NoRootElement string
  | _ ->
    Unknown "unsupported exception"
;;

let to_string = function
  | Unknown message ->
    Format.sprintf "[Unknown] %s" message
  | Unmatched_character char ->
    Format.sprintf "[Unmatched_character] for \"%c\"" char
  | Illegal_character char ->
    Format.sprintf "[Illegal_character] [%c]" char
  | Unclosed_string string ->
    Format.sprintf "[Unclosed_string] [%s]" string
  | NoRootElement string ->
    Format.sprintf "[NoRootElement] for [%s]" string
;;

let raise_ error =
  let exn = to_exception error in
  raise exn
;;
