type t =
  | Unknown of string
  | Unmatched_character of char
  | Illegal_character of char
  | Unclosed_string of string
  | No_root_element of string
  | Invalid_attribute of string
  | Already_exists of string
  | Unreadable of string
  | Unix of string

module Exn = struct
  type t = exn

  exception Unknown of string
  exception Unmatched_character of char
  exception Illegal_character of char
  exception Unclosed_string of string
  exception No_root_element of string
  exception Invalid_attribute of string
  exception Already_exists of string
  exception Unreadable of string
  exception Unix of string
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
  | No_root_element string ->
    Exn.No_root_element string
  | Invalid_attribute string ->
    Exn.Invalid_attribute string
  | Already_exists string ->
    Exn.Already_exists string
  | Unreadable string ->
    Exn.Unreadable string
  | Unix string ->
    Exn.Unix string
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
  | Exn.No_root_element string ->
    No_root_element string
  | Exn.Invalid_attribute string ->
    Invalid_attribute string
  | Exn.Already_exists string ->
    Already_exists string
  | Exn.Unreadable string ->
    Unreadable string
  | Exn.Unix string ->
    Unix string
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
  | No_root_element string ->
    Format.sprintf "[No_root_element] for [%s]" string
  | Invalid_attribute string ->
    Format.sprintf "[Invalid_attribute] for [%s]" string
  | Already_exists string ->
    Format.sprintf "[Already_exists] [%s]" string
  | Unreadable string ->
    Format.sprintf "[Unreadable] [%s]" string
  | Unix string ->
    Format.sprintf "[Unix error] %s" string
;;

let raise_ error =
  let exn = to_exception error in
  raise exn
;;
