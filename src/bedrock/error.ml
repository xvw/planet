type t =
  | Unknown of string
  | Unmatched_character of char
  | Illegal_character of char
  | Unclosed_string of string
  | No_root_element of string
  | Invalid_attribute of string
  | Already_exists of string
  | Unreadable of string
  | Invalid_year of int
  | Invalid_month of int
  | Invalid_day of int
  | Invalid_hour of int
  | Invalid_min of int
  | Invalid_char of char
  | Invalid_int of int
  | Must_be_positive of int
  | Must_be_negative of int
  | Unparsable of string
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
  exception Invalid_year of int
  exception Invalid_month of int
  exception Invalid_day of int
  exception Invalid_hour of int
  exception Invalid_min of int
  exception Invalid_char of char
  exception Invalid_int of int
  exception Must_be_positive of int
  exception Must_be_negative of int
  exception Unparsable of string
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
  | Invalid_year int ->
    Exn.Invalid_year int
  | Invalid_month int ->
    Exn.Invalid_month int
  | Invalid_day int ->
    Exn.Invalid_day int
  | Invalid_hour int ->
    Exn.Invalid_hour int
  | Invalid_min int ->
    Exn.Invalid_min int
  | Invalid_char char ->
    Exn.Invalid_char char
  | Invalid_int int ->
    Exn.Invalid_int int
  | Must_be_positive int ->
    Exn.Must_be_positive int
  | Must_be_negative int ->
    Exn.Must_be_negative int
  | Unparsable string ->
    Exn.Unparsable string
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
  | Exn.Invalid_year int ->
    Invalid_year int
  | Exn.Invalid_month int ->
    Invalid_month int
  | Exn.Invalid_day int ->
    Invalid_day int
  | Exn.Invalid_min int ->
    Invalid_min int
  | Exn.Invalid_hour int ->
    Invalid_hour int
  | Exn.Invalid_char char ->
    Invalid_char char
  | Exn.Invalid_int int ->
    Invalid_int int
  | Exn.Must_be_positive int ->
    Must_be_positive int
  | Exn.Must_be_negative int ->
    Must_be_negative int
  | Exn.Unparsable string ->
    Unparsable string
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
  | Invalid_year int ->
    Format.sprintf "[Invalid_year] [%d]" int
  | Invalid_month int ->
    Format.sprintf "[Invalid_month] [%d]" int
  | Invalid_day int ->
    Format.sprintf "[Invalid_day] [%d]" int
  | Invalid_hour int ->
    Format.sprintf "[Invalid_hour] [%d]" int
  | Invalid_min int ->
    Format.sprintf "[Invalid_min] [%d]" int
  | Invalid_char char ->
    Format.sprintf "[Invalid_char] [%c]" char
  | Invalid_int int ->
    Format.sprintf "[Invalid_int] [%d]" int
  | Must_be_positive int ->
    Format.sprintf "[Must_be_positive] [%d]" int
  | Must_be_negative int ->
    Format.sprintf "[Must_be_negative] [%d]" int
  | Unparsable string ->
    Format.sprintf "[Unparsable] [%s]" string
  | Unix string ->
    Format.sprintf "[Unix error] %s" string
;;

let raise_ error =
  let exn = to_exception error in
  raise exn
;;
