type t =
  | Of of string
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
  | Not_a_valid_node of string
  | Unknown_format of string
  | Undefined_field of string
  | Invalid_field of string
  | Invalid_text_scheme
  | Unknown_status of string
  | Unknown_kind of string
  | Mapping_failure of (string * string)
  | Unparsable_color of string
  | Unix of string
  | Wexited of int
  | Wsignaled of int
  | Wstopped of int
  | Exn of exn
  | List of t list

module Exn = struct
  type t = exn

  exception Of of string
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
  exception Not_a_valid_node of string
  exception Unix of string
  exception List of t list
  exception Unknown_format of string
  exception Undefined_field of string
  exception Invalid_field of string
  exception Invalid_text_scheme
  exception Unknown_status of string
  exception Unknown_kind of string
  exception Mapping_failure of (string * string)
  exception Unparsable_color of string
  exception Wexited of int
  exception Wsignaled of int
  exception Wstopped of int
end

let rec to_exception = function
  | Of message ->
    Exn.Of message
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
  | Not_a_valid_node string ->
    Exn.Not_a_valid_node string
  | Unknown_format string ->
    Exn.Unknown_format string
  | Undefined_field string ->
    Exn.Undefined_field string
  | Invalid_field string ->
    Exn.Invalid_field string
  | Unix string ->
    Exn.Unix string
  | Invalid_text_scheme ->
    Exn.Invalid_text_scheme
  | Unknown_status string ->
    Exn.Unknown_status string
  | Unknown_kind string ->
    Exn.Unknown_kind string
  | Exn exn ->
    exn
  | Mapping_failure (subject, content) ->
    Exn.Mapping_failure (subject, content)
  | Unparsable_color string ->
    Exn.Unparsable_color string
  | Wexited i ->
    Exn.Wexited i
  | Wsignaled i ->
    Exn.Wsignaled i
  | Wstopped i ->
    Exn.Wstopped i
  | List errors ->
    Exn.List (List.map to_exception errors)
;;

let rec from_exception = function
  | Exn.Of message ->
    Of message
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
  | Exn.Unknown_format string ->
    Unknown_format string
  | Exn.Invalid_field string ->
    Invalid_field string
  | Exn.Undefined_field string ->
    Undefined_field string
  | Exn.List errors ->
    List (List.map from_exception errors)
  | Exn.Invalid_text_scheme ->
    Invalid_text_scheme
  | Exn.Unknown_status string ->
    Unknown_status string
  | Exn.Unparsable_color string ->
    Unparsable_color string
  | Exn.Mapping_failure (subject, content) ->
    Mapping_failure (subject, content)
  | Exn.Wexited i ->
    Wexited i
  | Exn.Wsignaled i ->
    Wsignaled i
  | Exn.Wstopped i ->
    Wstopped i
  | e ->
    Exn e
;;

let rec to_string = function
  | Of message ->
    Format.sprintf "[Error] %s" message
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
  | Not_a_valid_node string ->
    Format.sprintf "[Not_a_valid_node] [%s]" string
  | Unknown_format string ->
    Format.sprintf "[Unknown_format] [%s]" string
  | Undefined_field string ->
    Format.sprintf "[Undefined_field] [%s]" string
  | Invalid_field string ->
    Format.sprintf "[Invalid_field] [%s]" string
  | Unix string ->
    Format.sprintf "[Unix error] %s" string
  | Unknown_status string ->
    Format.sprintf "[Unknown_status] %s" string
  | Unknown_kind string ->
    Format.sprintf "[Unknown_kind] %s" string
  | Invalid_text_scheme ->
    "[Invalid_text_scheme]"
  | Exn e ->
    Format.sprintf "[Exception: %s]" (Printexc.to_string e)
  | Mapping_failure (subject, content) ->
    Format.sprintf "[Mapping_failure] [%s] [%s]" subject content
  | Unparsable_color string ->
    Format.sprintf "[Unparsable_color] [%s]" string
  | Wexited i ->
    Format.sprintf "[WEXITED] [%d]" i
  | Wsignaled i ->
    Format.sprintf "[WSIGNALED] [%d]" i
  | Wstopped i ->
    Format.sprintf "[WSTOPPED] [%d]" i
  | List errors ->
    Format.sprintf
      "[List] %s"
      (List.map to_string errors |> String.concat "; ")
;;

let raise_ error =
  let exn = to_exception error in
  raise exn
;;

let pp ppf e =
  let str = to_string e in
  let f = Format.sprintf "Error(%s)" str in
  Format.fprintf ppf "%s" f
;;

let eq a b =
  (* Cheap comparison *)
  let x = to_string a in
  let y = to_string b in
  x = y
;;
