(** All Errors wrapped into a single module, using Variants. 
    It is a conveinent approach to have a 1-arity [Result] 
    type.
*)

(** {2 Types and exception} *)

(** Describe the list of all errors *)
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
  | Not_a_valid_node of string
  | Unknown_format of string
  | Undefined_field of string
  | Invalid_field of string
  | Invalid_text_scheme
  | Unknown_status of string
  | Mapping_failure of (string * string)
  | Unparsable_color of string
  | Unix of string
  | Wexited of int
  | Wsignaled of int
  | Wstopped of int
  | Exn of exn
  | List of t list

(** Each errors has a corresponding exception *)
module Exn : sig
  type t = exn

  exception Unknown of string
  exception Unmatched_character of char
  exception Illegal_character of char
  exception Unclosed_string of string
  exception No_root_element of string
  exception Already_exists of string
  exception Invalid_attribute of string
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
  exception Not_a_valid_node of string
  exception List of t list
  exception Unknown_format of string
  exception Undefined_field of string
  exception Invalid_field of string
  exception Invalid_text_scheme
  exception Unknown_status of string
  exception Mapping_failure of (string * string)
  exception Unparsable_color of string
  exception Wexited of int
  exception Wsignaled of int
  exception Wstopped of int
end

(** {2 Tools to deal with errors} *)

(** Convert error to [Exception] *)
val to_exception : t -> Exn.t

(** Convert [Exception] to [Error.t] *)
val from_exception : Exn.t -> t

(** Convert [Error.t] to [string] *)
val to_string : t -> string

(** Raise an [Error.t] as an [Exn.t] *)
val raise_ : t -> 'a

(** {2 Tools for testing} *)

val pp : Format.formatter -> t -> unit
val eq : t -> t -> bool
