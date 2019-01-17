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
