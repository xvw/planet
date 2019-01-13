type t = Unknown of string

module Exn = struct
  type t = exn

  exception Unknown of string
end

let to_exception = function Unknown message -> Exn.Unknown message

let from_exception = function
  | Exn.Unknown message ->
    Unknown message
  | _ ->
    Unknown "unsupported exception"
;;

let to_string = function
  | Unknown message ->
    Format.sprintf "[Unknown] %s" message
;;

let raise_ error =
  let exn = to_exception error in
  raise exn
;;
