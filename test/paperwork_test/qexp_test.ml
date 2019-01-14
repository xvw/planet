open Test_tools
open Paperwork
open Bedrock.Error

let parse_message l r = failwith (Format.sprintf "[%s] => [%s]" l r)

let parse_empty_data_1 () =
  match Qexp.from_string "foo" with
  | Ok Qexp.(Node []) ->
    ()
  | Ok qexp ->
    parse_message "()" (Qexp.to_string qexp)
  | Error err ->
    parse_message "()" (to_string err)
;;

let suite = [test "[from_string] Parse empty node" parse_empty_data_1]
