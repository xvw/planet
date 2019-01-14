open Test_tools
open Paperwork
open Bedrock.Error

let parse_message l r = failwith (Format.sprintf "[%s] => [%s]" l r)

let parse_empty_data_1 () =
  match Qexp.from_string "" with
  | Ok Qexp.(Node []) ->
    ()
  | Ok qexp ->
    parse_message "()" (Qexp.to_string qexp)
  | Error err ->
    parse_message "()" (to_string err)
;;

let parse_empty_data_2 () =
  match Qexp.from_string "     " with
  | Ok Qexp.(Node []) ->
    ()
  | Ok qexp ->
    parse_message "()" (Qexp.to_string qexp)
  | Error err ->
    parse_message "()" (to_string err)
;;

let parse_empty_data_3 () =
  match Qexp.from_string "  \n   " with
  | Ok Qexp.(Node []) ->
    ()
  | Ok qexp ->
    parse_message "()" (Qexp.to_string qexp)
  | Error err ->
    parse_message "()" (to_string err)
;;

let parse_empty_data_4 () =
  match Qexp.from_string ";foo bar baz" with
  | Ok Qexp.(Node []) ->
    ()
  | Ok qexp ->
    parse_message "()" (Qexp.to_string qexp)
  | Error err ->
    parse_message "()" (to_string err)
;;

let parse_empty_data_5 () =
  match Qexp.from_string ";foo bar baz\n    \n;fooo bae" with
  | Ok Qexp.(Node []) ->
    ()
  | Ok qexp ->
    parse_message "()" (Qexp.to_string qexp)
  | Error err ->
    parse_message "()" (to_string err)
;;

let suite =
  [ test "[from_string] Parse empty node" parse_empty_data_1
  ; test
      "[from_string] Parse empty node with spaces"
      parse_empty_data_2
  ; test
      "[from_string] Parse empty node with new lines"
      parse_empty_data_3
  ; test
      "[from_string] Parse empty node with comment"
      parse_empty_data_4
  ; test
      "[from_string] Parse empty node with comment and newlines"
      parse_empty_data_5 ]
;;
