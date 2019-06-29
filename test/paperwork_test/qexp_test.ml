open Test_tools
open Paperwork
open Bedrock.Error

let parse_message l r = failwith (Format.sprintf "[%s] => [%s]" l r)

let parse_empty_data_1 () =
  let message = parse_message "()" in
  match Qexp.from_string ";" with
  | Ok Qexp.(Node []) ->
    ()
  | Ok qexp ->
    message (Qexp.to_string qexp)
  | Error err ->
    message (to_string err)
;;

let parse_empty_data_2 () =
  let message = parse_message "()" in
  match Qexp.from_string "     " with
  | Ok Qexp.(Node []) ->
    ()
  | Ok qexp ->
    message (Qexp.to_string qexp)
  | Error err ->
    message (to_string err)
;;

let parse_empty_data_3 () =
  let message = parse_message "()" in
  match Qexp.from_string "  \n   " with
  | Ok Qexp.(Node []) ->
    ()
  | Ok qexp ->
    message (Qexp.to_string qexp)
  | Error err ->
    message (to_string err)
;;

let parse_empty_data_4 () =
  let message = parse_message "()" in
  match Qexp.from_string ";foo bar baz" with
  | Ok Qexp.(Node []) ->
    ()
  | Ok qexp ->
    message (Qexp.to_string qexp)
  | Error err ->
    message (to_string err)
;;

let parse_empty_data_5 () =
  let message = parse_message "()" in
  match Qexp.from_string ";foo bar baz\n    \n;fooo bae" with
  | Ok Qexp.(Node []) ->
    ()
  | Ok qexp ->
    message (Qexp.to_string qexp)
  | Error err ->
    message (to_string err)
;;

let parse_simple_atom_1 () =
  let message = parse_message "foo" in
  match Qexp.from_string "foo" with
  | Ok Qexp.(Node [ Atom "foo" ]) ->
    ()
  | Ok qexp ->
    message (Qexp.to_string qexp)
  | Error err ->
    message (to_string err)
;;

let parse_simple_atom_2 () =
  let message = parse_message "foo" in
  match Qexp.from_string ";comment\nfoo;comment" with
  | Ok Qexp.(Node [ Atom "foo" ]) ->
    ()
  | Ok qexp ->
    message (Qexp.to_string qexp)
  | Error err ->
    message (to_string err)
;;

let parse_simple_tag_1 () =
  let message = parse_message ":foo" in
  match Qexp.from_string ":foo" with
  | Ok Qexp.(Node [ Tag "foo" ]) ->
    ()
  | Ok qexp ->
    message (Qexp.to_string qexp)
  | Error err ->
    message (to_string err)
;;

let parse_simple_tag_2 () =
  let message = parse_message ":foo" in
  match Qexp.from_string ";comment\n  :foo;comment" with
  | Ok Qexp.(Node [ Tag "foo" ]) ->
    ()
  | Ok qexp ->
    message (Qexp.to_string qexp)
  | Error err ->
    message (to_string err)
;;

let parse_simple_kwd_1 () =
  let message = parse_message "#foo" in
  match Qexp.from_string "#foo" with
  | Ok Qexp.(Node [ Keyword "foo" ]) ->
    ()
  | Ok qexp ->
    message (Qexp.to_string qexp)
  | Error err ->
    message (to_string err)
;;

let parse_simple_kwd_2 () =
  let message = parse_message "#foo" in
  match Qexp.from_string ";comment\n  #foo;comment" with
  | Ok Qexp.(Node [ Keyword "foo" ]) ->
    ()
  | Ok qexp ->
    message (Qexp.to_string qexp)
  | Error err ->
    message (to_string err)
;;

let parse_complex_expression () =
  let str =
    {|
    (#hello "world") 
    (:foo `bar`)
    (`bar` "baz")
    (node {
      foo (bar (baz))
    })
|}
  in
  match Qexp.from_string str with
  | Ok
      Qexp.(
        Node
          [ Node [ Keyword "hello"; String (Double, "world") ]
          ; Node [ Tag "foo"; String (Backtick, "bar") ]
          ; Node [ String (Backtick, "bar"); String (Double, "baz") ]
          ; Node
              [ Atom "node"
              ; Block
                  [ Atom "foo"
                  ; Node [ Atom "bar"; Node [ Atom "baz" ] ]
                  ]
              ]
          ]) ->
    ()
  | Ok qexp ->
    parse_message str (Qexp.to_string qexp)
  | Error err ->
    parse_message str (to_string err)
;;

let parse_failure_1 () =
  match Qexp.from_string ") foo" with
  | Error (Unmatched_character ')') ->
    ()
  | _ ->
    failwith "Should fail with unmatched parenthesis"
;;

let parse_failure_2 () =
  match Qexp.from_string "foo (bar" with
  | Error (Unmatched_character '(') ->
    ()
  | _ ->
    failwith "Should fail with unmatched parenthesis"
;;

let parse_failure_3 () =
  match Qexp.from_string "foo (bar ()" with
  | Error (Unmatched_character '(') ->
    ()
  | _ ->
    failwith "Should fail with unmatched parenthesis"
;;

let parse_failure_4 () =
  match Qexp.from_string "foo (bar {)}" with
  | Error (Unmatched_character '{') ->
    ()
  | _ ->
    failwith "Should fail with unmatched brace"
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
      parse_empty_data_5
  ; test "[from_string] Parse simple atom" parse_simple_atom_1
  ; test
      "[from_string] Parse simple atom with comments"
      parse_simple_atom_2
  ; test "[from_string] Parse simple tag" parse_simple_tag_1
  ; test
      "[from_string] Parse simple tag with comments"
      parse_simple_tag_2
  ; test "[from_string] Parse simple keyword" parse_simple_kwd_1
  ; test
      "[from_string] Parse simple keyword with comments"
      parse_simple_kwd_2
  ; test "[from_string] Parse complex Qexp" parse_complex_expression
  ; test
      "[from_string] Invalid Qexp, closed unopened parenthesis"
      parse_failure_1
  ; test
      "[from_string] Invalid Qexp, unclosed parenthesis 1"
      parse_failure_2
  ; test
      "[from_string] Invalid Qexp, unclosed parenthesis 2"
      parse_failure_3
  ; test
      "[from_string] Invalid Qexp, invalid brace/parenthesis"
      parse_failure_4
  ]
;;
