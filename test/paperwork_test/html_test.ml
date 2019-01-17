open Bedrock
open Paperwork
open Test_tools

let parse_attributes_1 () =
  let open Result.Monad in
  let result = "" |> Qexp.from_string >>= Html.process_attributes in
  match result with
  | Ok [] ->
    ()
  | Ok _ ->
    failwith "Attributes should be empty"
  | Error err ->
    failwith (Error.to_string err)
;;

let parse_attributes_2 () =
  let open Result.Monad in
  let result =
    "leaf (foo bar) baz" |> Qexp.from_string
    >>= Html.process_attributes
  in
  match result with
  | Ok Html.([Flag "leaf"; Pair ("foo", "bar"); Flag "baz"]) ->
    ()
  | Ok _ ->
    failwith "Attributes list is invalid"
  | Error err ->
    failwith (Error.to_string err)
;;

let parse_attributes_3 () =
  let open Result.Monad in
  let result =
    "#leaf (:foo \"bar\") baz (foo bar-baz) (x `yze`)"
    |> Qexp.from_string >>= Html.process_attributes
  in
  match result with
  | Ok
      Html.([ Flag "leaf"
            ; Pair ("foo", "bar")
            ; Flag "baz"
            ; Pair ("foo", "bar-baz")
            ; Pair ("x", "yze") ]) ->
    ()
  | Ok _ ->
    failwith "Attributes list is invalid"
  | Error err ->
    failwith (Error.to_string err)
;;

let suite =
  [ test
      "[proccess_attributes] Test for empty node"
      parse_attributes_1
  ; test
      "[proccess_attributes] Test for with various values"
      parse_attributes_2
  ; test
      "[proccess_attributes] Test for with various values 2"
      parse_attributes_3 ]
;;
