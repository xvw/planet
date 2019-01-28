open Bedrock
open Paperwork
open Shapes
open Test_tools
open Error

let invalid1 () =
  let q = Qexp.(node [node [string "name"; string "foobar"]]) in
  match Project.from_qexp q with
  | Ok _ ->
    failwith "Invalid Project"
  | Error
      [ Undefined_field "title"
      ; Undefined_field "synopsis"
      ; Undefined_field "status" ] ->
    ()
  | Error xs ->
    let s = String.concat "\n" (List.map to_string xs) in
    failwith s
;;

let invalid2 () =
  let q =
    Qexp.(node [node [string "name"; node [string "foobar"]]])
  in
  match Project.from_qexp q with
  | Ok _ ->
    failwith "Invalid Project"
  | Error
      [ Invalid_field "name"
      ; Undefined_field "title"
      ; Undefined_field "synopsis"
      ; Undefined_field "status" ] ->
    ()
  | Error xs ->
    let s = String.concat "\n" (List.map to_string xs) in
    failwith s
;;

let suite =
  [ test "[from_qexp] with missing fields 1" invalid1
  ; test "[from_qexp] with missing fields 1" invalid2 ]
;;
