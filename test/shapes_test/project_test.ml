open Bedrock
open Paperwork
open Shapes
open Test_tools
open Error

let invalid1 () =
  let q = Qexp.(node [ node [ string "name"; string "foobar" ] ]) in
  match Project.from_qexp q with
  | Ok _ -> failwith "Invalid Project"
  | Error
      [ Undefined_field "title"; Undefined_field "synopsis"; Undefined_field "status" ] ->
    ()
  | Error xs ->
    let s = String.concat "\n" (List.map to_string xs) in
    failwith s
;;

let invalid2 () =
  let q = Qexp.(node [ node [ string "name"; node [ string "foobar" ] ] ]) in
  match Project.from_qexp q with
  | Ok _ -> failwith "Invalid Project"
  | Error
      [ Invalid_field "name"
      ; Undefined_field "title"
      ; Undefined_field "synopsis"
      ; Undefined_field "status"
      ] -> ()
  | Error xs ->
    let s = String.concat "\n" (List.map to_string xs) in
    failwith s
;;

let invalid3 () =
  let q =
    Qexp.(
      node
        [ node [ string "name"; string "foobar" ]
        ; node [ string "title"; string "foobar project" ]
        ; node [ string "synopsis"; string "A foobar project" ]
        ; node [ string "status"; tag "wipz" ]
        ])
  in
  match Project.from_qexp q with
  | Ok _ -> failwith "Invalid Project"
  | Error [ Unknown_status "wipz" ] -> ()
  | Error xs ->
    let s = String.concat "\n" (List.map to_string xs) in
    failwith s
;;

let valid1 () =
  let q =
    Qexp.(
      node
        [ node [ string "name"; string "foobar" ]
        ; node [ string "title"; string "foobar project" ]
        ; node [ string "synopsis"; string "A foobar project" ]
        ; node [ string "status"; tag "wip" ]
        ])
  in
  match Project.from_qexp q with
  | Ok _ -> ()
  | Error xs ->
    let s = String.concat "\n" (List.map to_string xs) in
    failwith s
;;

let suite =
  [ test "[from_qexp] with missing fields 1" invalid1
  ; test "[from_qexp] with missing fields 2" invalid2
  ; test "[from_qexp] with missing fields 3" invalid3
  ; test "[from_qexp] valid case 1" valid1
  ]
;;
