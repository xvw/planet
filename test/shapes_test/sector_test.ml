open Bedrock
open Paperwork
open Shapes
open Test_tools
open Error

let invalid1 () =
  let qexp = Qexp.(node [node [tag "name"; string "test"]]) in
  match Sector.from_qexp qexp with
  | Ok _ ->
    failwith "should be invalid"
  | Error [Undefined_field "desc"; Undefined_field "color"] ->
    ()
  | _ ->
    failwith "Invalid error"
;;

let invalid2 () =
  let qexp =
    Qexp.(
      node
        [ node [tag "name"; string "test"]
        ; node [tag "color"; string "#FF0000"] ])
  in
  match Sector.from_qexp qexp with
  | Ok _ ->
    failwith "should be invalid"
  | Error [Undefined_field "desc"] ->
    ()
  | _ ->
    failwith "Invalid error"
;;

let invalid3 () =
  let qexp =
    Qexp.(
      node
        [ node [tag "name"; string "test"]
        ; node [tag "desc"; string "A description"] ])
  in
  match Sector.from_qexp qexp with
  | Ok _ ->
    failwith "should be invalid"
  | Error [Undefined_field "color"] ->
    ()
  | _ ->
    failwith "Invalid error"
;;

let invalid4 () =
  let qexp =
    Qexp.(
      node
        [ node [tag "name"; string "test"]
        ; node [tag "color"; string "#FF"]
        ; node [tag "desc"; string "A description"] ])
  in
  match Sector.from_qexp qexp with
  | Ok _ ->
    failwith "should be invalid"
  | Error [Unparsable_color "#FF"] ->
    ()
  | _ ->
    failwith "Invalid error"
;;

let valid1 () =
  let qexp =
    Qexp.(
      node
        [ node [tag "name"; string "test"]
        ; node [tag "color"; string "#FFFFFF"]
        ; node [tag "desc"; string "A description"] ])
  in
  match Sector.from_qexp qexp with
  | Ok _ ->
    ()
  | _ ->
    failwith "Should be valid"
;;

let valid2 () =
  let qexp =
    Qexp.(
      node
        [ node [tag "name"; string "test"]
        ; node [tag "color"; string "rgba(200, 23, 45, 0.6)"]
        ; node [tag "desc"; string "A description"] ])
  in
  match Sector.from_qexp qexp with
  | Ok _ ->
    ()
  | _ ->
    failwith "Should be valid"
;;

let suite =
  [ test "[from_qexp] with two missing fields" invalid1
  ; test "[from_qexp] with one missing fields" invalid2
  ; test "[from_qexp] with one missing fields" invalid3
  ; test "[from_qexp] with invalid fields" invalid4
  ; test "[from_qexp] with valid fields" valid1
  ; test "[from_qexp] with valid fields 2" valid2 ]
;;
