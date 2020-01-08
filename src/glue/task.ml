module U = Util
open Bedrock
open Baremetal
open Error
open Paperwork

let database = Database.tasks
let folder = Database.path database
let files () = Dir.children folder

let fresh_id () =
  let open Result.Infix in
  files ()
  >>= function
  | [] -> Ok "TASK-1"
  | list ->
    (try
       list
       |> List.map (fun x -> Scanf.sscanf x "TASK-%d.qube" (fun x -> x))
       |> List.sort (fun a b -> Int.compare b a)
       |> List.hd
       |> succ
       |> Format.asprintf "TASK-%d"
       |> fun x -> Ok x
     with
    | _ -> Error (Of "unable to find task id"))
;;

let tasks () =
  let open Validation.Infix in
  files ()
  |> Validation.from_result
  >|= List.map (fun fname ->
          let f = Filename.concat folder fname in
          File.to_stream (fun _ s -> Qexp.from_stream s) f
          |> Validation.from_result
          >>= Shapes.Task.from_qexp)
  >>= Validation.Applicative.sequence
;;

let init project sectors name description checklist tags engagement_date =
  let open Result.Syntax in
  let* id = fresh_id () in
  let* date = U.day () in
  let+ result =
    Ok
      Shapes.Task.
        { state = Backlog
        ; uuid = id
        ; project
        ; sectors
        ; name
        ; description
        ; checklist = List.map (fun x -> false, x) checklist
        ; tags
        ; date
        ; opening_date = None
        ; closing_date = None
        ; engagement_date
        }
  in
  result
;;

let tasks_to_json () =
  let open Validation.Infix in
  tasks () >|= Shapes.Task.board_create >|= Shapes.Task.board_to_json
;;
