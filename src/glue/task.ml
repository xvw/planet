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
    | [] ->
      Ok "TASK-1"
    | list -> (
      try
        list
        |> List.map (fun x -> Scanf.sscanf x "TASK-%d.qube" (fun x -> x))
        |> List.sort (fun a b -> Int.compare b a)
        |> List.hd
        |> succ
        |> Format.asprintf "TASK-%d"
        |> (fun x -> Ok x)
      with
      | _ ->
        Error (Of "unable to find task id")
    )
;;

let tasks () =
  let open Validation.Infix in
  files ()
  |> Validation.from_result
  >|= List.map (fun f ->
          File.to_stream (fun _ s -> Qexp.from_stream s) f
          |> Validation.from_result
          >>= Shapes.Task.from_qexp)
  >>= Validation.Applicative.sequence
;;
