open Bedrock
open Util
open Paperwork
open Baremetal
module DB = Database

let database = DB.projects

let read filename =
  let open Validation in
  filename
  |> Filename.concat (DB.path database)
  |> File.to_stream (fun _ -> Qexp.from_stream)
  |> from_result >>= Shapes.Project.from_qexp
  |> fun x -> x, filename
;;

let inspect () =
  let open Result.Monad in
  Dir.children ~filter:(flip String.has_extension "qube")
  $ DB.path database >|= List.map read
;;
