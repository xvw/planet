open Bedrock
open Baremetal
open Paperwork

let database = Database.stories
let folder = Database.path database

let collect () =
  let open Validation in
  Dir.children
    ~filter:(fun x -> String.has_extension x "qube")
    folder
  |> from_result
  >>= fun children ->
  List.map
    (fun filename ->
      filename |> Filename.concat folder
      |> File.to_stream (fun _ -> Qexp.from_stream)
      |> from_result >>= Shapes.Story.from_qexp)
    children
  |> Validation.Applicative.sequence
;;
