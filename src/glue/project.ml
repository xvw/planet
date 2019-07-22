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

let all () =
  let open Result.Infix in
  inspect () >|= List.map fst >|= Validation.Applicative.sequence
  |> Validation.from_result |> Validation.join
  |> Validation.map (List.sort Shapes.Project.compare_date)
;;

let to_json () =
  let open Validation.Infix in
  all ()
  >|= fun projects ->
  Json.obj
  $ List.map
      (fun project -> Shapes.Project.(project.name, to_json project))
      projects
;;

let fetch_project_content content =
  let open Shapes.Text in
  match content with
  | Plain str ->
    Ok str
  | File filename ->
    File.to_string filename
;;

let fetch_project_format = function
  | Shapes.Text.Format.Raw ->
    "txt"
  | Shapes.Text.Format.Org ->
    "org"
  | Shapes.Text.Format.Markdown ->
    "md"
;;

let fetch_project_text project =
  let open Shapes.Project in
  match project.content with
  | None ->
    Ok ("org", "")
  | Some (format, t) ->
    let open Result.Syntax in
    let* content = fetch_project_content t in
    Ok (fetch_project_format format, content)
;;

let to_hakyll_string_aux project =
  let open Format in
  let open Shapes.Project in
  let open Result.Syntax in
  let+ ext, body = fetch_project_text project in
  let content =
    "---\n"
    ^ asprintf "title: %s\n" project.title
    ^ asprintf "name: %s\n" project.name
    ^ asprintf "synopsis: %s\n" project.synopsis
    ^ "---\n" ^ body
  in
  project, ext, content
;;

let to_hakyll_string project =
  project |> to_hakyll_string_aux |> Validation.from_result
;;
