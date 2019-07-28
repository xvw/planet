open Bedrock
open Util
open Paperwork
open Baremetal
module DB = Database

let database = DB.projects

let read table filename =
  let open Validation in
  filename
  |> Filename.concat (DB.path database)
  |> File.to_stream (fun _ -> Qexp.from_stream)
  |> from_result >>= Shapes.Project.from_qexp
  >|= (fun project ->
        project, Shapes.Update_table.fetch table project.name)
  |> fun x -> x, filename
;;

let may_sort = function
  | None, None ->
    0
  | _, None ->
    -1
  | None, _ ->
    1
  | Some x, Some y ->
    Timetable.Day.cmp y x
;;

let inspect () =
  let open Result.Infix in
  Log.read_project_updates ()
  >>= fun table ->
  Dir.children ~filter:(flip String.has_extension "qube")
  $ DB.path database
  >|= List.map (read table)
  >|= List.sort (fun (p, _) (q, _) ->
          match p, q with
          | Error _, Error _ ->
            0
          | _, Error _ ->
            -1
          | Error _, _ ->
            1
          | Ok (_, x), Ok (_, y) ->
            may_sort (x, y))
;;

let all () =
  let open Result.Infix in
  inspect () >|= List.map fst >|= Validation.Applicative.sequence
  |> Validation.from_result |> Validation.join
;;

let to_json () =
  let open Validation.Infix in
  all ()
  >|= fun projects ->
  Json.obj
  $ List.map
      (fun (project, _) ->
        Shapes.Project.(project.name, to_json project))
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

let to_hakyll_string_aux day project =
  let open Shapes.Project in
  let open Result.Syntax in
  let+ ext, body = fetch_project_text project in
  let header =
    Hakyll.(
      join
        [ render_string "title" project.title
        ; render_string "name" project.name
        ; render_string "synopsis" project.synopsis
        ; may_render_date ~default:"2019-01-01" "date" day
        ; render
            "status"
            Shapes.Project.status_to_string
            project.status
        ; may_render "pictogram" id project.picto
        ; may_render "repo" id project.repo
        ; may_render "license" id project.license
        ; render_if "indexed" project.indexed
        ; render_if "published" project.published
        ])
  in
  let content = header ^ body in
  project, ext, content
;;

let to_hakyll_string (project, day) =
  project |> to_hakyll_string_aux day |> Validation.from_result
;;
