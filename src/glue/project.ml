open Bedrock
open Util
open Paperwork
open Baremetal
module DB = Database

let database = DB.projects

let read ctx filename =
  let open Validation in
  filename
  |> Filename.concat (DB.path database)
  |> File.to_stream (fun _ -> Qexp.from_stream)
  |> from_result >>= Shapes.Project.from_qexp
  >|= (fun project ->
        let open Shapes.Context.Projects in
        ( project
        , Shapes.Update_table.fetch ctx.updates project.name
        , Hashtbl.find_opt ctx.projects project.name ))
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
  let open Validation.Infix in
  Log.read_project_updates ()
  |> Validation.from_result
  >>= fun table ->
  Log.traverse Shapes.Context.Projects.update
  $ Shapes.Context.Projects.init table
  >>= (fun ctx ->
        Dir.children
          ~filter:(flip String.has_extension "qube")
          (DB.path database)
        |> Validation.from_result
        >|= fun children -> children, ctx)
  >|= (fun (children, ctx) -> ctx, List.map (read ctx) children)
  >|= fun (ctx, list) ->
  ( ctx
  , List.sort
      (fun (p, _) (q, _) ->
        match p, q with
        | Error _, Error _ ->
          0
        | _, Error _ ->
          -1
        | Error _, _ ->
          1
        | Ok (_, x, _), Ok (_, y, _) ->
          may_sort (x, y))
      list )
;;

let all () =
  let open Validation.Infix in
  inspect ()
  >|= (fun (ctx, x) -> ctx, List.map fst x)
  >>= fun (ctx, x) ->
  x |> Validation.Applicative.sequence >|= fun list -> ctx, list
;;

let to_json () =
  let open Validation.Infix in
  all ()
  >|= fun projects ->
  Json.obj
  $ List.map
      (fun (project, _, _) ->
        Shapes.Project.(project.name, to_json project))
      (snd projects)
;;

let fetch_project_content content =
  let open Shapes.Text in
  match content with
  | Plain str ->
    Ok str
  | File filename ->
    File.to_string filename
;;

let fetch_project_text project =
  let open Shapes.Project in
  match project.content with
  | None ->
    Ok ("org", "")
  | Some ((_format, t) as k) ->
    let open Result.Syntax in
    let+ content = fetch_project_content t in
    Shapes.Text.extension_for k, content
;;

let as_textarea =
  Format.asprintf {|<textarea data-planet-qexp="%s">%s</textarea>|}
;;

let to_hakyll_string_aux day project_opt project =
  let open Shapes.Project in
  let open Result.Syntax in
  let+ ext, body = fetch_project_text project in
  let pstring =
    project |> Shapes.Project.to_qexp |> Paperwork.Qexp.to_string
  in
  let render_picto = function
    | None ->
      Hakyll.render_string "pictogram" "unknown"
    | Some x ->
      Hakyll.render_string "pictogram" x
  in
  let header =
    Hakyll.(
      join
        [ render_string "title" project.title
        ; render_string "displayable_title" project.title
        ; render_string "name" project.name
        ; render_string "synopsis" project.synopsis
        ; may_render_with_format
            ~default:"2019-01-01"
            Timetable.Day.ppr
            "date"
            day
        ; may_render_with_format
            ~default:"019A01"
            Timetable.Day.pp
            "date_planet"
            day
        ; render
            "status"
            Shapes.Project.status_to_string
            project.status
        ; render_picto project.picto
        ; may_render "repo" Shapes.Repo.repr project.repo
        ; may_render "repo_url" Shapes.Repo.base_url project.repo
        ; may_render "license" id project.license
        ; render_if "indexed" project.indexed
        ; render_if "published" project.published
        ; render_string
            "qexp_partial"
            (Format.asprintf
               "_seeds/partials/%s.qexp.html"
               project.name)
        ])
  in
  let content = header ^ body in
  ( project
  , ext
  , content
  , as_textarea "project" pstring
    ^
    match project_opt with
    | None ->
      ""
    | Some metadata ->
      let str =
        Shapes.Context.Projects.project_to_qexp project.name metadata
        |> Paperwork.Qexp.to_string
      in
      "\n" ^ as_textarea "project_timedata" str )
;;

let to_hakyll_string (project, day, project_opt) =
  project
  |> to_hakyll_string_aux day project_opt
  |> Validation.from_result
;;
