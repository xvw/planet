open Bedrock
open Bedrock.Util
open Baremetal

let site_folder = "./_seeds"
let api_folder = Filename.concat site_folder "api"
let project_folder = Filename.concat site_folder "projects"
let seed_partials = Filename.concat site_folder "partials"

let soft_creation folder =
  let open Result.Infix in
  (if not (Dir.exists folder)
  then Dir.make folder >> Ok (true, folder)
  else Ok (false, folder))
  |> Validation.from_result
;;

let soft_deletion_file filename =
  let open Result.Infix in
  (if File.exists filename
  then File.delete filename >> Ok (true, filename)
  else Ok (false, filename))
  |> Validation.from_result
;;

let init () = soft_creation site_folder

let trace action message = function
  | Error errs ->
    Prompter.prompt_errors errs
  | Ok (x, filename) ->
    (if x
    then
      Ansi.
        [ fg green
        ; text
          $ Format.sprintf
              "%s [%s] has been %s"
              action
              filename
              message
        ]
    else
      Ansi.
        [ fg yellow
        ; text
          $ Format.sprintf "%s [%s] Nothing to do" action filename
        ])
    |> Ansi.to_string |> print_endline
;;

let trace_creation = trace "create" "created"
let trace_deletion = trace "delete" "deleted"
let generate () = trace_creation (init ())

let create_api_folder () =
  generate ();
  trace_creation (soft_creation api_folder)
;;

let create_projects_folder () =
  generate ();
  trace_creation (soft_creation project_folder)
;;

let create_partials () =
  generate ();
  trace_creation (soft_creation seed_partials)
;;

let create_file f folder file =
  let target = Filename.concat folder file in
  let () = trace_deletion (soft_deletion_file target) in
  let open Validation.Infix in
  f () >|= Paperwork.Json.to_string
  >>= (fun str -> File.create target str |> Validation.from_result)
  >|= (fun () -> true, target)
  |> trace_creation
;;

let initialize_api_project () =
  create_file Glue.Project.to_json api_folder "projects.json"
;;

let initialize_api_sectors () =
  create_file Glue.Sector.to_json api_folder "sectors.json"
;;

let initialize_api_current_position () =
  create_file Glue.Log.whereami_to_json api_folder "whereami.json"
;;

let initialize_logs () =
  create_file Glue.Log.collect_all_log_in_json api_folder "logs.json"
;;

let create_projects_files () =
  let open Validation.Infix in
  Glue.Project.all ()
  >|= List.map Glue.Project.to_hakyll_string
  >>= Validation.Applicative.sequence
  >>= fun elts ->
  List.map
    (fun (project, extension, content, project_str) ->
      let open Shapes.Project in
      let filename = project.name ^ "." ^ extension in
      let target = Filename.concat project_folder filename in
      let partial =
        Filename.concat seed_partials project.name ^ ".qexp.html"
      in
      let () = trace_deletion (soft_deletion_file target) in
      let () = trace_deletion (soft_deletion_file partial) in
      File.create target content
      |> Result.bind (fun () -> File.create partial project_str)
      |> Result.map (fun () -> true, target ^ " & " ^ partial)
      |> Validation.from_result |> trace_creation |> Validation.pure)
    elts
  |> Validation.Applicative.sequence
;;

let api () =
  let () = create_api_folder () in
  let () = initialize_api_project () in
  let () = initialize_api_sectors () in
  let () = initialize_api_current_position () in
  let () = initialize_logs () in
  ()
;;

let projects () =
  let () = create_projects_folder () in
  let () = create_partials () in
  match create_projects_files () with
  | Error e ->
    Prompter.prompt_errors e
  | Ok _ ->
    ()
;;

let all () =
  let () = api () in
  let () = projects () in
  ()
;;
