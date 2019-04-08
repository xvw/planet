open Bedrock
open Bedrock.Util
open Baremetal

let site_folder = "./_site"
let api_folder = Filename.concat site_folder "api"

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
              "%s [%s] has beed %s"
              action
              filename
              message ]
    else
      Ansi.
        [ fg yellow
        ; text
          $ Format.sprintf "%s [%s] Nothing to do" action filename ])
    |> Ansi.to_string |> print_endline
;;

let trace_creation = trace "create" "created"
let trace_deletion = trace "delete" "deleted"
let generate () = trace_creation (init ())

let create_api_folder () =
  generate ();
  trace_creation (soft_creation api_folder)
;;

let create_api_file f folder file =
  let target = Filename.concat folder file in
  let () = trace_deletion (soft_deletion_file target) in
  let open Validation.Infix in
  f () >|= Paperwork.Json.to_string
  >>= (fun str -> File.create target str |> Validation.from_result)
  >|= (fun () -> true, target)
  |> trace_creation
;;

let initialize_project () =
  create_api_file Glue.Project.to_json api_folder "projects.json"
;;

let initialize_sectors () =
  create_api_file Glue.Sector.to_json api_folder "sectors.json"
;;

let initialize_current_position () =
  create_api_file
    Glue.Log.whereami_to_json
    api_folder
    "whereami.json"
;;

let initialize_logs () =
  create_api_file
    Glue.Log.collect_all_log_in_json
    api_folder
    "logs.json"
;;

let api () =
  let () = create_api_folder () in
  let () = initialize_project () in
  let () = initialize_sectors () in
  let () = initialize_current_position () in
  let () = initialize_logs () in
  ()
;;
