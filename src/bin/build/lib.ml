open Bedrock
open Bedrock.Util
open Baremetal

let site_folder = "./_site"
let api_folder = Filename.concat site_folder "api"
let logs_folder = Filename.concat api_folder "logs"

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

let initialize_project () =
  let target = Filename.concat api_folder "projects.json" in
  let () = trace_deletion (soft_deletion_file target) in
  let open Validation.Infix in
  Glue.Project.to_json () >|= Paperwork.Json.to_string
  >>= (fun str -> File.create target str |> Validation.from_result)
  >|= (fun () -> true, target)
  |> trace_creation
;;

let initialize_sectors () =
  let target = Filename.concat api_folder "sectors.json" in
  let () = trace_deletion (soft_deletion_file target) in
  let open Validation.Infix in
  Glue.Sector.to_json () >|= Paperwork.Json.to_string
  >>= (fun str -> File.create target str |> Validation.from_result)
  >|= (fun () -> true, target)
  |> trace_creation
;;

let api () =
  let () = create_api_folder () in
  let () = initialize_project () in
  let () = initialize_sectors () in
  ()
;;

let logs () =
  create_api_folder ();
  trace_creation (soft_creation logs_folder)
;;
