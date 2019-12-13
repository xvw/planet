open Bedrock
open Bedrock.Util
open Baremetal

let site_folder = "./_seeds"
let api_folder = Filename.concat site_folder "api"
let project_folder = Filename.concat site_folder "projects"
let seed_partials = Filename.concat site_folder "partials"
let long_folder = Filename.concat site_folder "longs"
let short_folder = Filename.concat site_folder "shorts"
let twtxt_folder = Filename.concat site_folder "twtxt"

let soft_creation folder =
  let open Result.Infix in
  ( if not (Dir.exists folder) then
      Dir.make folder >> Ok (true, folder)
  else
    Ok (false, folder)
  )
  |> Validation.from_result
;;

let soft_deletion_file filename =
  let open Result.Infix in
  ( if File.exists filename then
      File.delete filename >> Ok (true, filename)
  else
    Ok (false, filename)
  )
  |> Validation.from_result
;;

let init () = soft_creation site_folder

let trace action message = function
  | Error errs ->
    Prompter.prompt_errors errs
  | Ok (x, filename) ->
    ( if x then
        Ansi.
          [ fg green
          ; text $ Format.sprintf "%s [%s] has been %s" action filename message
          ]
    else
      Ansi.
        [ fg yellow
        ; text $ Format.sprintf "%s [%s] Nothing to do" action filename
        ]
    )
    |> Ansi.to_string
    |> print_endline
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

let create_stories_folder () =
  generate ();
  trace_creation (soft_creation long_folder);
  trace_creation (soft_creation short_folder)
;;

let create_file f folder file =
  let target = Filename.concat folder file in
  let () = trace_deletion (soft_deletion_file target) in
  let open Validation.Infix in
  f ()
  >|= Paperwork.Json.to_string
  >>= (fun str -> File.create target str |> Validation.from_result)
  >|= (fun () -> (true, target))
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
  let open Validation.Infix in
  let list =
    Glue.Log.traverse ~reverse:true
      (fun acc log -> acc @ [ Shapes.Log.to_json log ])
      [] in
  let fragment = list >|= List.hds 5 in
  let () =
    create_file (fun () -> list >|= Paperwork.Json.array) api_folder "logs.json"
  in
  create_file
    (fun () -> fragment >|= Paperwork.Json.array)
    api_folder "last_logs.json"
;;

let create_projects_files ?rctx () =
  let open Validation.Infix in
  Glue.Project.all ?rctx ()
  >>= (fun (ctx, projects) ->
        Glue.Log.push_project_updates Shapes.Context.Projects.(ctx.updates)
        |> Validation.from_result
        >|= (fun () -> List.map Glue.Project.to_hakyll_string projects))
  >>= Validation.Applicative.sequence
  >>= fun elts ->
  List.map
    (fun (project, extension, content, project_str) ->
      let open Shapes.Project in
      let filename = project.name ^ "." ^ extension in
      let target = Filename.concat project_folder filename in
      let partial = Filename.concat seed_partials project.name ^ ".qexp.html" in
      let () = trace_deletion (soft_deletion_file target) in
      let () = trace_deletion (soft_deletion_file partial) in
      File.create target content
      |> Result.bind (fun () -> File.create partial project_str)
      |> Result.map (fun () -> (true, target ^ " & " ^ partial))
      |> Validation.from_result
      |> trace_creation
      |> Validation.pure)
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

let projects ?rctx () =
  let () = create_projects_folder () in
  let () = create_partials () in
  match create_projects_files ?rctx () with
  | Error e ->
    Prompter.prompt_errors e
  | Ok _ ->
    ()
;;

let generation_id () =
  let open Validation.Infix in
  let () = create_partials () in
  let partial = Filename.concat seed_partials "generation_id.meta.html" in
  let () = trace_deletion (soft_deletion_file partial) in
  let str =
    Shapes.Metahtml.to_html [ "generation-id-data" ]
      [ ("uuid", Baremetal.Uuid.(make () |> to_string)) ] in
  File.create partial str
  |> Validation.from_result
  >|= (fun () -> (true, partial))
  |> trace_creation
;;

let copyright_date () =
  let open Validation.Infix in
  let () = create_partials () in
  let partial = Filename.concat seed_partials "copyright_end.html" in
  let () = trace_deletion (soft_deletion_file partial) in
  let str = string_of_int (Glue.Util.current_year ()) in
  File.create partial str
  |> Validation.from_result
  >|= (fun () -> (true, partial))
  |> trace_creation
;;

let context ctx =
  let open Validation.Infix in
  let () = create_partials () in
  let partial = Filename.concat seed_partials "planet_context.meta.html" in
  let () = trace_deletion (soft_deletion_file partial) in
  let str =
    Format.asprintf
      {|<textarea data-planet-qexp="global-context">%s</textarea>|}
      Shapes.Context.(
        context_to_qexp ctx.global_data |> Paperwork.Qexp.to_string) in
  File.create partial str
  |> Validation.from_result
  >|= (fun () -> (true, partial))
  |> trace_creation
;;

let sectors () =
  let open Validation.Infix in
  let () = create_partials () in
  let partial = Filename.concat seed_partials "planet_sectors.meta.html" in
  let () = trace_deletion (soft_deletion_file partial) in
  let str = Glue.Sector.to_html () in
  File.create partial str
  |> Validation.from_result
  >|= (fun () -> (true, partial))
  |> trace_creation
;;

let story_chose_target story filename =
  let folder =
    match story.Shapes.Story.kind with
    | Shapes.Story.Long ->
      long_folder
    | Shapes.Story.Short ->
      short_folder in
  Filename.concat folder filename
;;

let stories () =
  let open Validation.Infix in
  let () = create_partials () in
  let () = create_stories_folder () in
  Glue.Story.collect ()
  >|= List.map Glue.Story.to_hakyll
  >>= Validation.Applicative.sequence
  >>= (fun elts ->
        List.map
          (fun (story, extension, content, partial_name, partial_content) ->
            let open Shapes.Story in
            let filename = story.permaname ^ "." ^ extension in
            let target = story_chose_target story filename in
            let partial = Filename.concat seed_partials partial_name in
            let () = trace_deletion (soft_deletion_file target) in
            let () = trace_deletion (soft_deletion_file partial) in
            File.create target content
            |> Result.bind (fun () -> File.create partial partial_content)
            |> Result.map (fun () -> (true, target ^ " & " ^ partial))
            |> Validation.from_result
            |> trace_creation
            |> Validation.pure)
          elts
        |> Validation.Applicative.sequence)
  |> (function Error e -> Prompter.prompt_errors e | Ok _ -> ())
;;

let twtxt () =
  let database = Glue.Database.twtxt in
  let path = Glue.Database.path database in
  let () = generate () in
  let () = trace_creation (soft_creation twtxt_folder) in
  let open Validation.Infix in
  Dir.children path
  |> Validation.from_result
  >>= (fun files ->
        List.map
          (fun file ->
            let f = Filename.concat path file in
            let rf = Filename.concat twtxt_folder file in
            File.to_stream (fun _ -> Paperwork.Qexp.from_stream) f
            |> Result.bind Paperwork.Qexp.extract_root
            |> Validation.from_result
            >>= (fun nodes ->
                  List.map (fun t -> Shapes.Twtxt.from_qexp t) nodes
                  |> Validation.Applicative.sequence)
            >|= fun datatxt ->
            let sorted =
              List.sort (fun a b -> Shapes.Twtxt.cmp a b * -1) datatxt in
            (rf, List.map Shapes.Twtxt.to_string sorted))
          files
        |> Validation.Applicative.sequence)
  >>= (fun l ->
        List.fold_left
          (fun acc (file, data) ->
            acc
            >>= fun () ->
            let () =
              Ansi.
                [ fg green; text $ Format.sprintf "twtxt processed: %s" file ]
              |> Ansi.to_string
              |> print_endline in
            let content = String.concat "\n" data in
            File.touch file
            |> Result.bind (fun () -> File.overwrite file content)
            |> Validation.from_result)
          (Ok ()) l)
  |> (function Error e -> Prompter.prompt_errors e | Ok _ -> ())
;;

let base_project () = projects ()
let location () = ()

let all () =
  let _ =
    let open Validation.Syntax in
    let* rctx = Glue.Log.context () in
    let* () = Ok (generation_id ()) in
    let* () = Ok (copyright_date ()) in
    let* () = Ok (api ()) in
    let* () = Ok (projects ~rctx ()) in
    let* () = Ok (context rctx) in
    let* () = Ok (sectors ()) in
    let* () = Ok (stories ()) in
    let* () = Ok (location ()) in
    let* () = Ok (twtxt ()) in
    Ok () in
  ()
;;
