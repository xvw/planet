open Bedrock
open Error
open Util
open Paperwork
open Baremetal

let sectors () =
  match Glue.Sector.all () with
  | Ok hashtable ->
    let () =
      Ansi.[ bold; fg cyan; !"Available sectors\n" ]
      |> Ansi.to_string |> print_endline
    in
    Hashtbl.iter
      (fun _ sector ->
        let open Shapes.Sector in
        let open Ansi in
        text_box sector.name sector.desc
        @ [ reset
          ; fg cyan
          ; !(Color.to_hex sector.color)
          ; reset
          ; !"\n"
          ]
        |> to_string |> print_endline)
      hashtable
  | Error errs ->
    Prompter.prompt_errors errs
;;

let repeat_result = function
  | Ok _ ->
    true
  | Error e ->
    Prompter.prompt_error e;
    false
;;

let repeat_validation = function
  | Ok _ ->
    true
  | Error e ->
    Prompter.prompt_errors e;
    false
;;

let repeat_option = function
  | None ->
    Prompter.prompt_error Error.(Invalid_field "input");
    false
  | Some _ ->
    true
;;

let rec when_ () =
  try_until repeat_result (fun () ->
      Prompter.resultable
        ~title:"When"
        ~answer_style:Ansi.[ fg yellow ]
        (fun x ->
          if String.(length $ trim x) = 0
          then Glue.Util.day ()
          else Timetable.Day.from_string x)
        "Use empty string for [current timestamp]")
  |> function
  | Ok x ->
    let valid =
      Prompter.yes_no
        ~answer_style:Ansi.[ fg yellow ]
        ~title:"Confirm?"
        (Format.asprintf "Choice %a" Timetable.Day.pp x)
    in
    if valid then x else when_ ()
  | _ ->
    when_ ()
;;

let rec during () =
  try_until repeat_option (fun () ->
      Prompter.int_opt
        ~answer_style:Ansi.[ fg yellow ]
        ~title:"During"
        ~f:(function
          | None -> None | Some x when x <= 0 -> None | x -> x)
        "How much time (in minut)")
  |> function
  | Some x ->
    let valid =
      Prompter.yes_no
        ~answer_style:Ansi.[ fg yellow ]
        ~title:"Confirm?"
        (Format.asprintf "Choice %d" x)
    in
    if valid then x else during ()
  | _ ->
    during ()
;;

let rec sector sectors =
  try_until repeat_result (fun () ->
      Prompter.choose
        ~answer_style:Ansi.[ fg yellow ]
        ~title:"In which sector"
        Util.id
        Util.id
        (Array.of_seq $ Hashtbl.to_seq_keys sectors)
        "Related sector")
  |> function Ok x -> x | _ -> sector sectors
;;

let rec project projects =
  let all_projects = None :: List.map (fun x -> Some x) projects in
  try_until repeat_result (fun () ->
      Prompter.choose
        ~answer_style:Ansi.[ fg yellow ]
        ~title:"In which project?"
        (Option.map (fun x -> Shapes.Project.(x.name)))
        (function
          | Some x ->
            Shapes.Project.(
              Format.sprintf "%s - %s" x.title x.synopsis)
          | None ->
            "Not connected")
        (Array.of_list all_projects)
        "Related project")
  |> function Ok x -> x | _ -> project projects
;;

let rec label () =
  try_until repeat_option (fun () ->
      Prompter.string_opt
        ~answer_style:Ansi.[ fg yellow ]
        ~title:"Label?"
        "Describe the task")
  |> function
  | Some x ->
    let valid =
      Prompter.yes_no
        ~answer_style:Ansi.[ fg yellow ]
        ~title:"Confirm?"
        (Format.asprintf "Choice `%s`" x)
    in
    if valid then x else label ()
  | _ ->
    label ()
;;

let qexpify log = Qexp.node [ Shapes.Log.to_qexp log ]

let push_result log =
  let open Result.Infix in
  Glue.Log.create_file Shapes.Log.(log.day)
  >>= (fun (filename, created) ->
        let () =
          if not created
          then
            Ansi.(
              [ fg yellow
              ; text filename
              ; fg green
              ; text " has been created"
              ]
              |> to_string)
            |> print_endline
        in
        Ok filename)
  >>= fun filename ->
  let str_log = log |> qexpify |> Qexp.to_string |> String.trim in
  File.append filename ("\n" ^ str_log ^ "\n")
  >>= (fun () -> Glue.Git.stage Glue.Log.log_pattern)
  >>= (fun () ->
        Glue.Git.commit ~desc:str_log
        $ Format.asprintf "Record task: %a" Timetable.Day.pp log.day
        )
  >> Ok (filename, str_log)
;;

let ensure_sectors_projects f =
  match Glue.Sector.all (), Glue.Project.all () with
  | Error x, Error y ->
    Prompter.prompt_errors (x @ y)
  | Error x, _ | _, Error x ->
    Prompter.prompt_errors x
  | Ok sectors, Ok projects ->
    f sectors projects
;;

let push_feedback = function
  | Ok (filename, str_log) ->
    Ansi.(
      [ fg yellow
      ; text "\n"
      ; text str_log
      ; text "\n"
      ; fg green
      ; text "has been dumped in: "
      ; text filename
      ]
      |> to_string)
    |> print_endline
  | Error x ->
    Prompter.prompt_error x
;;

let visual_push log = log |> push_result |> push_feedback

let interactive () =
  ensure_sectors_projects (fun sectors projects ->
      let uuid = Uuid.make () in
      let a_timecode = when_ () in
      let a_duration = during () in
      let a_sector = sector sectors in
      let some_project = project projects in
      let a_label = label () in
      let () = Ansi.[ reset ] |> Ansi.to_string |> print_endline in
      let log =
        Shapes.Log.new_log
          (Uuid.to_string uuid)
          a_timecode
          a_duration
          a_sector
          some_project
          a_label
      in
      visual_push log)
;;

let check_day = function
  | None ->
    Glue.Util.day () |> Validation.from_result
  | Some x ->
    Timetable.Day.from_string x |> Validation.from_result
;;

let check_duration =
  Validation.from_option (Invalid_field "duration")
;;

let check_sector sectors = function
  | None ->
    Error [ Invalid_field "sector" ]
  | Some x ->
    let open Validation.Infix in
    Validation.from_option
      (Unknown ("sector: " ^ x))
      (Hashtbl.find_opt sectors x)
    >|= fun x -> x.Shapes.Sector.name
;;

let check_project projects = function
  | None ->
    Ok None
  | Some x ->
    let open Validation.Infix in
    let flag =
      List.find_opt (fun p -> p.Shapes.Project.name = x) projects
    in
    Validation.from_option (Unknown ("project: " ^ x)) flag
    >|= fun x -> Some x.Shapes.Project.name
;;

let check_label x =
  if String.length (String.trim x) = 0
  then Error [ Invalid_field "label" ]
  else Ok x
;;

let record sector duration timecode project label =
  ensure_sectors_projects (fun sectors projects ->
      let open Validation.Infix in
      let potential_log =
        Shapes.Log.new_log (Uuid.make () |> Uuid.to_string)
        <$> check_day timecode <*> check_duration duration
        <*> check_sector sectors sector
        <*> check_project projects project
        <*> check_label (String.concat " " label)
      in
      match potential_log with
      | Error xs ->
        Prompter.prompt_errors xs
      | Ok log ->
        visual_push log)
;;

let push_whereami place =
  let open Result.Infix in
  Glue.Log.create_whereami_file ()
  >|= (fun (filename, created) ->
        let () =
          if not created
          then
            Ansi.(
              [ fg yellow
              ; text filename
              ; fg green
              ; text " has been created"
              ]
              |> to_string)
            |> print_endline
        in
        filename)
  >>= (fun filename ->
        File.append filename ("\n" ^ place) >> Ok (filename, place))
  >>= (fun result ->
        Glue.Git.stage [ Glue.Log.whereami_file ]
        >>= (fun () -> Glue.Git.commit $ "update location: " ^ place)
        >|= const result)
  |> push_feedback
;;

let whereami moment opt_country opt_city =
  match opt_country, opt_city with
  | Some country, Some city ->
    let co = String.(lowercase_ascii %> trim $ country) in
    let ci = String.(lowercase_ascii %> trim $ city) in
    let mo =
      Option.map Timetable.Day.from_string moment
      |> Option.get_or Glue.Util.day
    in
    (match mo with
    | Error err ->
      Prompter.prompt_error err
    | Ok timecode ->
      let qexp_str =
        Qexp.(
          node
            [ node
                [ keyword (Timetable.Day.to_string timecode)
                ; string co
                ; string ci
                ]
            ])
        |> Qexp.to_string
      in
      push_whereami qexp_str)
  | _ ->
    Prompter.prompt_error
      (Invalid_field "country or city can not be empty")
;;
