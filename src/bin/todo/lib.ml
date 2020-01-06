open Bedrock
open Baremetal

let ensure_task taskname f =
  let filename =
    Filename.concat
      Glue.(Database.path Task.database)
      (String.uppercase_ascii taskname ^ ".qube")
  in
  match
    filename
    |> File.to_stream (fun _ -> Paperwork.Qexp.from_stream)
    |> Validation.from_result
    |> Validation.bind Shapes.Task.from_qexp
  with
  | Error errs -> Prompter.prompt_errors errs
  | Ok task -> f task
;;

let ansi_header task =
  let open Shapes.Task in
  Ansi.[ !"\n" ]
  @ Ansi.(box task.name [ [ fg cyan; !(task.description) ] ])
  @ Ansi.[ fg magenta; !(task.uuid ^ " ~> " ^ state_to_string task.state) ]
  @ Ansi.[ reset; !"\n" ]
;;

let ansi_list title elements =
  match elements with
  | [] -> []
  | _ ->
    Ansi.
      [ bold
      ; !(title ^ ": ")
      ; reset
      ; fg yellow
      ; !(String.concat ", " elements)
      ; reset
      ; !"\n"
      ]
;;

let ansi_project task =
  ansi_list "Project" (Option.to_list Shapes.Task.(task.project))
;;

let ansi_sectors task = ansi_list "Sectors" Shapes.Task.(task.sectors)
let ansi_tags task = ansi_list "Tags" Shapes.Task.(task.tags)

let ansi_checklist task =
  let open Shapes.Task in
  match task.checklist with
  | [] -> []
  | _ ->
    Ansi.(
      box
        "Checklist"
        (List.mapi
           (fun i (flag, label) ->
             [ bold; fg magenta; !(Format.asprintf "%03d:" (succ i)); reset ]
             @ (if flag
               then
                 [ fg blue; !"["; fg green; bold; !"X"; reset; fg blue; !"]" ]
               else [ fg blue; !"[ ]" ])
             @ [ reset; !" "; !label; reset ])
           task.checklist))
;;

let ansi_dates task =
  let open Shapes.Task in
  let open Ansi in
  let dates =
    [ "Creation date", Some task.date
    ; "Engagement date", task.engagement_date
    ; "Opening date", task.opening_date
    ; "Closing date", task.closing_date
    ]
    |> List.bind (function
           | label, Some date ->
             [ [ bold
               ; !(label ^ ": ")
               ; reset
               ; !(Paperwork.Timetable.Day.to_string date)
               ]
             ]
           | _ -> [])
  in
  [ !"\n" ] @ box "Dates" dates
;;

let display_patch new_state new_opening_date new_closing_date =
  let open Ansi in
  ((match new_state with
   | None -> []
   | Some x ->
     [ bold
     ; fg blue
     ; !"New state: "
     ; reset
     ; fg yellow
     ; !(Shapes.Task.state_to_string x)
     ; !"\n"
     ])
  @ (match new_opening_date with
    | None -> []
    | Some x ->
      [ bold
      ; fg blue
      ; !"New Opening Date: "
      ; reset
      ; fg yellow
      ; !(Paperwork.Timetable.Day.to_string x)
      ; !"\n"
      ])
  @
  match new_closing_date with
  | None -> []
  | Some x ->
    [ bold
    ; fg blue
    ; !"New Closing Date: "
    ; reset
    ; fg yellow
    ; !(Paperwork.Timetable.Day.to_string x)
    ; !"\n"
    ])
  |> Ansi.to_string
;;

let display task =
  let fragment =
    ansi_header task
    @ ansi_project task
    @ ansi_sectors task
    @ ansi_tags task
    @ ansi_checklist task
    @ ansi_dates task
    @ Ansi.[ !"\n" ]
  in
  fragment |> Ansi.to_string ~scoped:true |> print_endline
;;

let show taskname = ensure_task taskname display

let move taskname new_state =
  match Shapes.Task.state_from_string new_state with
  | Error errs -> Prompter.prompt_errors errs
  | Ok state ->
    ensure_task taskname (fun task ->
        let open Shapes.Task in
        let filename =
          Filename.concat
            Glue.(Database.path Task.database)
            (Shapes.Task.(task.uuid) ^ ".qube")
        in
        let new_task = { task with state } in
        let qexp = to_qexp new_task in
        let str = Paperwork.Qexp.to_string qexp in
        match File.overwrite filename str with
        | Ok () -> display new_task
        | Error err -> Prompter.prompt_error err)
;;

let may_update_state task =
  let open Shapes.Task in
  let open Result.Syntax in
  let* day = Glue.Util.day () in
  let (new_state, new_opening_date, new_closing_date), need_changement =
    need_state_changement day task
  in
  if need_changement
  then (
    let valid =
      Prompter.yes_no
        ~answer_style:Ansi.[ fg yellow ]
        ~title:"Apply patch"
        (display_patch new_state new_opening_date new_closing_date)
    in
    if valid
    then (
      match new_state with
      | None -> Ok task
      | Some nstate ->
        Ok
          { task with
            state = nstate
          ; opening_date = new_opening_date
          ; closing_date = new_closing_date
          })
    else Ok task)
  else Ok task
;;

let check taskname =
  ensure_task taskname (fun task ->
      let () = Ansi.(ansi_header task |> to_string |> print_endline) in
      let open Shapes.Task in
      let open Result.Infix in
      Util.try_until Prompter.repeat_result (fun () ->
          Prompter.choose_multiple
            ~answer_style:Ansi.[ fg yellow ]
            ~title:"Which task"
            (fun (i, _, _) -> i)
            (fun (_, flag, label) ->
              let f = if flag then "x" else " " in
              Format.asprintf "[%s] %s" f label)
            (Array.of_list (List.mapi (fun i (f, g) -> i, f, g) task.checklist))
            "Toggle task")
      >|= (fun indexes ->
            let new_check =
              List.mapi
                (fun i (f, l) -> if List.mem i indexes then not f, l else f, l)
                task.checklist
            in
            { task with checklist = new_check })
      >>= may_update_state
      >|= (fun task ->
            let () = ansi_checklist task |> Ansi.to_string |> print_endline in
            task)
      >>= (fun task ->
            let qexp = Shapes.Task.to_qexp task in
            let filename =
              Filename.concat
                Glue.(Database.path Task.database)
                (Shapes.Task.(task.uuid) ^ ".qube")
            in
            let task_str = Paperwork.Qexp.to_string qexp in
            File.overwrite filename task_str >|= fun () -> task)
      |> function
      | Error err -> Prompter.prompt_error err
      | Ok new_task -> display new_task)
;;

let update_engagement date task =
  let open Shapes.Task in
  let valid =
    Prompter.yes_no
      ~answer_style:Ansi.[ fg yellow ]
      ~title:"Update engagement"
      (Format.asprintf
         "old: %a \t new: %a"
         (Option.pp Paperwork.Timetable.Day.pp)
         task.engagement_date
         (Option.pp Paperwork.Timetable.Day.pp)
         date)
  in
  if valid
  then (
    let filename =
      Filename.concat
        Glue.(Database.path Task.database)
        (Shapes.Task.(task.uuid) ^ ".qube")
    in
    let new_task = { task with engagement_date = date } in
    let qexp = to_qexp new_task in
    let str = Paperwork.Qexp.to_string qexp in
    match File.overwrite filename str with
    | Ok () -> display new_task
    | Error err -> Prompter.prompt_error err)
  else ()
;;

let engage taskname date_str =
  match Paperwork.Timetable.Day.from_string date_str with
  | Error err -> Prompter.prompt_error err
  | Ok date -> ensure_task taskname (update_engagement (Some date))
;;

let desengage taskname = ensure_task taskname (update_engagement None)

let create () =
  Glue.Ui.ensure_sectors_projects (fun sectors (_ctx, projects) ->
      let name = Glue.Ui.get_string "Title?" "Title of the task" in
      let description = Glue.Ui.get_string "Description?" "Describe the task" in
      let some_project =
        Glue.Ui.may_project (List.map (fun (x, _, _) -> x) projects)
        |> Option.map (fun x -> Shapes.Project.(x.name))
      in
      let sectors = Glue.Ui.select_sectors sectors in
      let checklist =
        Glue.Ui.get_string_opt "Tasks?" "Checklist"
        |> Option.to_list
        |> List.bind (String.tokenize ',')
      in
      let tags =
        Glue.Ui.get_string_opt "Tags?" "Tags of the task"
        |> Option.to_list
        |> List.bind (String.tokenize ',')
      in
      let engagement = Glue.Ui.get_day_opt "Engagement?" "Potential due date" in
      let open Result.Infix in
      Glue.Task.init
        some_project
        sectors
        name
        description
        checklist
        tags
        engagement
      >|= (fun task ->
            let () = display task in
            task)
      >>= (fun task ->
            let qexp = Shapes.Task.to_qexp task in
            let filename =
              Filename.concat
                Glue.(Database.path Task.database)
                (Shapes.Task.(task.uuid) ^ ".qube")
            in
            let task_str = Paperwork.Qexp.to_string qexp in
            let valid =
              Prompter.yes_no
                ~answer_style:Ansi.[ fg yellow ]
                ~title:"Confirm?"
                task_str
            in
            if valid
            then
              File.create filename task_str
              >|= fun () ->
              Ansi.
                [ bold
                ; fg green
                ; !filename
                ; reset
                ; !" has been dumped\n"
                ; fg yellow
                ; !task_str
                ; reset
                ; !"\n"
                ]
              |> Ansi.to_string ~scoped:true
              |> print_endline
            else Ok ())
      |> function
      | Ok _ -> ()
      | Error e -> Prompter.prompt_error e)
;;
