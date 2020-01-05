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

let ansi_project task = ansi_list "Project" (Option.to_list Shapes.Task.(task.project))
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
               then [ fg blue; !"["; fg green; bold; !"X"; reset; fg blue; !"]" ]
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
             [ [ bold; !(label ^ ": "); reset; !(Paperwork.Timetable.Day.to_string date) ]
             ]
           | _ -> [])
  in
  [ !"\n" ] @ box "Dates" dates
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

let check taskname =
  ensure_task taskname (fun task ->
      let () = Ansi.(ansi_header task |> to_string |> print_endline) in
      ())
;;

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
      Glue.Task.init some_project sectors name description checklist tags engagement
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
              Prompter.yes_no ~answer_style:Ansi.[ fg yellow ] ~title:"Confirm?" task_str
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
