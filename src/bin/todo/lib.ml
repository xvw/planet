open Bedrock
open Baremetal

let ansi_header task =
  let open Shapes.Task in
  Ansi.[ !"\n" ]
  @ Ansi.(box task.name [ [ fg cyan; !(task.description) ] ])
  @ Ansi.[ fg magenta; !(task.uuid ^ " ~> " ^ state_to_string task.state) ]
  @ Ansi.[ reset; !"\n" ]
  @
  match task.project with
  | None ->
    []
  | Some x ->
    Ansi.[ bold; !"Project: "; reset; fg yellow; !x; reset; !"\n" ]
;;

let ansi_list title elements =
  match elements with
  | [] ->
    []
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

let ansi_sectors task = ansi_list "Sectors" Shapes.Task.(task.sectors)
let ansi_tags task = ansi_list "Tags" Shapes.Task.(task.tags)

let ansi_checklist task =
  let open Shapes.Task in
  match task.checklist with
  | [] ->
    []
  | _ ->
    Ansi.(
      box "Checklist"
        (List.mapi
           (fun i (flag, label) ->
             [ bold; fg magenta; !(Format.asprintf "%03d:" (succ i)); reset ]
             @ ( if flag then
                   [ fg blue; !"["; fg green; bold; !"X"; reset; fg blue; !"]" ]
               else
                 [ fg blue; !"[ ]" ]
               )
             @ [ reset; !" "; !label; reset ])
           task.checklist))
;;

let ansi_dates task =
  let open Shapes.Task in
  let open Ansi in
  let dates =
    [ ("Creation date", Some task.date)
    ; ("Engagement date", task.engagement_date)
    ; ("Opening date", task.opening_date)
    ; ("Closing date", task.closing_date)
    ]
    |> List.bind (function
         | (label, Some date) ->
           [ [ bold
             ; !(label ^ ": ")
             ; reset
             ; !(Paperwork.Timetable.Day.to_string date)
             ]
           ]
         | _ ->
           []) in
  [ !"\n" ] @ box "Dates" dates
;;

let display task =
  let fragment =
    ansi_header task
    @ ansi_sectors task
    @ ansi_tags task
    @ ansi_checklist task
    @ ansi_dates task
    @ Ansi.[ !"\n" ] in
  fragment |> Ansi.to_string ~scoped:true |> print_endline
;;

let create () =
  Glue.Ui.ensure_sectors_projects (fun sectors (_ctx, projects) ->
      let name = Glue.Ui.get_string "Title?" "Title of the task" in

      let description = Glue.Ui.get_string "Description?" "Describe the task" in

      let some_project =
        Glue.Ui.may_project (List.map (fun (x, _, _) -> x) projects)
        |> Option.map (fun x -> Shapes.Project.(x.name)) in

      let sectors = Glue.Ui.select_sectors sectors in

      let checklist =
        Glue.Ui.get_string_opt "Tasks?" "Checklist"
        |> Option.to_list
        |> List.bind (String.tokenize ',') in
      let tags =
        Glue.Ui.get_string_opt "Tags?" "Tags of the task"
        |> Option.to_list
        |> List.bind (String.tokenize ',') in

      let engagement = Glue.Ui.get_day_opt "Engagement?" "Potential due date" in

      let task =
        Glue.Task.init some_project sectors name description checklist tags
          engagement in
      (match task with Error e -> Prompter.prompt_error e | Ok x -> display x))
;;
