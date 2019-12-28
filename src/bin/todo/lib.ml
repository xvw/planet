open Bedrock
open Baremetal
module Binutil = Glue.Binutil

let display task =
  let open Shapes.Task in
  let fragment =
    Ansi.[ !"\n" ]
    @ Ansi.(box task.name [ [ fg cyan; !(task.description) ] ])
    @ Ansi.[ fg magenta; !(task.uuid ^ " ~> " ^ state_to_string task.state) ]
    @ Ansi.[ reset; !"\n" ]
    @ (match task.project with
      | None ->
        []
      | Some x ->
        Ansi.[ bold; !"Project: "; reset; fg yellow; !x; reset; !"\n" ])
    @ (match task.sectors with
      | [] ->
        []
      | _ ->
        Ansi.
          [ bold
          ; !"Sectors: "
          ; reset
          ; fg yellow
          ; !(String.concat ", " task.sectors)
          ; reset
          ; !"\n"
          ])
    @ (match task.tags with
      | [] ->
        []
      | _ ->
        Ansi.
          [ bold
          ; !"Tags: "
          ; reset
          ; fg yellow
          ; !(String.concat ", " task.tags)
          ; reset
          ; !"\n"
          ])
    @ (match task.checklist with
      | [] ->
        []
      | _ ->
        Ansi.(
          box "Checklist"
            (List.mapi
               (fun i (flag, label) ->
                 [ bold
                 ; fg magenta
                 ; !(Format.asprintf "%03d:" (succ i))
                 ; reset
                 ]
                 @ ( if flag then
                       [ fg blue
                       ; !"["
                       ; fg green
                       ; bold
                       ; !"X"
                       ; reset
                       ; fg blue
                       ; !"]"
                       ]
                   else
                     [ fg blue; !"[ ]" ]
                   )
                 @ [ reset; !" "; !label; reset ])
               task.checklist)))
    @ Ansi.[ !"\n" ] in
  fragment |> Ansi.to_string ~scoped:true |> print_endline
;;

let create () =
  Binutil.ensure_sectors_projects (fun sectors (_ctx, projects) ->
      let name = Binutil.get_string "Title?" "Title of the task" in

      let description = Binutil.get_string "Description?" "Describe the task" in

      let some_project =
        Binutil.may_project (List.map (fun (x, _, _) -> x) projects)
        |> Option.map (fun x -> Shapes.Project.(x.name)) in

      let sectors = Binutil.select_sectors sectors in

      let checklist =
        Binutil.get_string_opt "Tasks?" "Checklist"
        |> Option.to_list
        |> List.bind (String.tokenize ',') in
      let tags =
        Binutil.get_string_opt "Tags?" "Tags of the task"
        |> Option.to_list
        |> List.bind (String.tokenize ',') in

      let engagement = Binutil.get_day_opt "Engagement?" "Potential due date" in

      let task =
        Glue.Task.init some_project sectors name description checklist tags
          engagement in
      (match task with Error e -> Prompter.prompt_error e | Ok x -> display x))
;;
