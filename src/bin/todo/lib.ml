open Bedrock
open Baremetal
module Binutil = Glue.Binutil

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

      let engagement = None in

      let task =
        Glue.Task.init some_project sectors name description checklist tags
          engagement in
      match task with
      | Error e ->
        Prompter.prompt_error e
      | Ok x ->
        let str = Shapes.Task.to_qexp x in
        Format.printf "%a\n" Paperwork.Qexp.pp str)
;;
