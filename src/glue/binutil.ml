open Baremetal

let ensure_sectors_projects f =
  match (Sector.all (), Project.all ()) with
  | (Error x, Error y) ->
    Prompter.prompt_errors (x @ y)
  | (Error x, _) | (_, Error x) ->
    Prompter.prompt_errors x
  | (Ok sectors, Ok ctx) ->
    f sectors ctx
;;

let rec may_project projects =
  let all_projects = None :: List.map (fun x -> Some x) projects in
  Bedrock.Util.try_until Prompter.repeat_result (fun () ->
      Prompter.choose
        ~answer_style:Ansi.[ fg yellow ]
        ~title:"In which project?"
        (fun x -> x)
        (function
            | Some x ->
              Shapes.Project.(Format.sprintf "%s - %s" x.title x.synopsis)
            | None ->
              "Not connected")
        (Array.of_list all_projects)
        "Related project")
  |> (function Ok x -> x | _ -> may_project projects)
;;
