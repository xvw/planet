open Baremetal
open Bedrock
open Bedrock.Util

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
  try_until Prompter.repeat_result (fun () ->
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

let rec select_sectors sectors =
  let all_sectors =
    None
    :: (sectors
       |> Hashtbl.to_seq_keys
       |> Seq.map (fun x -> Some x)
       |> List.of_seq
       ) in
  try_until Prompter.repeat_result (fun () ->
      Prompter.choose_multiple
        ~answer_style:Ansi.[ fg yellow ]
        ~title:"In which sector" Option.to_list
        (function Some x -> x | None -> "Not connected")
        (Array.of_list all_sectors)
        "Related sector")
  |> Result.map List.flatten
  |> (function Ok x -> x | _ -> select_sectors sectors)
;;

let rec get_string a b =
  Util.try_until Prompter.repeat_option (fun () ->
      Prompter.string_opt ~answer_style:Ansi.[ fg yellow ] ~title:a b)
  |> (function Some x -> x | _ -> get_string a b)
;;

let get_string_opt a b =
  Prompter.string_opt ~answer_style:Ansi.[ fg yellow ] ~title:a b
;;

let rec get_day_opt a b =
  try_until Prompter.repeat_result (fun () ->
      Prompter.string_opt ~answer_style:Ansi.[ fg yellow ] ~title:a b
      |> function
        | None ->
          Ok None
        | Some x ->
          Paperwork.Timetable.Day.from_string x |> Result.map (fun x -> Some x))
  |> (function Ok x -> x | _ -> get_day_opt a b)
;;
