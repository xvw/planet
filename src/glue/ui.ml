open Bedrock
open Util
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

let link_box
    ?(prefix = Ansi.[ !"│+ " ])
    ?(box_style = Ansi.[ fg cyan ])
    ?(title_style = Ansi.[ bold ])
    ?(f =
      fun (name, url) ->
        Ansi.
          [ fg green
          ; !name
          ; reset
          ; !"  <"
          ; fg yellow
          ; underline
          ; !url
          ; reset
          ; !">"
          ])
    title
    list =
  let maxlen =
    List.fold_left (fun acc (x, _) -> max acc $ String.length x) 0 list in
  let nl =
    List.map
      (fun (a, b) -> (a ^ (String.make $ maxlen - String.length a $ ' '), b))
      list in
  Ansi.(generic_box ~prefix ~box_style ~title_style) f title nl
;;

let dated_link_box
    ?(prefix = Ansi.[ !"│+ " ])
    ?(box_style = Ansi.[ fg cyan ])
    ?(title_style = Ansi.[ bold ])
    ?(f =
      fun (name, date, url) ->
        Ansi.
          [ fg green
          ; !name
          ; reset
          ; !"   "
          ; fg black
          ; bg cyan
          ; !" "
          ; !(Paperwork.Timetable.Day.to_string date)
          ; !" "
          ; reset
          ; !"  <"
          ; fg yellow
          ; underline
          ; !url
          ; reset
          ; !">"
          ])
    title
    list =
  let maxlen =
    List.fold_left (fun acc (x, _, _) -> max acc $ String.length x) 0 list in
  let nl =
    List.map
      (fun (a, b, c) ->
        (a ^ (String.make $ maxlen - String.length a $ ' '), b, c))
      list in
  Ansi.(generic_box ~prefix ~box_style ~title_style) f title nl
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
