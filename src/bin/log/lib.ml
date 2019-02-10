open Bedrock
open Util
open Paperwork
open Baremetal

let sectors () =
  match Glue.Sector.all () with
  | Ok hashtable ->
    let () =
      Ansi.[bold; fg cyan; !"Available sectors\n"]
      |> Ansi.to_string |> print_endline
    in
    Hashtbl.iter
      (fun _ sector ->
        let open Shapes.Sector in
        let open Ansi in
        text_box sector.name sector.desc
        @ [reset; fg cyan; !(Color.to_hex sector.color); reset; !"\n"]
        |> to_string |> print_endline )
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
    Prompter.prompt_error Error.(Invalid_int 0);
    false
  | Some _ ->
    true
;;

let rec when_ () =
  try_until repeat_result (fun () ->
      Prompter.resultable
        ~title:"When"
        ~answer_style:Ansi.[fg yellow]
        (fun x ->
          if String.(length $ trim x) = 0
          then Glue.Util.day ()
          else Timetable.Day.from_string x )
        "Use empty string for [current timestamp]" )
  |> function
  | Ok x ->
    let valid =
      Prompter.yes_no
        ~answer_style:Ansi.[fg yellow]
        ~title:"Confirm ?"
        (Format.asprintf "Choice %a" Timetable.Day.pp x)
    in
    if valid then x else when_ ()
  | _ ->
    when_ ()
;;

let rec during () =
  try_until repeat_option (fun () ->
      Prompter.int_opt
        ~answer_style:Ansi.[fg yellow]
        ~title:"During"
        ~f:(function
          | None -> None | Some x when x <= 0 -> None | x -> x)
        "How much time" )
  |> function
  | Some x ->
    let valid =
      Prompter.yes_no
        ~answer_style:Ansi.[fg yellow]
        ~title:"Confirm ?"
        (Format.asprintf "Choice %d" x)
    in
    if valid then x else during ()
  | _ ->
    during ()
;;

let rec sector sectors =
  try_until repeat_result (fun () ->
      Prompter.choose
        ~answer_style:Ansi.[fg yellow]
        ~title:"In which sector"
        Util.id
        Util.id
        (Array.of_seq $ Hashtbl.to_seq_keys sectors)
        "Related sector" )
  |> function
  | Ok x ->
    let valid =
      Prompter.yes_no
        ~answer_style:Ansi.[fg yellow]
        ~title:"Confirm ?"
        (Format.asprintf "Choice %s" x)
    in
    if valid then x else sector sectors
  | _ ->
    sector sectors
;;

let project projects =
  try_until repeat_result (fun () ->
      Prompter.choose
        ~answer_style:Ansi.[fg yellow]
        ~title:"In which project"
        (fun x -> Shapes.Project.(x.name))
        (fun x ->
          Shapes.Project.(
            Format.sprintf "%s - %s" x.title x.synopsis) )
        (Array.of_list projects)
        "Related project" )
;;

let interactive () =
  match Glue.Sector.all (), Glue.Project.all () with
  | Error x, Error y ->
    Prompter.prompt_errors (x @ y)
  | Error x, _ | _, Error x ->
    Prompter.prompt_errors x
  | Ok sectors, Ok projects ->
    let _uuid = Uuid.make () in
    let _timecode = when_ () in
    let _duration = during () in
    let _sector = sector sectors in
    let _project = project projects in
    ()
;;
