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

let rec when_ () =
  try_until repeat_result (fun () ->
      Prompter.resultable
        ~title:"When"
        (fun x ->
          if String.(length $ trim x) = 0
          then Glue.Util.day ()
          else Timetable.Day.from_string x )
        "Use empty string for [current timestamp]" )
  |> function
  | Ok x as final ->
    let x =
      Prompter.yes_no
        ~title:"Confirm ?"
        (Format.asprintf "Choice %a" Timetable.Day.pp x)
    in
    if x then final else when_ ()
  | _ ->
    when_ ()
;;

let interactive () =
  let _ = when_ () in
  ()
;;
