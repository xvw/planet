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
    Glue.Ui.prompt_errors errs
;;

let interactive () = Prompter.string_opt "When?" |> ignore
