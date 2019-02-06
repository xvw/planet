open Baremetal
open Bedrock.Util

let ls_render_valid_project projects =
  let () =
    Ansi.[bold; text "Valid projects:\n"]
    |> Ansi.to_string ~scoped:true
    |> print_endline
  in
  List.iter
    (fun (_, x) ->
      let name, status =
        Shapes.Project.(x.name, status_to_string x.status)
      in
      Ansi.
        [ bold
        ; foreground green
        ; text " - "
        ; !name
        ; reset
        ; !" ("
        ; !status
        ; !")" ]
      |> Ansi.to_string ~scoped:true
      |> print_endline )
    projects
;;

let ls_render_invalid_project projects =
  match projects with
  | [] ->
    ()
  | _ ->
    let () =
      Ansi.[bold; text "Invalid projects:\n"]
      |> Ansi.to_string ~scoped:true
      |> print_endline
    in
    let () =
      List.iter
        (fun (f, x) ->
          let () =
            Ansi.[bold; foreground red; text " - "; text f]
            |> Ansi.to_string ~scoped:true
            |> print_endline
          in
          Glue.Ui.prompt_errors ~intro:false x )
        projects
    in
    let () = print_newline () in
    ()
;;

let ls () =
  match Glue.Project.inspect () with
  | Error err ->
    Glue.Ui.prompt_error err
  | Ok projects ->
    let invalid_project, valid_projects =
      List.fold_right
        (fun (elt, f) (l, r) ->
          match elt with
          | Error errs ->
            (f, errs) :: l, r
          | Ok project ->
            l, (f, project) :: r )
        projects
        ([], [])
    in
    let () = print_newline () in
    let () = ls_render_valid_project valid_projects in
    let () = print_newline () in
    let () = ls_render_invalid_project invalid_project in
    ()
;;

let license = function
  | None ->
    []
  | Some x ->
    Ansi.
      [reset; fg cyan; !"-"; bg red; fg yellow; !" "; !x; !" "; reset]
;;

let render_link_box title list =
  match list with
  | [] ->
    []
  | li ->
    Ansi.[reset; !"\n\n"] @ Glue.Ui.link_box title li
;;

let render_dated_link_box title list =
  match list with
  | [] ->
    []
  | li ->
    Ansi.[reset; !"\n\n"] @ Glue.Ui.dated_link_box title li
;;

let show_project project =
  let open Shapes.Project in
  let fragment =
    Ansi.[!"\n"]
    @ Ansi.(box project.title [[fg cyan; !(project.synopsis)]])
    @ Ansi.
        [ bg red
        ; fg yellow
        ; !(Format.sprintf " %s " $ status_to_string project.status)
        ]
    @ license project.license
    @ render_link_box "Tools" project.tools
    @ render_link_box "Links" project.links
    @ (render_dated_link_box "Releases" $ List.rev project.releases)
    @ Ansi.[!"\n"]
  in
  fragment |> Ansi.to_string ~scoped:true |> print_endline
;;

let show project_name =
  match Glue.Project.read (project_name ^ ".qube") with
  | Error err, _ ->
    Glue.Ui.prompt_errors err
  | Ok project, _ ->
    show_project project
;;
