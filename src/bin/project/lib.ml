open Baremetal

let ls_render_valid_project projects =
  let () =
    Ansi.[bold; text "Valid projects:\n"]
    |> Ansi.to_string ~scoped:true
    |> print_endline
  in
  List.iter
    (fun (_, x) ->
      let name = Shapes.Project.(x.name) in
      let status = Shapes.Project.(x.status |> status_to_string) in
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
