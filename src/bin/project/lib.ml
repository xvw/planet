open Baremetal
open Bedrock.Util

let ls_render_valid_project projects =
  let () =
    Ansi.[ bold; text "Valid projects:\n" ]
    |> Ansi.to_string ~scoped:true
    |> print_endline
  in
  List.iter
    (fun (_, x) ->
      let name, status, published =
        Shapes.Project.(
          x.name, status_to_string x.status, x.published)
      in
      let color =
        if published then Ansi.green else Ansi.bright_magenta
      in
      Ansi.
        [ bold
        ; foreground color
        ; text " - "
        ; !name
        ; reset
        ; !" ("
        ; !status
        ; !")"
        ]
      |> Ansi.to_string ~scoped:true
      |> print_endline)
    projects
;;

let ls_render_invalid_project projects =
  match projects with
  | [] ->
    ()
  | _ ->
    let () =
      Ansi.[ bold; text "Invalid projects:\n" ]
      |> Ansi.to_string ~scoped:true
      |> print_endline
    in
    let () =
      List.iter
        (fun (f, x) ->
          let () =
            Ansi.[ bold; foreground red; text " - "; text f ]
            |> Ansi.to_string ~scoped:true
            |> print_endline
          in
          Prompter.prompt_errors ~intro:false x)
        projects
    in
    let () = print_newline () in
    ()
;;

let ls () =
  match Glue.Project.inspect () with
  | Error err ->
    Prompter.prompt_errors err
  | Ok projects ->
    let invalid_project, valid_projects =
      List.fold_right
        (fun (elt, f) (l, r) ->
          match elt with
          | Error errs ->
            (f, errs) :: l, r
          | Ok (project, _, _) ->
            l, (f, project) :: r)
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
      [ reset
      ; fg cyan
      ; !"─"
      ; bg green
      ; fg black
      ; bold
      ; !" "
      ; !x
      ; !" "
      ; reset
      ]
;;

let render_links f title list =
  match list with
  | [] ->
    []
  | li ->
    Ansi.[ reset; !"\n\n" ] @ f title li
;;

let render_link_box = render_links Glue.Ui.link_box
let render_dated_link_box = render_links Glue.Ui.dated_link_box

let render_content expanded content =
  match expanded, content with
  | _, None | false, _ ->
    []
  | _, Some x ->
    let label, text =
      match snd x with
      | Shapes.Text.Plain s ->
        "local", s
      | Shapes.Text.File x ->
        (match File.to_string x with
        | Error _ ->
          x, "unreadable"
        | Ok txt ->
          x, txt)
    in
    Ansi.[ reset; !"\n\n" ]
    @ Ansi.text_box ~text_style:Ansi.[ fg bright_blue ] label text
;;

let show_project expanded project =
  let open Shapes.Project in
  let fragment =
    Ansi.[ !"\n" ]
    @ Ansi.(box project.title [ [ fg cyan; !(project.synopsis) ] ])
    @ Ansi.
        [ bg green
        ; fg black
        ; bold
        ; !(Format.sprintf " %s " $ status_to_string project.status)
        ]
    @ license project.license
    @ List.fold_left
        (fun acc (key, value) -> acc @ render_link_box key value)
        []
        project.links
    @ (render_dated_link_box "Releases" $ List.rev project.releases)
    @ render_content expanded project.content
    @ Ansi.[ !"\n" ]
  in
  fragment |> Ansi.to_string ~scoped:true |> print_endline
;;

let show project_name expanded =
  let r =
    let open Bedrock in
    Glue.Log.read_project_updates ()
    |> Validation.from_result
    |> Validation.map Glue.Context.Projects.init
    |> Validation.bind (fun t ->
           fst (Glue.Project.read t (project_name ^ ".qube")))
  in
  match r with
  | Error err ->
    Prompter.prompt_errors err
  | Ok (project, _, _) ->
    show_project expanded project
;;
