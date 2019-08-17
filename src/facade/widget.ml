open Js_of_ocaml
open Bedrock
open Error
open Paperwork
open Util
module Tyxml = Js_of_ocaml_tyxml.Tyxml_js
module Svg = Tyxml.Svg

let d ?(u = `Px) value = value, Some u

let render_error errors =
  errors |> List.iter (to_string %> Js.string %> Console.error)
;;

let validate str optional_node =
  optional_node |> Js.Opt.to_option
  |> Validation.from_option (Of str)
;;

let get_data f elt key =
  elt##getAttribute (Js.string $ Format.asprintf "data-%s" key)
  |> Js.Opt.to_option
  |> Validation.from_option (Of "Unable to find sector data")
  |> Validation.bind f
;;

module Sector = struct
  let nodelist_to_hashtbl node_list =
    let () = Console.log node_list in
    node_list |> Dom.list_of_nodeList
    |> List.map (fun node ->
           let open Validation.Infix in
           Shapes.Sector.make
           <$> get_data (fun x -> Ok (Js.to_string x)) node "name"
           <*> get_data (fun x -> Ok (Js.to_string x)) node "desc"
           <*> get_data
                 (Js.to_string %> Color.from_string
                %> Validation.from_result)
                 node
                 "color"
           >|= fun sector -> sector.name, sector)
    |> Validation.Applicative.sequence
    |> Validation.map (List.to_seq %> Hashtbl.of_seq)
  ;;
end

module Project = struct
  class type boot_input =
    object
      method timedata :
        Dom_html.textAreaElement Js.t Js.Opt.t Js.readonly_prop

      method project :
        Dom_html.textAreaElement Js.t Js.Opt.t Js.readonly_prop

      method rightContainer :
        Dom_html.element Js.t Js.Opt.t Js.readonly_prop

      method bottomContainer :
        Dom_html.element Js.t Js.Opt.t Js.readonly_prop

      method sectors :
        Dom_html.element Dom.nodeList Js.t Js.readonly_prop
    end

  let render_releases = function
    | [] ->
      []
    | releases ->
      let len = List.length releases in
      let open Tyxml.Html in
      [ div
          ~a:[ a_class [ "project-block"; "release-list" ] ]
          [ h3
              [ span [ txt "Releases" ]
              ; span
                  ~a:[ a_class [ "label" ] ]
                  [ txt $ string_of_int len ]
              ]
          ; ul
              (List.map
                 (fun (name, date, url) ->
                   li
                     [ span
                         ~a:[ a_class [ "date" ] ]
                         [ txt
                           $ Format.asprintf
                               "%a"
                               Paperwork.Timetable.Day.ppr
                               date
                         ]
                     ; a ~a:[ a_href url ] [ txt name ]
                     ])
                 releases)
          ]
      ]
  ;;

  let render_tags = function
    | [] ->
      []
    | tags ->
      let len = List.length tags in
      let open Tyxml.Html in
      [ div
          ~a:[ a_class [ "project-block"; "tag-list" ] ]
          [ h3
              [ span [ txt "Tags" ]
              ; span
                  ~a:[ a_class [ "label" ] ]
                  [ txt $ string_of_int len ]
              ]
          ; ul (List.map (fun tag -> li [ txt tag ]) tags)
          ]
      ]
  ;;

  let render_links links =
    List.fold_left
      (fun acc (section_name, links) ->
        match links with
        | [] ->
          []
        | links ->
          let open Tyxml.Html in
          acc
          @ [ div
                ~a:[ a_class [ "project-block"; "link-list" ] ]
                [ h3 [ span [ txt section_name ] ]
                ; ul
                    (List.map
                       (fun (name, url) ->
                         li [ a ~a:[ a_href url ] [ txt name ] ])
                       links)
                ]
            ])
      []
      links
  ;;

  let collect_data textarea_timedata =
    let open Validation.Infix in
    textarea_timedata |> Js.Opt.to_option
    |> Validation.from_option (Of "Unable to find time metadata")
    >>= fun textarea ->
    textarea##.textContent
    |> validate "Content of textarea is malformed"
    >>= fun text ->
    Js.to_string text |> Paperwork.Qexp.from_string
    |> Validation.from_result
    >>= Shapes.Context.Projects.project_from_qexp
  ;;

  let create_data_block key value =
    let open Tyxml.Html in
    li
      [ span ~a:[ a_class [ "label" ] ] [ txt key ]
      ; span ~a:[ a_class [ "data" ] ] [ txt value ]
      ]
  ;;

  let render_start_date = function
    | None ->
      []
    | Some start_date ->
      [ create_data_block "Démarrage"
        $ Format.asprintf
            "~%a"
            Paperwork.Timetable.Day.ppr
            start_date
      ]
  ;;

  let sectors_legend total sectors counters _len =
    let size = 10. in
    List.mapi
      (fun ik (sector_name, duration) ->
        let i = float_of_int ik in
        let c =
          Hashtbl.find_opt sectors sector_name
          |> Option.map (fun x -> Shapes.Sector.(x.color))
          |> Option.get_or (fun () -> Color.create 255 0 0)
        in
        let pc =
          float_of_int duration /. float_of_int total *. 100.
        in
        let open Svg in
        [ rect
            ~a:
              [ a_x $ d 0.
              ; a_y $ d ((size +. 2.) *. i)
              ; a_width $ d size
              ; a_height $ d size
              ; a_rx $ d 2.
              ; a_ry $ d 2.
              ; a_fill $ `Color (Color.to_hex c, None)
              ]
            []
        ; text
            ~a:
              [ a_x_list [ d $ size +. 4. ]
              ; a_y_list [ d $ ((size +. 2.) *. i) +. 8. ]
              ; a_text_anchor `Start
              ]
            [ txt
              $ Format.asprintf "%s (%03.2f%s)" sector_name pc "%"
            ]
        ])
      counters
    |> List.flatten
  ;;

  let render_sector_graph total sectors hash_counters =
    let counters =
      hash_counters |> Hashtbl.to_seq
      |> Seq.filter (fun (k, v) -> v > 0)
      |> List.of_seq
      |> List.sort (fun (_, a) (_, b) -> compare b a)
    in
    let len = List.length counters in
    Tyxml.Html.svg
      ~a:
        Svg.
          [ a_viewBox (0., 0., 200., 80.)
          ; a_class [ "tracking-graph" ]
          ]
      (sectors_legend total sectors counters len)
  ;;

  let render_timedata data sectors =
    match collect_data data with
    | Error errs ->
      let () = render_error errs in
      []
    | Ok ctx ->
      let open Tyxml.Html in
      let open Shapes.Context.Projects in
      let hours =
        let duration = float_of_int ctx.minuts_counter in
        let minuts = duration /. 60.0 in
        minuts
      in
      [ div
          ~a:[ a_class [ "project-block"; "tracking" ] ]
          [ h3 [ span [ txt "Suivi" ] ]
          ; ul
              ~a:[ a_class [ "stats" ] ]
              (render_start_date ctx.start_date
              @ [ create_data_block "Logs"
                  $ Format.asprintf
                      "%d entrée%s"
                      ctx.logs_counter
                      (if ctx.logs_counter > 1 then "s" else "")
                ; create_data_block "Durée"
                  $ Format.asprintf
                      "~%0.1f heure%s"
                      hours
                      (if hours >= 2.0 then "s" else "")
                ])
          ]
      ; render_sector_graph
          ctx.minuts_counter
          sectors
          ctx.sectors_counters
      ]
  ;;

  let render_summary
      right_container
      bottom_container
      project
      timedata
      sectors
    =
    let open Tyxml.Html in
    let right_content =
      div
        (render_timedata timedata sectors
        @ render_tags Shapes.Project.(project.tags)
        @ render_releases Shapes.Project.(List.rev project.releases)
        )
      |> Tyxml.To_dom.of_div
    in
    let () = Dom.appendChild right_container right_content in
    match Shapes.Project.(project.links) with
    | [] ->
      ()
    | _ :: _ ->
      let bottom_content =
        div
          ~a:[ a_class [ "list-of-links" ] ]
          (render_links Shapes.Project.(project.links))
        |> Tyxml.To_dom.of_div
      in
      Dom.appendChild bottom_container bottom_content
  ;;

  let validate_project node =
    let open Validation.Infix in
    node
    |> validate "unable to find project metadata"
    >>= (fun textarea ->
          textarea##.textContent
          |> validate "unable to find meta data for project")
    >|= Js.to_string
    >>= Qexp.from_string %> Validation.from_result
    >>= Shapes.Project.from_qexp
  ;;

  let boot input =
    let open Validation.Infix in
    let () = Console.log input##.sectors in
    match
      (fun x y z a -> x, y, z, a)
      <$> validate
            "unable to find right container"
            input##.rightContainer
      <*> validate
            "unable to find bottom container"
            input##.bottomContainer
      <*> validate_project input##.project
      <*> Sector.nodelist_to_hashtbl input##.sectors
    with
    | Ok (right_container, bottom_container, project, sectors) ->
      render_summary
        right_container
        bottom_container
        project
        input##.timedata
        sectors
    | Error errs ->
      render_error errs
  ;;

  let api =
    object%js
      method boot input = boot input
    end
  ;;
end
