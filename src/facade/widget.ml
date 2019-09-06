open Js_of_ocaml
open Bedrock
open Error
open Paperwork
open Util
module Tyxml = Js_of_ocaml_tyxml.Tyxml_js
module Svg = Tyxml.Svg

let d ?(u = `Px) value = value, Some u

let validate str optional_node =
  optional_node |> Js.Opt.to_option
  |> Validation.from_option (Of str)
;;

let get_data f elt key =
  Attr.Data.(elt.%{key})
  |> Validation.from_option (Of "Unable to find sector data")
  |> Validation.bind f
;;

module Common = struct
  let compute_time_ago node =
    let open Validation.Infix in
    match
      get_data
        (Timetable.Day.from_string %> Validation.from_result)
        node
        "planet-ago"
    with
    | Error errs ->
      let () = node##.classList##add (Js.string "hidden-object") in
      Console.dump_errors node errs
    | Ok min_date ->
      let d = Calendar.from_day min_date in
      let txt_content = Calendar.Ago.compute d in
      let content =
        txt_content |> Calendar.Ago.stringify |> Tyxml.Html.txt
        |> Tyxml.To_dom.of_pcdata
      in
      Dom.appendChild node content
  ;;

  let time_ago_for =
    Dom.list_of_nodeList %> List.iter compute_time_ago
  ;;

  let render_links_subsection links =
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

  let render_links ?(classes = []) container = function
    | [] ->
      ()
    | _ :: _ as links ->
      let open Tyxml.Html in
      let bottom_content =
        div
          ~a:[ a_class ("list-of-links" :: classes) ]
          (render_links_subsection links)
        |> Tyxml.To_dom.of_div
      in
      Dom.appendChild container bottom_content
  ;;

  let api =
    object%js
      method timeAgo nodes = time_ago_for nodes
    end
  ;;
end

module Sector = struct
  let nodelist_to_hashtbl node_list =
    node_list |> Dom.list_of_nodeList
    |> List.map (fun node ->
           let open Validation.Infix in
           Shapes.Sector.make
           <$> get_data (fun x -> Ok x) node "name"
           <*> get_data (fun x -> Ok x) node "desc"
           <*> get_data
                 (Color.from_string %> Validation.from_result)
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

  let cut = function a :: b :: c :: _ -> [ a; b; c ] | xs -> xs

  let render_releases repo = function
    | [] ->
      []
    | releases ->
      let len = List.length releases in
      let cutted = cut releases in
      let open Tyxml.Html in
      [ div
          ~a:[ a_class [ "project-block"; "release-list" ] ]
          ([ h3
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
                  cutted)
           ]
          @ (repo
            |> Option.map (fun r ->
                   a
                     ~a:
                       [ a_class [ "view-releases" ]
                       ; a_href (Shapes.Repo.releases_url r)
                       ]
                     [ txt "Toutes les releases" ])
            |> Option.to_list))
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

  let create_data_block ?(classes = []) key value =
    let open Tyxml.Html in
    li
      [ span ~a:[ a_class [ "label" ] ] [ txt key ]
      ; span ~a:[ a_class ([ "data" ] @ classes) ] [ txt value ]
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

  let render_last_update = function
    | None ->
      []
    | Some update ->
      let ts = Calendar.from_day update in
      let r = Calendar.Ago.compute ts in
      [ create_data_block ~classes:[ "capitalized" ] "Mise à jour"
        $ Format.asprintf "%s" (Calendar.Ago.stringify r)
      ]
  ;;

  let compute_sectors total sectors hash_counters =
    let counters =
      hash_counters |> Hashtbl.to_seq
      |> Seq.filter (fun (k, v) -> v > 0)
      |> List.of_seq
      |> List.sort (fun (_, a) (_, b) -> compare b a)
    in
    List.map
      (fun (sector_name, duration) ->
        let c =
          Hashtbl.find_opt sectors sector_name
          |> Option.map (fun x -> Shapes.Sector.(x.color))
          |> Option.get_or (fun () -> Color.create 255 0 0)
        in
        let pc =
          float_of_int duration /. float_of_int total *. 100.
        in
        sector_name, c, pc)
      counters
  ;;

  let sectors_legend margin counters =
    let size = 10. in
    List.mapi
      (fun ik (sector_name, color, percent) ->
        let i = float_of_int ik in
        let open Svg in
        [ rect
            ~a:
              [ a_x $ d 0.
              ; a_y $ d (margin +. ((size +. 2.) *. i))
              ; a_width $ d size
              ; a_height $ d size
              ; a_rx $ d 2.
              ; a_ry $ d 2.
              ; a_fill $ `Color (Color.to_hex color, None)
              ]
            []
        ; text
            ~a:
              [ a_x_list [ d $ size +. 4. ]
              ; a_y_list [ d $ margin +. ((size +. 2.) *. i) +. 8. ]
              ; a_text_anchor `Start
              ]
            [ txt
              $ Format.asprintf
                  "%s (%03.2f%s)"
                  sector_name
                  percent
                  "%"
            ]
        ])
      counters
    |> List.flatten
  ;;

  let sectors_charts width counters =
    let h = 10. in
    List.mapi
      (fun ik (sector_name, color, percent) ->
        let i = float_of_int ik in
        let rect_width = width *. (percent /. 100.0) in
        let open Svg in
        [ rect
            ~a:
              [ a_x $ d 0.
              ; a_y $ d ((h +. 6.) *. i)
              ; a_height $ d h
              ; a_width $ d width
              ; a_fill $ `Color ("#ffffff", None)
              ; a_rx $ d 2.
              ; a_ry $ d 2.
              ]
            []
        ; rect
            ~a:
              [ a_x $ d 0.
              ; a_y $ d ((h +. 6.) *. i)
              ; a_height $ d h
              ; a_width $ d rect_width
              ; a_fill $ `Color (Color.to_hex color, None)
              ; a_rx $ d 2.
              ; a_ry $ d 2.
              ]
            []
        ])
      counters
    |> List.flatten
  ;;

  let render_sector_graph total sectors hash_counters =
    let counters = compute_sectors total sectors hash_counters in
    let len = List.length counters in
    let flen = float_of_int len in
    let width = 200. in
    let bh = 14.5 in
    let ch = 16.0 in
    let margin = 10.0 in
    let computed_height_sectors = flen *. bh in
    let computed_height_charts = (flen *. ch) +. margin in
    let height = computed_height_charts +. computed_height_sectors in
    Tyxml.Html.svg
      ~a:
        Svg.
          [ a_viewBox (0., 0., width, height)
          ; a_width $ d ~u:`Pt width
          ; a_height $ d ~u:`Pt height
          ; a_class [ "tracking-graph" ]
          ]
      (sectors_charts width counters
      @ sectors_legend computed_height_charts counters)
  ;;

  let render_timedata data sectors =
    match collect_data data with
    | Error errs ->
      let () = Console.render_error errs in
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
              @ render_last_update ctx.last_update
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

  let compute_links project =
    let open Shapes.Project in
    (Option.map
       (fun repo ->
         ( Shapes.Repo.kind repo
         , [ "Page du projet", Shapes.Repo.base_url repo
           ; "Bug tracker", Shapes.Repo.bucktracker_url repo
           ; "Contributeurs", Shapes.Repo.contributors_url repo
           ] ))
       project.repo
    |> Option.to_list)
    @ project.links
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
        @ render_releases
            project.repo
            Shapes.Project.(List.rev project.releases))
      |> Tyxml.To_dom.of_div
    in
    let () = Dom.appendChild right_container right_content in
    let links = compute_links project in
    Common.render_links bottom_container links
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
      Console.render_error errs
  ;;

  let api =
    object%js
      method boot input = boot input
    end
  ;;
end

module Story = struct
  class type boot_input =
    object
      method story :
        Dom_html.textAreaElement Js.t Js.Opt.t Js.readonly_prop

      method rightContainer :
        Dom_html.element Js.t Js.Opt.t Js.readonly_prop

      method bottomContainer :
        Dom_html.element Js.t Js.Opt.t Js.readonly_prop
    end

  let render_summary right_container bottom_container story =
    Common.render_links bottom_container story.Shapes.Story.links
  ;;

  let validate_story node =
    let open Validation.Infix in
    node
    |> validate "unable to find story metadata"
    >>= (fun textarea ->
          textarea##.textContent
          |> validate "unable to find meta data for story")
    >|= Js.to_string
    >>= Qexp.from_string %> Validation.from_result
    >>= Shapes.Story.from_qexp
  ;;

  let boot input =
    let open Validation.Infix in
    match
      (fun x y z -> x, y, z)
      <$> validate
            "unable to find right container"
            input##.rightContainer
      <*> validate
            "unable to find bottom container"
            input##.bottomContainer
      <*> validate_story input##.story
    with
    | Ok (right_container, bottom_container, story) ->
      render_summary right_container bottom_container story
    | Error errs ->
      Console.render_error errs
  ;;

  let api =
    object%js
      method boot input = boot input
    end
  ;;
end
