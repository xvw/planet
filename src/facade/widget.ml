open Js_of_ocaml
open Paperwork
module Tyxml = Js_of_ocaml_tyxml.Tyxml_js
module Svg = Tyxml.Svg
module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events

let d ?(u = `Px) value = value, Some u

module Resume = struct
  open Util

  let resize = watch Lwt_js_events.onresize
  let make_storage_key path = "--planet-scroll-position:" ^ path

  let get_size = function
    | None ->
      let h = document##.documentElement##.scrollHeight in
      let c = document##.documentElement##.clientHeight in
      float_of_int (h - c)
    | Some x ->
      let c = document##.documentElement##.clientHeight in
      float_of_int (offset_y x - c)
  ;;

  let find_pred_h offset =
    let h = Js.string "h1, h2, h3, h4, h5, h6" in
    let nodes =
      document##querySelectorAll h |> Dom.list_of_nodeList
    in
    nodes
    |> List.filter (fun node -> offset_y node < offset)
    |> List.sort (fun a b -> compare (offset_y b) (offset_y a))
    |> function x :: _ -> Some (Js.to_string x##.id) | [] -> None
  ;;

  let jump_to elt _ev _ =
    let offset = max 0 (offset_y elt - 20) in
    let () = window##scroll 0 offset in
    Lwt.return_unit
  ;;

  let compute_progress percent x =
    let p = min (int_of_float (percent *. 100.0)) 100 in
    let px = string_of_int p ^ "%" in
    x##.style##.width := Js.string px
  ;;

  let perform_ui key resume jump =
    match Bedrock.Option.(Storage.Local.get key >>= get_by_id) with
    | None ->
      Dom.removeChild resume jump
    | Some elt ->
      let _ = Lwt_js_events.(async_loop click jump (jump_to elt)) in
      ()
  ;;

  let handle resume progress jump path eof =
    let path = Js.to_string path in
    let key = make_storage_key path in
    let last_tick = ref 0. in
    let document_size = ref (get_size eof) in
    let () = perform_ui key resume jump in
    let () = compute_progress 0.0 progress in
    let _ =
      let open Lwt_js_events in
      let _ =
        resize () (fun _ ->
            document_size := get_size eof;
            compute_progress !last_tick progress)
      in
      seq_loop scroll window (fun target _ ->
          let real_scroll = scroll_y () in
          let scroll = float_of_int real_scroll in
          let raw_percent = scroll /. !document_size in
          let percent =
            if raw_percent >= 95.0 then 100.0 else raw_percent
          in
          let pred_percent = !last_tick in
          let abs_percent = abs_float (percent -. pred_percent) in
          let diff = abs_percent *. !document_size in
          let () =
            Console.log
              (object%js
                 val percent = percent

                 val scroll = scroll

                 val pc = abs_percent

                 val diff = diff

                 val size = !document_size
              end)
          in
          let () =
            if diff > 25.0
            then (
              let () = last_tick := percent in
              let () =
                match find_pred_h real_scroll with
                | Some "" | None ->
                  Storage.Local.remove key
                | Some id ->
                  Storage.Local.set key id
              in
              compute_progress percent progress)
          in
          request_animation_frame ())
    in
    ()
  ;;
end

open Bedrock
open Error
open Util

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

  let api =
    object%js
      method timeAgo nodes = time_ago_for nodes
    end
  ;;
end

module Graph = struct
  let calendar
      ?(prefix_id = "calendar-box-")
      ?(classes = [])
      ?(end_date = Calendar.now ())
      ?(gutter = 2)
      width
    =
    let w = float_of_int width in
    let g = float_of_int gutter in
    let start_date = Calendar.years_ago 1 end_date in
    let cell_w = (w -. (52.0 *. g)) /. 53.0 in
    let h = (cell_w *. 7.0) +. (7.0 *. g) in
    let boxes =
      let rec aux offx offy acc date =
        let ts = date##valueOf in
        if ts >= end_date##valueOf
        then acc
        else (
          let y = float_of_int offx *. (cell_w +. g) in
          let x = float_of_int offy *. (cell_w +. g) in
          let rect =
            Svg.(
              rect
                ~a:
                  [ a_id
                      (Format.asprintf
                         "%s%04d-%02d-%02d"
                         prefix_id
                         date##getFullYear
                         date##getMonth
                         (date##getDate + 1))
                  ; a_x $ d x
                  ; a_y $ d y
                  ; a_rx $ d 2.
                  ; a_ry $ d 2.
                  ; a_width $ d cell_w
                  ; a_height $ d cell_w
                  ; a_fill $ `Color ("#FFF", None)
                  ]
                [])
          in
          let succ_date = new%js Js.date_fromTimeValue ts in
          let _ = succ_date##setDate (date##getDate + 1) in
          aux
            (succ offx mod 7)
            (if offx >= 6 then succ offy else offy)
            (rect :: acc)
            succ_date)
      in
      aux 0 0 [] start_date
    in
    Tyxml.Html.(
      svg
        ~a:
          Svg.
            [ a_viewBox (0., 0., w, h)
            ; a_width $ d ~u:`Pt w
            ; a_height $ d ~u:`Pt h
            ; a_class ("calendar-graph" :: classes)
            ])
      boxes
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
        @ Common.render_tags Shapes.Project.(project.tags)
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
      method path : Js.js_string Js.t Js.readonly_prop

      method eof : Dom_html.element Js.t Js.Opt.t Js.readonly_prop

      method story :
        Dom_html.textAreaElement Js.t Js.Opt.t Js.readonly_prop

      method rightContainer :
        Dom_html.element Js.t Js.Opt.t Js.readonly_prop

      method bottomContainer :
        Dom_html.element Js.t Js.Opt.t Js.readonly_prop
    end

  let resume_handler =
    let open Tyxml.Html in
    let progress = div ~a:[ a_class [ "progress" ] ] [] in
    let jump = a ~a:[ a_class [ "button" ] ] [ txt "Reprendre" ] in
    let b =
      div
        ~a:[ a_class [ "resume-box" ] ]
        [ h3 [ span [ txt "Progression" ] ]
        ; div ~a:[ a_class [ "progress-bar" ] ] [ progress ]
        ; jump
        ; a
            ~a:[ a_class [ "button" ]; a_href "/index.html" ]
            [ txt "Index" ]
        ]
    in
    b, progress, jump
  ;;

  let render_summary right_container bottom_container story =
    let open Tyxml.Html in
    let resume_box, progress, jump = resume_handler in
    let () =
      Dom.appendChild
        right_container
        (Tyxml.To_dom.of_div resume_box)
    in
    let right_content =
      div (Common.render_tags story.Shapes.Story.tags)
      |> Tyxml.To_dom.of_div
    in
    let () = Dom.appendChild right_container right_content in
    let () =
      Common.render_links bottom_container story.Shapes.Story.links
    in
    resume_box, progress, jump
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
      let resume, progress, jump =
        render_summary right_container bottom_container story
      in
      let tdom = Tyxml.To_dom.of_div in
      let tdoma = Tyxml.To_dom.of_a in
      Resume.handle
        (tdom resume)
        (tdom progress)
        (tdoma jump)
        input##.path
        (Js.Opt.to_option input##.eof)
    | Error errs ->
      Console.render_error errs
  ;;

  let api =
    object%js
      method boot input = boot input
    end
  ;;
end

module Location = struct
  class type boot_input =
    object
      method locationBox :
        Dom_html.element Js.t Js.Opt.t Js.readonly_prop
    end

  let walk_logs (now, current, logs) =
    let open Tyxml.Html in
    div
      (List.map
         (fun (log, country, city) ->
           let dt =
             Calendar.(Ago.compute ~in_day:true (from_day log))
           in
           let st = Calendar.Ago.stringify dt in
           let chead =
             if Timetable.Day.cmp now log < 0
             then "future"
             else if Timetable.Day.cmp now log > 0
             then "past"
             else "present"
           in
           let attr =
             match current with
             | None ->
               [ a_class [ chead ] ]
             | Some x ->
               if Timetable.Day.eq log x
               then
                 [ a_class [ "current"; chead ]
                 ; a_id "current-location"
                 ]
               else [ a_class [ chead ] ]
           in
           div
             ~a:attr
             [ div
                 ~a:[ a_class [ "moment" ] ]
                 [ txt $ Format.asprintf "%a" Timetable.Day.ppr log ]
             ; div ~a:[ a_class [ "since" ] ] [ txt st ]
             ; div ~a:[ a_class [ "country" ] ] [ txt country ]
             ; div ~a:[ a_class [ "city" ] ] [ txt city ]
             ])
         logs)
  ;;

  let collect_current_location (now, acc, r) (l, _, _) =
    if Timetable.Day.cmp l now <= 0
    then (
      match acc with
      | None ->
        now, Some l, r
      | Some x ->
        now, Some (if Timetable.Day.cmp x l < 0 then l else x), r)
    else now, acc, r
  ;;

  let handle_location = function
    | Ok logs ->
      (match Calendar.(to_day (now ())) with
      | Ok n ->
        logs
        |> List.fold_left collect_current_location (n, None, logs)
        |> walk_logs
        |> fun x -> logs, x
      | Error err ->
        Console.render_error [ err ];
        [], Tyxml.Html.div [])
    | Error errs ->
      Console.render_error errs;
      [], Tyxml.Html.div []
  ;;

  let boot input =
    match
      Validation.Infix.(
        id
        <$> validate
              "unable to find location container"
              input##.locationBox)
    with
    | Ok location_box ->
      let open Lwt.Infix in
      Binding.Location.get () >|= handle_location
      >|= (fun (logs, d) ->
            Dom.appendChild location_box (Tyxml.To_dom.of_div d);
            logs)
      |> ignore
    | Error errs ->
      Console.render_error errs
  ;;

  let api =
    object%js
      method boot input = boot input
    end
  ;;
end
