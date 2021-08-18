open Js_of_ocaml
open Paperwork
module U = Util
module Tyxml = Js_of_ocaml_tyxml.Tyxml_js
module Svg = Tyxml.Svg
module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events

let d ?(u = `Px) value = value, Some u
let clear node = node##.innerHTML := Js.string ""

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
    let nodes = document##querySelectorAll h |> Dom.list_of_nodeList in
    nodes
    |> List.filter (fun node -> offset_y node < offset)
    |> List.sort (fun a b -> compare (offset_y b) (offset_y a))
    |> function
    | x :: _ -> Some (Js.to_string x##.id)
    | [] -> None
  ;;

  let jump_to elt _ev _ =
    let offset = max 0 (offset_y elt - 80) in
    let () = window##scroll 0 offset in
    Lwt.return_unit
  ;;

  let compute_progress percent x =
    let p = min (int_of_float (percent *. 100.0)) 100 in
    let px = string_of_int p ^ "%" in
    x##.style##.width := Js.string px
  ;;

  let handle resume progress path eof =
    let path = Js.to_string path in
    let key = make_storage_key path in
    let last_tick = ref 0. in
    let document_size = ref (get_size eof) in
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
          let percent = if raw_percent >= 95.0 then 100.0 else raw_percent in
          let pred_percent = !last_tick in
          let abs_percent = abs_float (percent -. pred_percent) in
          let diff = abs_percent *. !document_size in
          let () =
            if diff > 25.0
            then (
              let () = last_tick := percent in
              let () =
                match find_pred_h real_scroll with
                | Some "" | None -> Storage.Local.remove key
                | Some id -> Storage.Local.set key id
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
  optional_node |> Js.Opt.to_option |> Validation.from_option (Of str)
;;

let get_data f elt key =
  Attr.Data.(elt.%{key})
  |> Validation.from_option (Of "Unable to find sector data")
  |> Validation.bind f
;;

module Common = struct
  let create_data_block ?(classes = []) key value =
    let open Tyxml.Html in
    li
      [ span ~a:[ a_class [ "label" ] ] [ txt key ]
      ; span ~a:[ a_class ([ "data" ] @ classes) ] [ txt value ]
      ]
  ;;

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
        txt_content
        |> Calendar.Ago.stringify
        |> Tyxml.Html.txt
        |> Tyxml.To_dom.of_pcdata
      in
      Dom.appendChild node content
  ;;

  let time_ago_for = Dom.list_of_nodeList %> List.iter compute_time_ago

  let render_links_subsection links =
    List.fold_left
      (fun acc (section_name, links) ->
        match links with
        | [] -> []
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
    | [] -> ()
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
    | [] -> []
    | tags ->
      let len = List.length tags in
      let open Tyxml.Html in
      [ div
          ~a:[ a_class [ "project-block"; "tag-list" ] ]
          [ h3
              [ span [ txt "Tags" ]
              ; span ~a:[ a_class [ "label" ] ] [ txt $ string_of_int len ]
              ]
          ; ul
              (List.map
                 (fun tag ->
                   li
                     [ a
                         ~a:
                           [ a_href ("/tags.html#" ^ String.lowercase_ascii tag)
                           ]
                         [ txt tag ]
                     ])
                 tags)
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
      logs
      (title_box : Dom_html.headingElement Js.t)
      render_logs
    =
    let w = float_of_int width in
    let g = float_of_int gutter in
    let start_date = Calendar.years_ago 1 end_date in
    let _ = end_date##setHours 23 in
    let _ = end_date##setMinutes 59 in
    let _ = end_date##setSeconds 59 in
    let nb_weeks = float_of_int (Calendar.weeks_between end_date start_date) in
    let cell_w = (w -. (nb_weeks *. g)) /. (nb_weeks +. 1.0) in
    let h = (cell_w *. 7.0) +. (7.0 *. g) in
    let ms = end_date##valueOf in
    let boxes =
      let rec aux offx offy acc date =
        let ts = date##valueOf in
        if ms < ts
        then acc
        else (
          let y = float_of_int offx *. (cell_w +. g) in
          let x = float_of_int offy *. (cell_w +. g) in
          let key =
            Format.asprintf
              "%04d-%02d-%02d"
              date##getFullYear
              (succ date##getMonth)
              date##getDate
          in
          let attr_tail =
            match Hashtbl.find_opt logs key with
            | None -> Svg.[ a_class [ "day-box"; "empty" ] ]
            | Some current_logs ->
              Svg.
                [ a_class [ "day-box"; "non-empty" ]
                ; a_onclick (fun _ ->
                      let () = render_logs current_logs in
                      let suffix =
                        if List.length current_logs > 1 then "s" else ""
                      in
                      let label = Format.asprintf "Entrée%s du %s" suffix key in
                      let () = title_box##.innerHTML := Js.string label in
                      true)
                ]
          in
          let rect =
            Svg.(
              rect
                ~a:
                  ([ a_id
                       (Format.asprintf
                          "%s%04d-%02d-%02d"
                          prefix_id
                          date##getFullYear
                          date##getMonth
                          date##getDate)
                   ; a_x $ d x
                   ; a_y $ d y
                   ; a_rx $ d 2.
                   ; a_ry $ d 2.
                   ; a_width $ d cell_w
                   ; a_height $ d cell_w
                   ]
                  @ attr_tail)
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
    node_list
    |> Dom.list_of_nodeList
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

module Stats = struct
  let render_start_date = function
    | None -> []
    | Some start_date ->
      [ Common.create_data_block "Démarrage"
        $ Format.asprintf "~%a" Paperwork.Timetable.Day.ppr start_date
      ]
  ;;

  let render_last_update = function
    | None -> []
    | Some update ->
      let ts = Calendar.from_day update in
      let r = Calendar.Ago.compute ts in
      [ Common.create_data_block ~classes:[ "capitalized" ] "Mise à jour"
        $ Format.asprintf "%s" (Calendar.Ago.stringify r)
      ]
  ;;

  let compute_sectors total sectors hash_counters =
    let counters =
      hash_counters
      |> Hashtbl.to_seq
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
        let pc = float_of_int duration /. float_of_int total *. 100. in
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
              ; a_y_list [ d $ margin +. ((size +. 2.) *. i) +. 10. ]
              ; a_text_anchor `Start
              ]
            [ txt $ Format.asprintf "%s (%03.2f%s)" sector_name percent "%" ]
        ])
      counters
    |> List.flatten
  ;;

  let sectors_charts width counters =
    let h = 10. in
    let open Svg in
    rect
      ~a:
        [ a_x $ d 0.
        ; a_y $ d (h +. 6.0)
        ; a_height $ d h
        ; a_width $ d width
        ; a_fill $ `Color ("#ffffff", None)
        ; a_rx $ d 2.
        ; a_ry $ d 2.
        ]
      []
    :: (List.fold_left
          (fun (x_offset, acc) (sector_name, color, percent) ->
            let rect_width = width *. (percent /. 100.0) in
            let rect =
              let open Svg in
              rect
                ~a:
                  [ a_x $ d x_offset
                  ; a_y $ d (h +. 6.0)
                  ; a_height $ d h
                  ; a_width $ d rect_width
                  ; a_fill $ `Color (Color.to_hex color, None)
                  ]
                []
            in
            x_offset +. rect_width, rect :: acc)
          (0.0, [])
          counters
       |> snd
       |> List.rev)
  ;;

  let render_sector_graph width total sectors hash_counters =
    let counters = compute_sectors total sectors hash_counters in
    let len = List.length counters in
    let flen = float_of_int len in
    let bh = 14.5 in
    let computed_height_sectors = flen *. bh in
    let computed_height_charts = 32.0 in
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

  let render_timedata
      ?(width = 200)
      minuts_counter
      start_date
      last_update
      logs_counter
      sectors
      sectors_counter
    =
    let open Tyxml.Html in
    let hours =
      let duration = float_of_int minuts_counter in
      let minuts = duration /. 60.0 in
      minuts
    in
    [ ul
        ~a:[ a_class [ "stats" ] ]
        (render_start_date start_date
        @ render_last_update last_update
        @ [ Common.create_data_block "Logs"
            $ Format.asprintf
                "%d entrée%s"
                logs_counter
                (if logs_counter > 1 then "s" else "")
          ; Common.create_data_block "Durée"
            $ Format.asprintf
                "~%0.1f heure%s"
                hours
                (if hours >= 2.0 then "s" else "")
          ])
    ; render_sector_graph
        (float_of_int width)
        minuts_counter
        sectors
        sectors_counter
    ]
  ;;
end

module Project = struct
  class type boot_input =
    object
      method timedata : Dom_html.textAreaElement Js.t Js.Opt.t Js.readonly_prop
      method project : Dom_html.textAreaElement Js.t Js.Opt.t Js.readonly_prop
      method rightContainer : Dom_html.element Js.t Js.Opt.t Js.readonly_prop
      method sectors : Dom_html.element Dom.nodeList Js.t Js.readonly_prop
    end

  let collect_data textarea_timedata =
    let open Validation.Infix in
    textarea_timedata
    |> Js.Opt.to_option
    |> Validation.from_option (Of "Unable to find time metadata")
    >>= fun textarea ->
    textarea##.textContent
    |> validate "Content of textarea is malformed"
    >>= fun text ->
    Js.to_string text
    |> Paperwork.Qexp.from_string
    |> Validation.from_result
    >>= Shapes.Context.Projects.project_from_qexp
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

  let render_summary right_container project timedata sectors =
    let open Tyxml.Html in
    let ctn =
      match collect_data timedata with
      | Error errs ->
        let () = Console.render_error errs in
        span []
      | Ok ctx ->
        let title = h3 [ span [ txt "Suivi" ] ] in
        let graph =
          let open Shapes.Context.Projects in
          Stats.render_timedata
            ctx.minuts_counter
            ctx.start_date
            ctx.last_update
            ctx.logs_counter
            sectors
            ctx.sectors_counters
        in
        div ~a:[ a_class [ "project-block"; "tracking" ] ] (title :: graph)
    in
    let () = clear right_container in
    Dom.appendChild right_container (Tyxml.To_dom.of_element ctn)
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
    Lwt.return
      (match
         (fun x y z -> x, y, z)
         <$> validate "unable to find right container" input##.rightContainer
         <*> validate_project input##.project
         <*> Sector.nodelist_to_hashtbl input##.sectors
       with
      | Ok (right_container, project, sectors) ->
        render_summary right_container project input##.timedata sectors
      | Error errs -> Console.render_error errs)
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
      method rightContainer : Dom_html.element Js.t Js.Opt.t Js.readonly_prop
      method resumeDetails : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop
    end

  let resume_handler =
    let open Tyxml.Html in
    let progress = div ~a:[ a_class [ "progress" ] ] [] in
    let b =
      div
        ~a:[ a_class [ "resume-box" ] ]
        [ h3 [ span [ txt "Progression" ] ]
        ; div ~a:[ a_class [ "progress-bar" ] ] [ progress ]
        ]
    in
    b, progress
  ;;

  let render_summary right_container =
    let open Tyxml.Html in
    let resume_box, progress = resume_handler in
    let () = Dom.appendChild right_container (Tyxml.To_dom.of_div resume_box) in
    resume_box, progress
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

  let render_resume path resume_box =
    let p = Js.to_string path in
    let key = Resume.make_storage_key p in
    match
      Bedrock.Option.(
        Storage.Local.get key
        >>= U.get_by_id
        >>= fun elt ->
        elt##.textContent
        |> Js.Opt.to_option
        >|= fun txt -> elt, Js.to_string txt)
    with
    | None -> ()
    | Some (elt, text) ->
      let open Tyxml.Html in
      let link = button [ txt (String.trim text) ] in
      let obj =
        div
          ~a:[ a_class [ "container" ] ]
          [ h3 [ txt "Reprendre la lecture ?" ]
          ; p [ txt "Vous aviez déjà entamé la lecture de cet article." ]
          ; link
          ]
      in
      let _ =
        Lwt_js_events.(
          async_loop click (Tyxml.To_dom.of_button link) (Resume.jump_to elt))
      in
      Dom.appendChild resume_box (Tyxml.To_dom.of_div obj)
  ;;

  let boot input =
    let open Validation.Infix in
    Lwt.return
      (match
         (fun w x -> w, x)
         <$> validate "unable to find right container" input##.rightContainer
         <*> validate
               "unable to find resume detail container"
               input##.resumeDetails
       with
      | Ok (right_container, resume_details) ->
        let resume, progress = render_summary right_container in
        let tdom = Tyxml.To_dom.of_div in
        Resume.handle
          (tdom resume)
          (tdom progress)
          input##.path
          (Js.Opt.to_option input##.eof);
        render_resume input##.path resume_details
      | Error errs -> Console.render_error errs)
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
      method locationBox : Dom_html.element Js.t Js.Opt.t Js.readonly_prop
    end

  let walk_logs (now, current, logs) =
    let open Tyxml.Html in
    div
      (List.map
         (fun (log, country, city) ->
           let dt = Calendar.(Ago.compute ~in_day:true (from_day log)) in
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
             | None -> [ a_class [ chead ] ]
             | Some x ->
               if Timetable.Day.eq log x
               then [ a_class [ "current"; chead ]; a_id "current-location" ]
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
      | None -> now, Some l, r
      | Some x -> now, Some (if Timetable.Day.cmp x l < 0 then l else x), r)
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
        id <$> validate "unable to find location container" input##.locationBox)
    with
    | Ok location_box ->
      let open Lwt.Infix in
      Binding.Location.get ()
      >|= handle_location
      >|= fun (logs, d) ->
      let () = clear location_box in
      Dom.appendChild location_box (Tyxml.To_dom.of_div d)
    | Error errs -> Lwt.return $ Console.render_error errs
  ;;

  let api =
    object%js
      method boot input = boot input
    end
  ;;
end

module Diary = struct
  class type boot_input =
    object
      method context : Dom_html.textAreaElement Js.t Js.Opt.t Js.readonly_prop
      method calendarBox : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop
      method titleBox : Dom_html.headingElement Js.t Js.Opt.t Js.readonly_prop
      method statisticBox : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop
      method entryBox : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop
      method sectors : Dom_html.element Dom.nodeList Js.t Js.readonly_prop
    end

  let validate_context node =
    let open Validation.Infix in
    node
    |> validate "unable to find context metadata"
    >>= (fun textarea ->
          textarea##.textContent |> validate "unable to find context")
    >|= Js.to_string
    >>= Qexp.from_string %> Validation.from_result
    >>= Shapes.Context.context_from_qexp
  ;;

  let render_calendar calendar_box title_box render_logs logs =
    let c = Graph.calendar 800 logs title_box render_logs in
    let () = clear calendar_box in
    Dom.appendChild calendar_box (Tyxml.To_dom.of_element c)
  ;;

  let render_stats ctx sectors statistic_box =
    let open Tyxml.Html in
    let block =
      div
        ~a:[ a_class [ "tracking" ] ]
        (let open Shapes.Context in
        Stats.render_timedata
          ~width:280
          ctx.minuts_counter
          ctx.start_date
          ctx.last_update
          ctx.logs_counter
          sectors
          ctx.sectors_counters)
      |> Tyxml.To_dom.of_div
    in
    let () = clear statistic_box in
    Dom.appendChild statistic_box block
  ;;

  let render_logs sectors container projects logs =
    let open Tyxml.Html in
    let l =
      ul
        ~a:[ a_class [ "logs" ] ]
        (List.map
           (fun log ->
             let open Shapes.Log in
             let sector = Hashtbl.find_opt sectors log.sector in
             let color =
               match sector with
               | Some x -> x.Shapes.Sector.color
               | None -> Color.create 255 0 0
             in
             let proj =
               let open Option.Infix in
               match
                 log.project
                 >>= fun key ->
                 Hashtbl.find_opt projects key >|= fun value -> key, value
               with
               | None -> []
               | Some (key, value) ->
                 [ div
                     ~a:[ a_class [ "log-project" ] ]
                     [ (if value
                       then
                         a
                           ~a:
                             [ a_href $ Format.asprintf "/projects/%s.html" key
                             ]
                           [ txt key ]
                       else span [ txt key ])
                     ]
                 ]
             in
             li
               ~a:[ a_class [ "log" ] ]
               [ div
                   ~a:[ a_class [ "log-header" ] ]
                   (div
                      ~a:[ a_class [ "log-date" ] ]
                      [ txt $ Format.asprintf "%a" Timetable.Day.ppr log.day ]
                   :: proj)
               ; div ~a:[ a_class [ "log-label" ] ] [ txt log.label ]
               ; div
                   ~a:[ a_class [ "log-bottom" ] ]
                   [ div
                       ~a:[ a_class [ "log-sector" ] ]
                       [ div
                           ~a:
                             [ a_class [ "log-sector-pill" ]
                             ; a_style
                               $ Format.asprintf
                                   "background-color: %s;"
                                   (Color.to_hex color)
                             ]
                           []
                       ; span [ txt log.sector ]
                       ]
                   ; div
                       ~a:[ a_class [ "log-duration" ] ]
                       [ txt $ string_of_int log.duration ]
                   ]
               ])
           logs)
    in
    let () = clear container in
    Dom.appendChild container (Tyxml.To_dom.of_ul l)
  ;;

  let boot input =
    match
      Validation.Infix.(
        (fun u v w x y z -> u, v, w, x, y, z)
        <$> validate "unable to find calendar container" input##.calendarBox
        <*> validate "unable to find statistic container" input##.statisticBox
        <*> validate "unable to find entry container" input##.entryBox
        <*> validate "Unable to find title container" input##.titleBox
        <*> Sector.nodelist_to_hashtbl input##.sectors
        <*> validate_context input##.context)
    with
    | Ok (calendar_box, statistic_box, entry_box, title_box, sectors, ctx) ->
      let open Lwt.Infix in
      Binding.Log.get_last_logs ()
      >>= (fun logs ->
            Binding.Project.get () >|= fun projects -> logs, projects)
      >|= (fun (logs, projects) ->
            let () = render_logs sectors entry_box projects logs in
            projects)
      >|= (fun projects ->
            let () = render_stats ctx sectors statistic_box in
            projects)
      >>= (fun projects ->
            Binding.Log.collect () >|= fun logs -> logs, projects)
      >|= fun (logs, projects) ->
      render_calendar
        calendar_box
        title_box
        (render_logs sectors entry_box projects)
        logs
    | Error errs -> Lwt.return (Console.render_error errs)
  ;;

  let api =
    object%js
      method boot input = boot input
    end
  ;;
end

module Tags = struct
  class type boot_input =
    object
      method tagsBox : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop
      method contentBox : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop
    end

  class type random_input =
    object
      method button : Dom_html.buttonElement Js.t Js.Opt.t Js.readonly_prop
    end

  let render_tags container tags =
    let open Tyxml.Html in
    let html_tags =
      List.map
        (fun x -> a ~a:[ a_href ("#" ^ x); a_user_data "tag" x ] [ txt x ])
        tags
    in
    let wildcard =
      a ~a:[ a_href "#*"; a_user_data "tag" "*" ] [ txt "toutes les entrées" ]
    in
    let block = div (wildcard :: html_tags) |> Tyxml.To_dom.of_div in
    let () = clear container in
    Dom.appendChild container block
  ;;

  let href_for content =
    let open Shapes.Tag in
    let open Format in
    match content.section with
    | "project" -> asprintf "/projects/%s.html" content.id
    | "long" -> asprintf "/longs/%s.html" content.id
    | x ->
      let open Shapes.Gallery in
      (match Shapes.Gallery.kind_from_string x with
      | Ok Illustration ->
        asprintf "/galleries/illustrations/%s.html" content.id
      | Ok Photography -> asprintf "/galleries/photographs/%s.html" content.id
      | Ok Painting -> asprintf "/galleries/paintings/%s.html" content.id
      | _ -> "#")
  ;;

  let render_pages container content =
    let open Tyxml.Html in
    let html_content =
      List.map
        (fun x ->
          let open Shapes.Tag in
          let html_tags = List.map (fun x -> span [ txt x ]) x.tags in
          a
            ~a:
              [ a_class [ "page" ]
              ; a_href (href_for x)
              ; a_user_data "tag" (String.concat "," x.tags)
              ; a_user_data "filterable" "true"
              ]
            [ h3
                [ span [ txt (Format.asprintf "%a" Timetable.Day.ppr x.date) ]
                ; span [ txt x.title ]
                ]
            ; p [ txt x.description ]
            ; div ~a:[ a_class [ "tags" ] ] html_tags
            ])
        content
    in
    let block = div html_content |> Tyxml.To_dom.of_div in
    let () = clear container in
    Dom.appendChild container block
  ;;

  let handle_anchor tags_container container =
    let removable = Js.string "remove-elt" in
    let negative = Js.string "negative-tag" in
    let hash =
      let x = String.trim (U.get_hash ()) in
      if String.length x = 0 then "*" else x
    in
    let selector = Js.string "a[data-filterable]" in
    let selector_tags = Js.string (Format.asprintf "a[data-tag]") in
    let nodes = container##querySelectorAll selector |> Dom.list_of_nodeList in
    let tags =
      tags_container##querySelectorAll selector_tags |> Dom.list_of_nodeList
    in
    let () =
      List.iter
        (fun node ->
          match Attr.Data.(node.%{"tag"}) with
          | Some x when String.equal x hash -> node##.classList##add negative
          | _ -> node##.classList##remove negative)
        tags
    in
    let () =
      List.iter
        (fun node ->
          match Attr.Data.(node.%{"tag"}) with
          | Some data_tags ->
            let tagslist =
              String.split_on_char ',' data_tags |> List.map String.trim
            in
            if List.exists (String.equal hash) tagslist
            then node##.classList##remove removable
            else if String.length hash = 0 || String.equal hash "*"
            then node##.classList##remove removable
            else node##.classList##add removable
          | None -> node##.classList##add removable)
        nodes
    in
    ()
  ;;

  let boot input =
    match
      Validation.Infix.(
        (fun a b -> a, b)
        <$> validate "unable to find tags container" input##.tagsBox
        <*> validate "unable to find pages container" input##.contentBox)
    with
    | Ok (tags_container, pages_container) ->
      let open Lwt.Infix in
      Binding.Tags.get ()
      >>= (function
      | Ok bucket ->
        let () = render_tags tags_container Shapes.Tag.(bucket.all_tags) in
        let () = render_pages pages_container Shapes.Tag.(bucket.contents) in
        let () = handle_anchor tags_container pages_container in
        U.watch Lwt_js_events.onhashchange () (fun _ ->
            handle_anchor tags_container pages_container)
      | Error errs -> Lwt.return (Console.render_error errs))
    | Error errs -> Lwt.return (Console.render_error errs)
  ;;

  let random input =
    match
      Validation.Infix.(
        (fun x -> x) <$> validate "unable to find random button" input##.button)
    with
    | Ok btn ->
      Lwt_js_events.(
        async_loop click btn (fun _ _ ->
            let open Lwt.Infix in
            Binding.Tags.get ()
            >>= function
            | Ok bucket ->
              let pages =
                "/"
                :: "/tags.html"
                :: "/galleries.html"
                :: "/journal.html"
                :: "/tasks.html"
                :: "/location.html"
                :: "/xavier.html"
                :: (Shapes.Tag.(bucket.contents) |> List.map href_for)
              in
              let index = Random.int (List.length pages) in
              let sample = List.nth pages index in
              Lwt.return (Js.string sample)
              >|= fun loc -> Dom_html.window##.location##.href := loc
            | Error errs -> Lwt.return (Console.render_error errs)))
    | Error errs -> Lwt.return (Console.render_error errs)
  ;;

  let api =
    object%js
      method boot input = boot input
      method random input = random input
    end
  ;;
end

module Tasks = struct
  class type boot_input =
    object
      method boardBox : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop
    end

  let kl n = "multi-col-board" ^ n

  let render_task_footer_date task =
    let open Shapes.Task in
    let open Tyxml.Html in
    match task.state with
    | InProgress | Opened | Backlog ->
      (match task.engagement_date with
      | Some day ->
        let date = Calendar.from_day day in
        let txtc = Calendar.Ago.compute date in
        let ctnt = txtc |> Calendar.Ago.stringify in
        let k =
          if Calendar.Ago.in_past (snd txtc) then "past" else "not-past"
        in
        [ div
            ~a:[ a_class [ kl "-task-due"; k ] ]
            [ txt $ Format.asprintf "Pour %s" ctnt ]
        ]
      | None -> [])
    | Done ->
      (match task.closing_date with
      | Some day ->
        [ div
            ~a:[ a_class [ kl "-task-due" ] ]
            [ txt
              $ Format.asprintf "Cloturée le %a" Paperwork.Timetable.Day.ppr day
            ]
        ]
      | None -> [])
    | _ -> []
  ;;

  let render_task_footer_project projects task =
    let open Shapes.Task in
    let open Tyxml.Html in
    let open Option.Infix in
    match
      task.project >>= fun k -> Hashtbl.find_opt projects k >|= fun v -> k, v
    with
    | None -> []
    | Some (k, published) ->
      [ div
          ~a:[ a_class [ kl "-task-project" ] ]
          [ (if published
            then
              a ~a:[ a_href $ Format.asprintf "/projects/%s.html" k ] [ txt k ]
            else span [ txt k ])
          ]
      ]
  ;;

  let render_task_footer projects task =
    render_task_footer_date task @ render_task_footer_project projects task
  ;;

  let render_task_body task =
    let open Shapes.Task in
    let open Tyxml.Html in
    let desc = p ~a:[ a_class [ kl "-task-desc" ] ] [ txt task.description ] in
    let togglable ctn = div ~a:[ a_class [ kl "-togglable-body" ] ] ctn in
    let checks =
      div
        ~a:[ a_class [ kl "-task-checks" ] ]
        [ ul
            (List.map
               (fun (checked, label) ->
                 let f = if checked then "checked" else "unchecked" in
                 li
                   ~a:[ a_class [ f ] ]
                   [ span ~a:[ a_class [ "square" ] ] []
                   ; span ~a:[ a_class [ "checklabel" ] ] [ txt label ]
                   ])
               task.checklist)
        ]
    in
    match task.state with
    | Opened | InProgress -> [ desc; togglable [ checks ] ]
    | _ -> [ togglable [ desc; checks ] ]
  ;;

  let render_task projects task =
    let open Shapes.Task in
    let open Tyxml.Html in
    div
      ~a:
        [ a_class [ kl "-task"; kl "-task-closed" ]
        ; a_id task.uuid
        ; a_onclick (fun ev ->
              match Js.Opt.to_option ev##.currentTarget with
              | None -> true
              | Some elt ->
                let cl1 = Js.string (kl "-task-closed") in
                let cl2 = Js.string (kl "-task-opened") in
                let _ = elt##.classList##toggle cl1 in
                let _ = elt##.classList##toggle cl2 in
                let () = Console.print "foo" in
                true)
        ]
      ([ div
           ~a:[ a_class [ kl "-task-header" ] ]
           [ h4 [ span [ txt task.name ]; span [ txt task.uuid ] ] ]
       ]
      @ render_task_body task
      @ [ div
            ~a:[ a_class [ kl "-task-footer" ] ]
            (render_task_footer projects task)
        ])
  ;;

  let render_column projects col_title (total, tasks) =
    let open Tyxml.Html in
    let hidden =
      match tasks with
      | [] -> [ "hidden-column" ]
      | _ -> []
    in
    div
      ~a:[ a_class ([ kl "-column" ] @ hidden) ]
      [ h2
          [ span ~a:[ a_class [ kl "-column-title" ] ] [ txt col_title ]
          ; span
              ~a:[ a_class [ kl "-column-title-total" ] ]
              [ txt $ string_of_int total ]
          ]
      ; div
          ~a:[ a_class [ kl "-tasks" ] ]
          (List.map (render_task projects) tasks)
      ]
  ;;

  let render_board container board projects =
    let open Tyxml.Html in
    let open Shapes.Task in
    let main_board =
      div
        ~a:[ a_class [ kl "-container" ] ]
        [ div
            ~a:[ a_class [ kl "" ] ]
            [ render_column projects "Ouvertes" board.opened
            ; render_column projects "En cours" board.in_progress
            ; render_column projects "Réalisées" board.done_
            ]
        ; div
            ~a:[ a_class [ kl "" ] ]
            [ render_column projects "Accumulées" board.backlog
            ; render_column projects "Bloquées" board.blocked
            ; div ~a:[ a_class [ kl "-column" ] ] []
            ]
        ]
    in
    let () = clear container in
    Dom.appendChild container (main_board |> Tyxml.To_dom.of_div)
  ;;

  let boot input =
    match
      Validation.Infix.(
        (fun x -> x) <$> validate "unable to find board box" input##.boardBox)
    with
    | Ok board_container ->
      let open Lwt.Infix in
      Binding.Tasks.get ()
      >>= (fun tasks ->
            Binding.Project.get () >|= fun projects -> tasks, projects)
      >>= (function
      | Error errs, _ -> Lwt.return (Console.render_error errs)
      | Ok board, projects ->
        Lwt.return (render_board board_container board projects))
    | Error errs -> Lwt.return (Console.render_error errs)
  ;;

  let api =
    object%js
      method boot input = boot input
    end
  ;;
end

module Gallery = struct
  class type boot_input =
    object
      method gallery : Dom_html.textAreaElement Js.t Js.Opt.t Js.readonly_prop
      method container : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop
      method rightContainer : Dom_html.divElement Js.t Js.Opt.t Js.readonly_prop
    end

  let validate_gallery node =
    let open Validation.Infix in
    validate "unable to find gallery metadata" node
    >>= (fun textarea ->
          textarea##.textContent
          |> validate "unable to find metadata for gallery")
    >|= Js.to_string
    >>= Qexp.from_string %> Validation.from_result
    >>= Shapes.Gallery.from_qexp
  ;;

  let render_tags gallery container =
    let open Tyxml.Html in
    let tags = Common.render_tags Shapes.Gallery.(gallery.tags) in
    let () = clear container in
    Dom.appendChild container (Tyxml.To_dom.of_div (div tags))
  ;;

  let render_images gallery container =
    let open Shapes.Gallery in
    let images =
      Tyxml.Html.div
        (List.map
           (fun image ->
             let open Shapes.Picture in
             let open Tyxml.Html in
             div
               ~a:[ a_class [ "picture" ] ]
               [ h4 [ txt image.name ]
               ; a
                   ~a:[ a_href image.image ]
                   [ img ~src:image.image ~alt:image.name () ]
               ; div
                   ~a:[ a_class [ "picture-footer" ] ]
                   [ div
                       ~a:[ a_class [ "description" ] ]
                       [ txt image.description ]
                   ; div
                       ~a:[ a_class [ "published" ] ]
                       [ txt
                           (Format.asprintf
                              "%a"
                              Paperwork.Timetable.Day.ppr
                              image.date)
                       ]
                   ]
               ])
           gallery.pictures)
    in
    let () = clear container in
    Dom.appendChild container (Tyxml.To_dom.of_div images)
  ;;

  let boot input =
    match
      Validation.Infix.(
        (fun a b c -> a, b, c)
        <$> validate "unable to find container" input##.container
        <*> validate "unable to find right container" input##.rightContainer
        <*> validate_gallery input##.gallery)
    with
    | Ok (container, right_container, gallery) ->
      let () = render_tags gallery right_container in
      let () = render_images gallery container in
      Lwt.return_unit
    | Error errs -> Lwt.return (Console.render_error errs)
  ;;

  let api =
    object%js
      method boot input = boot input
    end
  ;;
end
