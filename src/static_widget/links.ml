open Tyxml.Html

let render_links_subsection links =
  List.fold_left
    (fun acc (section_name, links) ->
      match links with
      | [] -> []
      | links ->
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

let render_links ?(classes = []) links =
  div
    ~a:[ a_class ("list-of-links" :: classes) ]
    (render_links_subsection links)
;;

let to_html_string ?(classes = []) = function
  | [] -> ""
  | links -> render_links ~classes links |> Format.asprintf "%a" (pp_elt ())
;;
