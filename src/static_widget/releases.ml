open Tyxml.Html
open Bedrock.Util

let cut = function
  | a :: b :: c :: _ -> [ a; b; c ]
  | xs -> xs
;;

let to_html_div repo releases =
  let len = List.length releases in
  let cutted = cut releases in
  [ div
      ~a:[ a_class [ "project-block"; "release-list" ] ]
      ([ h3
           [ span [ txt "Releases" ]
           ; span ~a:[ a_class [ "label" ] ] [ txt $ string_of_int len ]
           ]
       ; ul
           (List.map
              (fun (name, date, url) ->
                li
                  [ span
                      ~a:[ a_class [ "date" ] ]
                      [ txt
                        $ Format.asprintf "%a" Paperwork.Timetable.Day.ppr date
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

let to_html_string repo = function
  | [] -> ""
  | releases ->
    let pp =
      Format.(pp_print_list ~pp_sep:pp_force_newline (Tyxml.Html.pp_elt ()))
    in
    releases |> to_html_div repo |> Format.asprintf "%a" pp
;;
