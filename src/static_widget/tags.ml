open Tyxml.Html
open Bedrock.Util

let tags_as_html_div tags =
  let len = List.length tags in
  div
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
                   ~a:[ a_href ("/tags.html#" ^ String.lowercase_ascii tag) ]
                   [ txt tag ]
               ])
           tags)
    ]
;;

let to_html_string = function
  | [] -> ""
  | tags ->
    tags |> tags_as_html_div |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())
;;
