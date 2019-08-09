open Js_of_ocaml
open Bedrock
open Error
open Paperwork
open Util
module Tyxml = Js_of_ocaml_tyxml.Tyxml_js

let render_error errors =
  errors |> List.iter (to_string %> Js.string %> Console.error)
;;

module Project = struct
  class type boot_input =
    object
      method project :
        Dom_html.textAreaElement Js.t Js.Opt.t Js.readonly_prop

      method container :
        Dom_html.element Js.t Js.Opt.t Js.readonly_prop
    end

  let render_tags = function
    | [] ->
      []
    | tags ->
      let len = List.length tags in
      let open Tyxml.Html in
      [ div
          ~a:[ a_class [ "right-section"; "tag-list" ] ]
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

  let render_summary container project =
    let open Tyxml.Html in
    let ui =
      div (render_tags Shapes.Project.(project.tags))
      |> Tyxml.To_dom.of_div
    in
    Dom.appendChild container ui
  ;;

  let validate str optional_node =
    optional_node |> Js.Opt.to_option
    |> Validation.from_option (Of str)
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
      (fun x y -> x, y)
      <$> validate "unable to find container" input##.container
      <*> validate_project input##.project
    with
    | Ok (container, project) ->
      render_summary container project
    | Error errs ->
      render_error errs
  ;;

  let api =
    object%js
      method boot input = boot input
    end
  ;;
end
