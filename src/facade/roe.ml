open Js_of_ocaml
open Bedrock
open Util
open Error
module Tyxml = Js_of_ocaml_tyxml.Tyxml_js

module Code = struct
  let prepare parent node =
    let () = Dom.removeChild parent node in
    let open Tyxml.Html in
    let subparent =
      div
        ~a:[ a_class [ "code-container" ] ]
        [ div
            ~a:[ a_class [ "code-concrete-container" ] ]
            [ Tyxml.Of_dom.of_div node ]
        ]
      |> Tyxml.To_dom.of_div
    in
    let () = Dom.appendChild parent subparent in
    Ok (subparent, node)
  ;;

  let underbox parent (subparent, node) =
    if Attr.Data.(parent.?{"pellet"} || parent.?{"file"})
    then
      let open Tyxml.Html in
      let pellet =
        Attr.Data.(parent.%{"pellet"})
        |> Option.map (fun pellet ->
               [ div ~a:[ a_class [ "code-pellet" ] ] [ txt pellet ] ])
        |> Option.get_or (fun () -> [])
      in
      let file =
        Attr.Data.(parent.%{"file"})
        |> Option.map (fun file ->
               [ div ~a:[ a_class [ "code-file" ] ] [ txt file ] ])
        |> Option.get_or (fun () -> [])
      in
      let box =
        div ~a:[ a_class [ "code-underbox" ] ] (pellet @ file)
      in
      let () = Dom.appendChild subparent (Tyxml.To_dom.of_div box) in
      Ok (subparent, node)
    else Ok (subparent, node)
  ;;

  let line_number parent (subparent, node) =
    if Attr.Data.(parent.?{"line-number"})
    then (
      let l =
        (parent##querySelectorAll (Js.string ".sourceLine"))##.length
      in
      let open Tyxml.Html in
      let offset =
        Attr.Data.(parent.%{"line-start"})
        |> Option.bind int_of_string_opt
        |> Option.get_or (fun () -> 1)
      in
      let box =
        div
          ~a:[ a_class [ "code-line-number" ] ]
          (List.init l (fun i ->
               div [ txt $ Format.asprintf "%d" (offset + i) ]))
        |> Tyxml.To_dom.of_div
      in
      let receiver =
        subparent##querySelector
          (Js.string "div.code-concrete-container")
        |> Js.Opt.to_option
        |> Option.get_or (fun () -> subparent)
      in
      let () = Dom.appendChild receiver box in
      Ok (subparent, node))
    else Ok (subparent, node)
  ;;

  let deal_with parent nodes =
    let open Validation.Infix in
    let list = Dom.list_of_nodeList nodes in
    List.map
      (fun node ->
        node |> prepare parent >>= line_number parent
        >>= underbox parent)
      list
    |> Validation.Applicative.sequence >> Ok ()
  ;;
end

let mount container =
  container##querySelectorAll (Js.string ".roe")
  |> Dom.list_of_nodeList
  |> List.map (fun node ->
         Attr.Data.(node.%{"kind"})
         |> Validation.from_option (Of "Unable to find kind")
         |> Validation.bind (function
                | "code" ->
                  Code.deal_with
                    node
                    (node##querySelectorAll
                       (Js.string "div.sourceCode"))
                | kind ->
                  Error
                    [ Of (Format.asprintf "Unknown kind [%s]" kind) ]))
  |> Validation.Applicative.sequence
  |> function
  | Ok _ ->
    Console.print "ROE is mounted"
  | Error errs ->
    Console.render_error errs
;;

let api =
  object%js
    method mount container = mount container
  end
;;
