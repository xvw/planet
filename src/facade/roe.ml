open Js_of_ocaml
open Bedrock
open Error
module Tyxml = Js_of_ocaml_tyxml.Tyxml_js

module Code = struct
  let underbox parent node =
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
      Ok (Dom.appendChild node (Tyxml.To_dom.of_div box))
    else Ok ()
  ;;

  let deal_with parent nodes =
    let open Validation.Infix in
    List.map (underbox parent) (Dom.list_of_nodeList nodes)
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
