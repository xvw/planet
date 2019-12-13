open Js_of_ocaml
open Bedrock
open Util
open Error
module Tyxml = Js_of_ocaml_tyxml.Tyxml_js

type range =
  | Single of int
  | Range of (int * int)

let range_of_string obj =
  try
    Scanf.sscanf obj "%d..%d" (fun a b ->
        Some
          (if a < b
          then Range (a, b)
          else if a > b
          then Range (b, a)
          else Single a))
  with
  | _ ->
    obj |> int_of_string_opt |> Option.map (fun x -> Single x)
;;

let range_each f = function
  | Single x ->
    f x
  | Range (a, b) ->
    let rec aux n =
      if n = b
      then f n
      else (
        let () = f n in
        aux (succ n))
    in
    aux a
;;

let range_of_element element key =
  Attr.Data.(element.%{key})
  |> Option.map
       (String.split_on_char ';' %> List.map (String.trim %> range_of_string))
  |> Option.bind Option.Applicative.sequence
;;

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
               let attr =
                 match Attr.Data.(parent.%{"url"}) with
                 | None ->
                   txt file
                 | Some x ->
                   a ~a:[ a_href x ] [ txt file ]
               in
               [ div ~a:[ a_class [ "code-file" ] ] [ attr ] ])
        |> Option.get_or (fun () -> [])
      in
      let box = div ~a:[ a_class [ "code-underbox" ] ] (pellet @ file) in
      let () = Dom.appendChild subparent (Tyxml.To_dom.of_div box) in
      Ok (subparent, node)
    else Ok (subparent, node)
  ;;

  let line_number parent (subparent, node) =
    if Attr.Data.(parent.?{"line-number"})
    then (
      let l = (parent##querySelectorAll (Js.string ".sourceLine"))##.length in
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
        subparent##querySelector (Js.string "div.code-concrete-container")
        |> Js.Opt.to_option
        |> Option.get_or (fun () -> subparent)
      in
      let () = Dom.appendChild receiver box in
      Ok (subparent, node))
    else Ok (subparent, node)
  ;;

  let highlight_lines parent (subparent, node) =
    let offset =
      Attr.Data.(parent.%{"line-start"})
      |> Option.bind int_of_string_opt
      |> Option.get_or (fun () -> 1)
    in
    let ranges = range_of_element parent "hl" |> Option.get_or (fun () -> []) in
    let () =
      List.iter
        (range_each (fun i ->
             let real_index = i - offset + 1 in
             match
               node##querySelector
                 (Js.string
                 $ Format.asprintf ".sourceLine[title='%d']" real_index)
               |> Js.Opt.to_option
             with
             | None ->
               ()
             | Some x ->
               x##.classList##add (Js.string "highlighted")))
        ranges
    in
    Ok (subparent, node)
  ;;

  let deal_with parent nodes =
    let open Validation.Infix in
    let list = Dom.list_of_nodeList nodes in
    List.map
      (fun node ->
        node |> prepare parent >>= line_number parent >>= underbox parent
        >>= highlight_lines parent)
      list
    |> Validation.Applicative.sequence >> Ok ()
  ;;
end

module Quote = struct
  let at = function
    | None ->
      []
    | Some date ->
      let open Tyxml.Html in
      [ div ~a:[ a_class [ "quote-date" ] ] [ span [ txt date ] ] ]
  ;;

  let where node = function
    | None ->
      []
    | Some place ->
      let open Tyxml.Html in
      let t = txt place in
      let child =
        match Attr.Data.(node.%{"url"}) with
        | None ->
          span [ t ]
        | Some x ->
          a ~a:[ a_href x ] [ t ]
      in
      [ div ~a:[ a_class [ "quote-place" ] ] [ child ] ]
  ;;

  let by = function
    | None ->
      []
    | Some author ->
      let open Tyxml.Html in
      [ div ~a:[ a_class [ "quote-author" ] ] [ span [ txt author ] ] ]
  ;;

  let deal_with node =
    let () =
      if Attr.Data.(node.?{"author"} || node.?{"where"} || node.?{"when"})
      then
        let open Tyxml.Html in
        let box =
          div
            ~a:[ a_class [ "quote-underbox" ] ]
            (at Attr.Data.(node.%{"when"})
            @ where node Attr.Data.(node.%{"where"})
            @ by Attr.Data.(node.%{"author"}))
          |> Tyxml.To_dom.of_div
        in
        Dom.appendChild node box
    in
    Ok ()
  ;;
end

let mount container =
  container##querySelectorAll (Js.string "[data-roe-kind]")
  |> Dom.list_of_nodeList
  |> List.map (fun node ->
         Attr.Data.(node.%{"roe-kind"})
         |> Validation.from_option (Of "Unable to find kind")
         |> Validation.bind (function
                | "code" ->
                  Code.deal_with
                    node
                    (node##querySelectorAll (Js.string "pre.sourceCode"))
                | "quote" ->
                  Quote.deal_with node
                | kind ->
                  Error [ Of (Format.asprintf "Unknown kind [%s]" kind) ]))
  |> Validation.Applicative.sequence
  |> function
  | Ok _ ->
    Console.print "ROE is mounted"
  | Error errs ->
    Console.render_error errs
;;

let convert_to_link h =
  let tc = h##.textContent in
  let content = Js.Opt.case tc (const "") Js.to_string in
  let idt = "section-" ^ Js.to_string h##.id in
  let () = h##.innerHTML := Js.string "" in
  let a =
    Tyxml.Html.(a ~a:[ a_href ("#" ^ idt) ] [ txt content ])
    |> Tyxml.To_dom.of_a
  in
  let s =
    Tyxml.Html.(span ~a:[ a_id idt; a_class [ "hidden-anchor" ] ] [])
    |> Tyxml.To_dom.of_span
  in
  Dom.appendChild h s;
  Dom.appendChild h a
;;

let handle_title_links container =
  let nodes_name = Js.string "h1, h2, h3, h4, h5, h6" in
  let nodes = container##querySelectorAll nodes_name |> Dom.list_of_nodeList in
  List.iter convert_to_link nodes
;;

let api =
  object%js
    method mount container =
      let () = mount container in
      handle_title_links container
  end
;;
