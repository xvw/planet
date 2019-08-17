open Js_of_ocaml
open Bedrock
open Error

module Deal = struct
  let with_code node = Ok ()
end

let mount container =
  container##querySelectorAll (Js.string ".roe")
  |> Dom.list_of_nodeList
  |> List.map (fun node ->
         Attr.Data.(node.%{"kind"})
         |> Validation.from_option (Of "Unable to find kind")
         |> Validation.bind (function
                | "code" ->
                  Deal.with_code node
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
