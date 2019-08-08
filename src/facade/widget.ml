open Js_of_ocaml
open Bedrock
open Error
open Paperwork
open Util

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

  let ensure container project = container, project

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
      ensure
      <$> validate "unable to find container" input##.container
      <*> validate_project input##.project
    with
    | Ok (_container, _project) ->
      Console.print "year"
    | Error errs ->
      render_error errs
  ;;

  let api =
    object%js
      method boot input = boot input
    end
  ;;
end
