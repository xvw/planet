open Bedrock
open Baremetal
open Error
module Ui = Glue.Ui

let path = Glue.Gallery.path

let try_gallery_creation day kind =
  let name = Ui.get_string "Name?" "Name of the gallery" in
  let permalink =
    Ui.get_string "Permalink?" "Permalink of the gallery"
    |> String.lowercase_ascii
  in
  let content =
    Shapes.Text.(Format.Org, File (Filename.concat path (permalink ^ ".org")))
  in
  let description = Ui.get_string "Description?" "Description of the gallery" in
  let updated_at = day in
  let tags =
    Ui.get_string_opt "Tags?" "Tags of the task"
    |> Option.to_list
    |> List.bind (String.tokenize ',')
  in
  let pictures = [] in
  Shapes.Gallery.new_gallery
    name
    permalink
    content
    description
    updated_at
    tags
    kind
    pictures
;;

let resolve_or_abort gallery =
  if Prompter.yes_no
       ~answer_style:Ansi.[ fg yellow ]
       ~title:"Confirm?"
       (Format.asprintf "%a" Shapes.Gallery.pp gallery)
  then Ok gallery
  else Error [ Of "Aborted" ]
;;

let create potential_kind =
  let potential_gallery =
    let open Validation.Infix in
    Glue.Util.day ()
    |> Validation.from_result
    >>= fun day ->
    Shapes.Gallery.kind_from_string potential_kind
    >|= (fun kind -> try_gallery_creation day kind)
    >>= (fun gallery -> resolve_or_abort gallery)
    >>= Glue.Gallery.create
  in
  match potential_gallery with
  | Ok gallery -> Format.printf "%a\n" Shapes.Gallery.pp gallery
  | Error errs -> Prompter.prompt_errors errs
;;
