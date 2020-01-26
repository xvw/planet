open Bedrock
open Baremetal

let path = Glue.Database.(path galleries)

let try_gallery_creation day kind =
  let name = Glue.Ui.get_string "Name?" "Name of the gallery" in
  let permalink = Glue.Ui.get_string "Permalink?" "Permalink of the gallery" in
  let content =
    Shapes.Text.(Format.Org, File (Filename.concat path (permalink ^ ".org")))
  in
  let description =
    Glue.Ui.get_string "Description?" "Description of the gallery"
  in
  let updated_at = day in
  let tags =
    Glue.Ui.get_string_opt "Tags?" "Tags of the task"
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

let create potential_kind =
  let potential_gallery =
    let open Validation.Syntax in
    let* day = Glue.Util.day () |> Validation.from_result in
    let+ kind = Shapes.Gallery.kind_from_string potential_kind in
    try_gallery_creation day kind
  in
  match potential_gallery with
  | Ok gallery -> Format.printf "%a\n" Shapes.Gallery.pp gallery
  | Error errs -> Prompter.prompt_errors errs
;;
