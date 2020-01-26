open Bedrock
open Baremetal
open Error
module Ui = Glue.Ui

let path = Glue.Gallery.path

let qube name =
  let right = String.lowercase_ascii name ^ ".qube" in
  Filename.concat path right
;;

let resolve_or_abort gallery =
  if Prompter.yes_no
       ~answer_style:Ansi.[ fg yellow ]
       ~title:"Confirm?"
       (Format.asprintf "%a" Shapes.Gallery.pp gallery)
  then Ok gallery
  else Error [ Of "Aborted" ]
;;

let read_gallery name =
  let open Validation.Infix in
  File.to_stream (fun _ -> Paperwork.Qexp.from_stream) (qube name)
  |> Validation.from_result
  >>= Shapes.Gallery.from_qexp
;;

let check_image gallery image =
  let path = Glue.Gallery.path_of Shapes.Gallery.(gallery.kind) in
  let imagepath = Filename.concat path image in
  if File.exists imagepath then Ok imagepath else Error [ Unreadable imagepath ]
;;

let get_place gallery =
  let open Shapes.Gallery in
  match gallery.kind with
  | Illustration -> None
  | _ ->
    let open Option.Syntax in
    let* country = Ui.get_string_opt "Place?" "Country" in
    let+ city = Ui.get_string_opt "Place?" "City" in
    country, city
;;

let attach_metadata day gallery imagepath =
  let name = Ui.get_string "Name?" "Name of the picture" in
  let description = Ui.get_string "Description?" "Description of the picture" in
  let pdate = Ui.get_day_opt "Date?" "When the picture was made" in
  let date = Option.(pdate <!> fun () -> day) in
  let tools =
    Ui.get_string_opt "Tools?" "Tools of the picture"
    |> Option.to_list
    |> List.bind (String.tokenize ',')
  in
  let tags =
    Ui.get_string_opt "Tags?" "Tags of the picture"
    |> Option.to_list
    |> List.bind (String.tokenize ',')
  in
  let place = get_place gallery in
  Ok
    (Shapes.Picture.new_picture
       name
       description
       date
       tools
       tags
       place
       imagepath
       None)
;;

let attach name image =
  let sort a b =
    let open Shapes.Picture in
    Paperwork.Timetable.Day.cmp b.date a.date
  in
  let result =
    let open Validation.Syntax in
    let* day = Glue.Util.day () |> Validation.from_result in
    let* gallery = read_gallery name in
    let* imagepath = check_image gallery image in
    let* imagedata = attach_metadata day gallery imagepath in
    let* list = Ok (imagedata :: Shapes.Gallery.(gallery.pictures)) in
    let* slist = Ok (List.sort sort list) in
    let* new_gallery =
      Ok Shapes.Gallery.{ gallery with pictures = slist; updated_at = day }
    in
    let* _ = resolve_or_abort new_gallery in
    Glue.Gallery.update new_gallery
  in
  match result with
  | Ok _ -> print_endline "done"
  | Error errs -> Prompter.prompt_errors errs
;;

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
    Ui.get_string_opt "Tags?" "Tags of the gallery"
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
  | Ok _ -> print_endline "done"
  | Error errs -> Prompter.prompt_errors errs
;;
