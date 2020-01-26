open Bedrock
open Baremetal

let database = Database.galleries
let path = Database.path database

let path_of kind =
  let sub =
    let open Shapes.Gallery in
    match kind with
    | Illustration -> "images/illustrations"
    | Painting -> "images/paintings"
    | Photography -> "images/photographs"
  in
  Filename.concat "." sub
;;

let create gallery =
  let open Shapes.Gallery in
  let permalink = String.lowercase_ascii gallery.permalink in
  let filename = permalink ^ ".qube" in
  let filepath = Filename.concat path filename in
  let qexp = to_qexp gallery in
  let qstr = Paperwork.Qexp.to_string qexp in
  let open Result.Infix in
  File.create filepath qstr
  >>= (fun () ->
        let open Shapes.Text in
        match gallery.content with
        | _, Plain _ -> Ok ()
        | _, File f -> File.create f ("Description de " ^ gallery.name))
  >|= (fun () -> gallery)
  |> Validation.from_result
;;

let update gallery =
  let open Shapes.Gallery in
  let permalink = String.lowercase_ascii gallery.permalink in
  let filename = permalink ^ ".qube" in
  let filepath = Filename.concat path filename in
  let qexp = to_qexp gallery in
  let qstr = Paperwork.Qexp.to_string qexp in
  let open Result.Infix in
  File.overwrite filepath qstr >|= (fun () -> gallery) |> Validation.from_result
;;
