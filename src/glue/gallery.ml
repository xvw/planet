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

let get () =
  let open Result.Infix in
  path
  |> Dir.children ~filter:(fun x -> String.has_extension x "qube")
  >|= List.map (Filename.concat path)
  |> Validation.from_result
;;

let read filepath =
  let open Validation.Infix in
  File.to_stream (fun _ -> Paperwork.Qexp.from_stream) filepath
  |> Validation.from_result
  >>= Shapes.Gallery.from_qexp
;;

let fetch_content content =
  let open Shapes.Text in
  let ext = extension_for content in
  match snd content with
  | Plain str -> Ok (ext, str)
  | File filename ->
    let open Result.Syntax in
    let+ content = File.to_string filename in
    ext, content
;;

let to_hakyll gallery =
  let open Result.Syntax in
  let open Shapes.Gallery in
  let+ ext, body = fetch_content gallery.content in
  let sqexp = gallery |> to_qexp |> Paperwork.Qexp.to_string in
  let header =
    Hakyll.(
      join
        [ render_string "title" gallery.name
        ; render_string "permalink" gallery.permalink
        ; render_string "description" gallery.description
        ; render_string "kind" (kind_to_string gallery.kind)
        ; render_string
            "qexp_partial"
            (Format.asprintf "_seeds/partials/%s.qexp.html" gallery.permalink)
        ; may_render_with_format
            ~default:"2019-01-01"
            Paperwork.Timetable.Day.ppr
            "date"
            (Some gallery.updated_at)
        ; may_render_with_format
            ~default:"2019-01-01"
            Paperwork.Timetable.Day.pp
            "date_planet"
            (Some gallery.updated_at)
        ])
  in
  let content = header ^ body in
  gallery, ext, content, Hakyll.textarea "gallery" sqexp
;;
