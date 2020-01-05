open Bedrock
open Baremetal
open Paperwork

let database = Database.stories
let folder = Database.path database

let collect () =
  let open Validation in
  Dir.children ~filter:(fun x -> String.has_extension x "qube") folder
  |> from_result
  >>= fun children ->
  List.map
    (fun filename ->
      filename
      |> Filename.concat folder
      |> File.to_stream (fun _ -> Qexp.from_stream)
      |> from_result
      >>= Shapes.Story.from_qexp)
    children
  |> Validation.Applicative.sequence
;;

let fetch_story_content content =
  let open Shapes.Text in
  match content with
  | Plain str -> Ok str
  | File filename -> File.to_string filename
;;

let fetch_story_text story =
  let open Shapes.Story in
  let open Result.Syntax in
  let _, text = story.content in
  let+ content = fetch_story_content text in
  Shapes.Text.extension_for story.content, content
;;

let as_textarea =
  Format.asprintf {|<textarea data-planet-qexp="%s">%s</textarea>|}
;;

let to_hakyll story =
  let open Shapes.Story in
  let open Result.Infix in
  fetch_story_text story
  >|= (fun (extension, content) ->
        let story_qexp =
          story |> Shapes.Story.to_qexp |> Paperwork.Qexp.to_string
        in
        let partial =
          Format.asprintf
            "%s.%s.qexp.html"
            story.permaname
            (Shapes.Story.kind_to_string story.kind)
        in
        let header =
          Hakyll.(
            join
              [ render_string "title" story.title
              ; render_string "main_section" "Tentative"
              ; render_string "permaname" story.permaname
              ; render_string "synopsis" story.synopsis
              ; render_string "description" story.synopsis
              ; render_if "published" story.published
              ; render
                  "date"
                  (Format.asprintf "%a" Timetable.Day.ppr)
                  story.date
              ; may_render "related_project" Util.id story.related_project
              ; render_string "category" story.category
              ; render "kind" Shapes.Story.kind_to_string story.kind
              ; render_string "qexp_partial" ("_seeds/partials/" ^ partial)
              ])
        in
        let final_content = header ^ content in
        story, extension, final_content, partial, as_textarea "story" story_qexp)
  |> Validation.from_result
;;
