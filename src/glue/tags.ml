open Bedrock

let date_or = function
  | Some x -> Ok x
  | None ->
    Paperwork.Timetable.Day.from_string "019A01" |> Validation.from_result
;;

let galleries bucket =
  let open Validation.Infix in
  Gallery.get ()
  >>= (fun x -> List.map Gallery.read x |> Validation.Applicative.sequence)
  >|= fun galleries ->
  List.fold_left
    (fun bucket gallery ->
      let open Shapes.Gallery in
      Shapes.Tag.add
        bucket
        gallery.name
        (kind_to_string gallery.kind)
        gallery.permalink
        gallery.updated_at
        gallery.description
        gallery.tags)
    bucket
    galleries
;;

let stories bucket =
  let open Validation.Infix in
  Story.collect ()
  >|= fun stories ->
  List.fold_left
    (fun bucket story ->
      let open Shapes.Story in
      match story.kind, story.published with
      | Long, true ->
        Shapes.Tag.add
          bucket
          story.title
          "long"
          story.permaname
          story.date
          story.synopsis
          story.tags
      | _, _ -> bucket)
    bucket
    stories
;;

let projects bucket =
  let open Validation.Infix in
  Project.all ()
  >>= fun (_, projects) ->
  let open Shapes.Project in
  List.fold_left
    (fun pbucket (project, day, _) ->
      pbucket
      >>= fun bucket ->
      if project.published && project.indexed
      then
        date_or day
        >|= fun date ->
        Shapes.Tag.add
          bucket
          project.title
          "project"
          project.name
          date
          project.synopsis
          project.tags
      else Ok bucket)
    (Ok bucket)
    projects
;;

let to_json () =
  let open Validation.Infix in
  Ok (Shapes.Tag.new_bucket ())
  >>= stories
  >>= projects
  >>= galleries
  >|= Shapes.Tag.sort
  >|= Shapes.Tag.to_json
;;
