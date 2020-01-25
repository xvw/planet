open Bedrock
open Util
open Paperwork

type t =
  { name : string
  ; description : string
  ; date : Timetable.Day.t
  ; tools : string list
  ; tags : string list
  ; place : (string * string) option
  ; image : string
  ; thumbnail : string option
  }

let new_picture name description date tools tags place image thumbnail =
  { name; description; date; tools; tags; place; image; thumbnail }
;;

let qexp_place = function
  | None -> []
  | Some (country, city) ->
    let open Qexp in
    [ node [ tag "place"; node [ string country; string city ] ] ]
;;

let to_qexp picture =
  let open Qexp in
  node
    ([ kv "name" picture.name
     ; kv "description" picture.description
     ; kv "date" $ Timetable.Day.to_string picture.date
     ; node [ tag "tools"; node $ List.map string picture.tools ]
     ; node [ tag "tags"; node $ List.map string picture.tags ]
     ]
    @ qexp_place picture.place
    @ [ kv "image" picture.image ]
    @ Kv.option picture.thumbnail "thumbnail" id)
;;

module Fetch = Table.Fetch
module Mapper = Table.Mapper

let from_qexp expr =
  match Table.configuration expr with
  | Ok config ->
    let open Validation.Infix in
    new_picture
    <$> Fetch.string config "name"
    <*> Fetch.string config "description"
    <*> Fetch.day config "date"
    <*> Fetch.list_refutable Mapper.string config "tools"
    <*> Fetch.list_refutable Mapper.string config "tags"
    <*> Fetch.(option $ map Mapper.(couple string string) $ config $ "place")
    <*> Fetch.string config "image"
    <*> Fetch.(option $ string $ config $ "thumbnail")
  | Error _ as e -> Validation.from_result e
;;

let pp ppf picture =
  let qexp = to_qexp picture in
  Format.fprintf ppf "%a" Qexp.pp qexp
;;

let eq left right =
  String.equal left.name right.name
  && String.equal left.description right.description
  && Timetable.Day.eq left.date right.date
  && List.eq String.equal left.tools right.tools
  && List.eq String.equal left.tags right.tags
  && Option.eq
       (fun (a, b) (x, y) -> String.equal a x && String.equal b y)
       left.place
       right.place
  && String.equal left.image right.image
  && Option.eq String.equal left.thumbnail right.thumbnail
;;
