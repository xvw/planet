open Bedrock
open Util
open Paperwork
open Error

type kind =
  | Photography
  | Illustration
  | Painting

type t =
  { name : string
  ; permalink : string
  ; content : Text.t
  ; description : string
  ; updated_at : Timetable.Day.t
  ; tags : string list
  ; kind : kind
  ; pictures : Picture.t list
  }

let new_gallery name permalink content description updated_at tags kind pictures
  =
  { name; permalink; content; description; updated_at; tags; kind; pictures }
;;

let kind_to_string = function
  | Photography -> "photography"
  | Illustration -> "illustration"
  | Painting -> "painting"
;;

let kind_from_string str =
  match String.lowercase_ascii str with
  | "photography" -> Ok Photography
  | "illustration" -> Ok Illustration
  | "painting" -> Ok Painting
  | _ -> Error [ Of (Format.asprintf "Unknown Gallery Kind [%s]" str) ]
;;

let to_qexp gallery =
  let open Qexp in
  node
    [ kv "name" gallery.name
    ; kv "permalink" gallery.permalink
    ; node [ tag "content"; Text.to_qexp gallery.content ]
    ; kv "description" gallery.description
    ; kv "updated_at" $ Timetable.Day.to_string gallery.updated_at
    ; node [ tag "tags"; node $ List.map string gallery.tags ]
    ; kv "kind" $ kind_to_string gallery.kind
    ; node [ tag "pictures"; node $ List.map Picture.to_qexp gallery.pictures ]
    ]
;;

module Fetch = Table.Fetch
module Mapper = Table.Mapper

let from_qexp expr =
  match Table.configuration expr with
  | Ok config ->
    let open Validation.Infix in
    new_gallery
    <$> Fetch.string config "name"
    <*> Fetch.string config "permalink"
    <*> Text.fetch config "content"
    <*> Fetch.string config "description"
    <*> Fetch.day config "updated_at"
    <*> Fetch.list_refutable Mapper.string config "tags"
    <*> Fetch.token kind_from_string config "kind"
    <*> Fetch.list Picture.from_qexp config "pictures"
  | Error _ as e -> Validation.from_result e
;;

let pp ppf gallery =
  let qexp = to_qexp gallery in
  Format.fprintf ppf "%a" Qexp.pp qexp
;;

let eq left right =
  String.equal left.name right.name
  && String.equal left.permalink right.permalink
  && Text.eq left.content right.content
  && String.equal left.description right.description
  && Timetable.Day.eq left.updated_at right.updated_at
  && List.eq String.equal left.tags right.tags
  && String.equal (kind_to_string left.kind) (kind_to_string right.kind)
  && List.eq Picture.eq left.pictures right.pictures
;;
