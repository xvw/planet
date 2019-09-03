open Bedrock
open Util
open Error
open Paperwork

type kind =
  | Long
  | Short

type t =
  { url : string
  ; title : string
  ; synopsis : string
  ; links : (string * Link.simple list) list
  ; content : Text.t
  ; published : bool
  ; related_project : string option
  ; category : string
  ; tags : string list
  ; date : Timetable.Day.t
  ; kind : kind
  }

let kind_from_string str =
  match String.lowercase_ascii str with
  | "long" ->
    Ok Long
  | "short" ->
    Ok Short
  | x ->
    Error [ Unknown_kind x ]
;;

let kind_to_string = function Long -> "long" | Short -> "short"

let new_story
    url
    title
    synopsis
    links
    content
    published
    related_project
    category
    tags
    date
    kind
  =
  { url
  ; title
  ; synopsis
  ; links
  ; content
  ; published = (match published with None -> true | Some x -> x)
  ; related_project
  ; category
  ; tags
  ; date
  ; kind
  }
;;

module Fetch = Table.Fetch

let from_qexp expr =
  match Table.configuration expr with
  | Error _ as e ->
    Validation.from_result e
  | Ok config ->
    let open Validation.Infix in
    new_story
    <$> Fetch.string config "url"
    <*> Fetch.string config "title"
    <*> Fetch.string config "synopsis"
    <*> Fetch.ziplist_refutable Link.mapper_simple config "links"
    <*> Text.fetch config "content"
    <*> Fetch.(option bool config "published")
    <*> Fetch.(option string config "related_project")
    <*> Fetch.string config "category"
    <*> Fetch.list_refutable Table.Mapper.string config "tags"
    <*> Fetch.token
          (Timetable.Day.from_string %> Validation.from_result)
          config
          "date"
    <*> Fetch.token kind_from_string config "kind"
;;

let to_qexp story =
  let open Qexp in
  [ kv "url" story.url
  ; kv "title" story.title
  ; kv "synopsis" story.synopsis
  ]
  @ Kv.ziplist "links" story.links Link.simple_to_qexp
  @ Kv.content (Some story.content)
  @ [ kv
        ~v:atom
        "published"
        (if story.published then "true" else "false")
    ]
  @ Kv.option story.related_project "related_project" id
  @ [ kv "category" story.category ]
  @ Kv.list "tags" story.tags string
  @ [ kv ~v:keyword "date" (Timetable.Day.to_string story.date)
    ; kv ~v:keyword "kind" (kind_to_string story.kind)
    ]
  |> node
;;

let pp ppf story =
  Format.fprintf
    ppf
    "Story(%a - %s : %s)"
    Timetable.Day.pp
    story.date
    (kind_to_string story.kind)
    story.title
;;

let kind_eq a b =
  match a, b with Long, Long | Short, Short -> true | _ -> false
;;

let eq a b =
  a.title = b.title && a.synopsis = b.synopsis && a.url = b.url
  && a.category = b.category
  && Timetable.Day.eq a.date b.date
  && kind_eq a.kind b.kind
  && List.eq
       (fun (k, v) (k2, v2) -> k = k2 && List.eq Link.eq_simple v v2)
       a.links
       b.links
  && Text.eq a.content b.content
  && Option.eq ( = ) a.related_project b.related_project
  && a.published = b.published
  && List.eq ( = ) a.tags b.tags
;;

let to_json story =
  let open Json in
  obj
    [ "title", string story.title
    ; "url", string story.url
    ; "category", string story.category
    ; "date", string (Timetable.Day.to_string story.date)
    ; "kind", string (kind_to_string story.kind)
    ; "synopsis", string story.synopsis
    ; ( "related_project"
      , nullable Option.(story.related_project >|= string) )
    ; "published", bool story.published
    ; "tags", array $ List.map string story.tags
    ; ( "links"
      , obj
          (List.map
             (fun (k, v) ->
               k, array $ List.map Link.simple_to_json v)
             story.links) )
    ]
;;
