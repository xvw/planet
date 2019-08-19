open Bedrock
open Paperwork
open Error
open Util

type status =
  | Unceasing
  | Wip
  | Done
  | Paused
  | Interrupted

let status_from_string str =
  match String.lowercase_ascii str with
  | "unceasing" | "continue" ->
    Ok Unceasing
  | "wip"
  | "workinprogress"
  | "work-in-progress"
  | "progress"
  | "onprogress" ->
    Ok Wip
  | "done" | "finished" ->
    Ok Done
  | "paused" | "pause" ->
    Ok Paused
  | "interrupted" | "stopped" | "abandonned" ->
    Ok Interrupted
  | _ ->
    Error [ Unknown_status str ]
;;

type t =
  { name : string
  ; title : string
  ; synopsis : string
  ; repo : Repo.t option
  ; license : string option
  ; links : (string * Link.simple list) list
  ; releases : Link.dated list
  ; status : status
  ; tags : string list
  ; picto : string option
  ; indexed : bool
  ; content : Text.t option
  ; published : bool
  ; subprojects : t list
  }

let new_project
    name
    title
    synopsis
    repo
    license
    links
    releases
    status
    tags
    picto
    indexed
    content
    published
    subprojects
  =
  { name
  ; title
  ; synopsis
  ; repo
  ; license
  ; links
  ; releases
  ; status
  ; tags
  ; picto
  ; indexed = (match indexed with None -> true | Some x -> x)
  ; content
  ; published
  ; subprojects
  }
;;

module Fetch = Table.Fetch

let rec from_qexp expr =
  match Table.configuration expr with
  | Ok config ->
    let open Validation.Infix in
    new_project
    <$> Fetch.string config "name"
    <*> Fetch.string config "title"
    <*> Fetch.string config "synopsis"
    <*> Fetch.(option (map Repo.from_qexp) config "repo")
    <*> Fetch.(option string config "license")
    <*> Fetch.ziplist_refutable Link.mapper_simple config "links"
    <*> Fetch.list_refutable Link.mapper_dated config "releases"
    <*> Fetch.token status_from_string config "status"
    <*> Fetch.list_refutable Table.Mapper.string config "tags"
    <*> Fetch.(option string config "picto")
    <*> Fetch.(option bool config "indexed")
    <*> Fetch.(option Text.fetch config "content")
    <*> Fetch.(bool_refutable config "published")
    <*> Fetch.list_refutable from_qexp config "subprojects"
  | Error _ as e ->
    Validation.from_result e
;;

let status_to_string = function
  | Unceasing ->
    "unceasing"
  | Wip ->
    "wip"
  | Done ->
    "done"
  | Paused ->
    "paused"
  | Interrupted ->
    "interrupted"
;;

let kvo obj k f =
  let open Option.Infix in
  obj >|= (fun x -> Qexp.kv k (f x)) |> Option.to_list
;;

let kvcontent = function
  | None ->
    []
  | Some (format, Text.File str) ->
    Qexp.
      [ node
          [ tag "content"
          ; string "external"
          ; keyword (Text.Format.to_string format)
          ; string str
          ]
      ]
  | Some (format, Text.Plain str) ->
    Qexp.
      [ node
          [ tag "content"
          ; string "internal"
          ; keyword (Text.Format.to_string format)
          ; string str
          ]
      ]
;;

let kvlist key list f =
  let open Qexp in
  [ node [ tag key; node (List.map f list) ] ]
;;

let kvziplist key list f =
  let open Qexp in
  [ node
      [ tag key
      ; node
          (List.map
             (fun (k, v) -> node [ string k; node (List.map f v) ])
             list)
      ]
  ]
;;

let to_qexp project =
  let open Qexp in
  [ kv "name" project.name
  ; kv "title" project.title
  ; kv "synopsis" project.synopsis
  ; kv "status" (status_to_string project.status)
  ; kv
      ~v:atom
      "indexed"
      (if project.indexed then "true" else "false")
  ; kv
      ~v:atom
      "published"
      (if project.published then "true" else "false")
  ]
  @ kvo project.license "license" id
  @ kvo project.picto "picto" id
  @ kvcontent project.content
  @ (project.repo |> Option.map Repo.to_qexp |> Option.to_list)
  @ kvlist "tags" project.tags string
  @ kvziplist "links" project.links Link.simple_to_qexp
  @ kvlist "releases" project.releases Link.dated_to_qexp
  |> node
;;

let pp ppf project =
  Format.fprintf
    ppf
    "Project(%s, ...)<%s>"
    project.name
    (status_to_string project.status)
;;

let status_eq left right =
  match left, right with
  | Unceasing, Unceasing
  | Wip, Wip
  | Done, Done
  | Paused, Paused
  | Interrupted, Interrupted ->
    true
  | _ ->
    false
;;

let rec eq a b =
  a.name = b.name && a.title = b.title && a.synopsis = b.synopsis
  && Option.eq ( = ) a.repo b.repo
  && Option.eq ( = ) a.license b.license
  && List.eq
       (fun (k, v) (k2, v2) -> k = k2 && List.eq Link.eq_simple v v2)
       a.links
       b.links
  && List.eq Link.eq_dated a.releases b.releases
  && status_eq a.status b.status
  && List.eq ( = ) a.tags b.tags
  && Option.eq ( = ) a.picto b.picto
  && a.indexed = b.indexed
  && a.published = b.published
  && Option.eq Text.eq a.content b.content
  && List.eq eq a.subprojects b.subprojects
;;

let rec to_json project =
  let open Json in
  obj
    [ "name", string project.name
    ; "published", bool project.published
    ; "title", string project.title
    ; "synopsis", string project.synopsis
    ; ( "repo"
      , nullable Option.(project.repo >|= Repo.base_url %> string) )
    ; "license", nullable Option.(project.license >|= string)
    ; ( "links"
      , obj
          (List.map
             (fun (k, v) ->
               k, array $ List.map Link.simple_to_json v)
             project.links) )
    ; ( "releases"
      , array $ List.map Link.dated_to_json project.releases )
    ; "status", string $ status_to_string project.status
    ; "tags", array $ List.map string project.tags
    ; "picto", nullable Option.(project.picto >|= string)
    ; "indexed", bool project.indexed
    ; "subprojects", array $ List.map to_json project.subprojects
    ]
;;
