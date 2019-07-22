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
  ; updated_at : Timetable.Day.t option
  ; repo : string option
  ; license : string option
  ; tools : Link.simple list
  ; links : Link.simple list
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
    updated_at
    repo
    license
    tools
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
  ; updated_at
  ; repo
  ; license
  ; tools
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
    <*> Fetch.(option day config "updated_at")
    <*> Fetch.(option string config "repo")
    <*> Fetch.(option string config "license")
    <*> Fetch.list_refutable Link.mapper_simple config "tools"
    <*> Fetch.list_refutable Link.mapper_simple config "links"
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

let kvcons key f list = Qexp.[ node (tag key :: List.map f list) ]

let kvcontent = function
  | None ->
    []
  | Some (format, Text.File str) ->
    Qexp.
      [ tag "content"
      ; string "external"
      ; keyword (Text.Format.to_string format)
      ; string str
      ]
  | Some (format, Text.Plain str) ->
    Qexp.
      [ tag "content"
      ; string "internal"
      ; keyword (Text.Format.to_string format)
      ; string str
      ]
;;

let to_qexp project =
  let open Qexp in
  let open Timetable in
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
  @ kvo project.repo "license" id
  @ kvo project.picto "picto" id
  @ kvo project.updated_at "updated_at" Day.to_string
  @ kvo project.repo "repo" id
  @ kvcons "tags" string project.tags
  @ kvcontent project.content
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
  && List.eq Link.eq_simple a.tools b.tools
  && List.eq Link.eq_simple a.links b.links
  && List.eq Link.eq_dated a.releases b.releases
  && status_eq a.status b.status
  && List.eq ( = ) a.tags b.tags
  && Option.eq ( = ) a.picto b.picto
  && a.indexed = b.indexed
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
    ; ( "updated_at"
      , nullable
          Option.(
            project.updated_at >|= Timetable.Day.to_string >|= string)
      )
    ; "repo", nullable Option.(project.repo >|= string)
    ; "license", nullable Option.(project.repo >|= string)
    ; "tools", array $ List.map Link.simple_to_json project.tools
    ; "links", array $ List.map Link.simple_to_json project.links
    ; ( "releases"
      , array $ List.map Link.dated_to_json project.releases )
    ; "status", string $ status_to_string project.status
    ; "tags", array $ List.map string project.tags
    ; "picto", nullable Option.(project.picto >|= string)
    ; "indexed", bool project.indexed
    ; "subprojects", array $ List.map to_json project.subprojects
    ]
;;

let compare_date project1 project2 =
  match project1.updated_at, project2.updated_at with
  | None, None ->
    0
  | Some _, None ->
    -1
  | None, Some _ ->
    1
  | Some x, Some y ->
    Timetable.Day.cmp y x
;;
