open Bedrock
open Paperwork
open Error

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
    Error [Unknown_status str]
;;

type t =
  { name : string
  ; title : string
  ; synopsis : string
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
  ; subprojects : t list }

let new_project
    name
    title
    synopsis
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
    subprojects =
  { name
  ; title
  ; synopsis
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
  ; subprojects }
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
