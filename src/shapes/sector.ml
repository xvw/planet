open Paperwork
open Bedrock
open Util

type t =
  { name : string
  ; desc : string
  ; color : Color.t
  }

let make name desc color = { name; desc; color }

let to_qexp sector =
  let open Qexp in
  node
    [ node [ tag "name"; string sector.name ]
    ; node [ tag "desc"; string sector.desc ]
    ; node [ tag "color"; string $ Color.to_hex sector.color ]
    ]
;;

module Fetch = Table.Fetch

let from_qexp expr =
  match Table.configuration expr with
  | Ok config ->
    let open Validation.Infix in
    make
    <$> Fetch.token (fun x -> Ok x) config "name"
    <*> Fetch.string config "desc"
    <*> Fetch.color config "color"
  | Error _ as e ->
    Validation.from_result e
;;

let pp ppf sector =
  let color = Format.asprintf "%a" Color.pp sector.color in
  Format.fprintf
    ppf
    "Sector(%s, %s, '%s')"
    sector.name
    color
    sector.desc
;;

let eq a b =
  a.name = b.name && a.desc = b.desc && Color.eq a.color b.color
;;

let to_json sector =
  let open Json in
  obj
    [ "name", string sector.name
    ; "desc", string sector.desc
    ; "color", string $ Color.to_hex sector.color
    ]
;;
