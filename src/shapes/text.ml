open Bedrock
open Paperwork
open Error

module Format = struct
  type t =
    | Org
    | Markdown
    | Raw

  let to_string = function
    | Org ->
      "org"
    | Markdown ->
      "markdown"
    | Raw ->
      "raw"
  ;;

  let from_string str =
    match String.lowercase_ascii str with
    | "raw" ->
      Ok Raw
    | "markdown" | "md" ->
      Ok Markdown
    | "org" | "orgmode" | "org-mode" ->
      Ok Org
    | x ->
      Error (Unknown_format x)
  ;;
end

type content =
  | File of string
  | Plain of string

let patch_flag str x f =
  match String.lowercase_ascii str with
  | "text" | "plain" | "internal" | "intern" ->
    Ok (f, Plain x)
  | "file" | "extern" | "external" ->
    Ok (f, File x)
  | _ ->
    Error [Invalid_text_scheme]
;;

type t = Format.t * content

let fetch table field =
  match Hashtbl.find_opt table field with
  | None ->
    Error [Undefined_field field]
  | Some (Some Qexp.(Node children)) ->
    (match children with
    | Qexp.([ (String (_, flag) | Keyword flag | Tag flag | Atom flag)
            ; (String (_, fmt) | Keyword fmt | Tag fmt | Atom fmt)
            ; String (_, content) ]) ->
      let open Validation.Infix in
      Format.from_string fmt |> Validation.from_result
      >>= patch_flag flag content
    | _ ->
      Error [Invalid_field field])
  | _ ->
    Error [Invalid_field field]
;;

let pp ppf (fmt, content) =
  let ext = Format.to_string fmt in
  let kind =
    match content with
    | File k ->
      Stdlib.Format.sprintf "file://%s" k
    | Plain k ->
      Stdlib.Format.sprintf "`%s`" k
  in
  Stdlib.Format.fprintf ppf "Text.%s=%s" ext kind
;;

let eq_format left right =
  let open Format in
  match left, right with
  | Org, Org | Markdown, Markdown | Raw, Raw ->
    true
  | _ ->
    false
;;

let eq_content left right =
  match left, right with
  | File x, File y | Plain x, Plain y ->
    x = y
  | _ ->
    false
;;

let eq (a, b) (x, y) = eq_format a x && eq_content b y
