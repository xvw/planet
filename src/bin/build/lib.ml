open Bedrock
open Bedrock.Util
open Baremetal

let site_folder = "./_site"

let init () =
  let open Result.Infix in
  if not (Dir.exists site_folder)
  then Dir.make site_folder >> Ok true
  else Ok false
;;

let clean () =
  let open Result.Infix in
  if Dir.exists site_folder
  then Dir.delete site_folder >> Ok true
  else Ok false
;;

let clean_generated () =
  match clean () with
  | Error err ->
    Prompter.prompt_error err
  | Ok x ->
    (if x
    then
      Ansi.
        [ fg green
        ; text $ Format.sprintf "[%s] has beed deleted" site_folder
        ]
    else Ansi.[fg yellow; text "Nothing to do"])
    |> Ansi.to_string |> print_endline
;;

let generate () =
  match init () with
  | Error err ->
    Prompter.prompt_error err
  | Ok x ->
    (if x
    then
      Ansi.
        [ fg green
        ; text $ Format.sprintf "[%s] has beed created" site_folder
        ]
    else Ansi.[fg yellow; text "Nothing to do"])
    |> Ansi.to_string |> print_endline
;;
