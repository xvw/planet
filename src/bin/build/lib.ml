open Bedrock
open Bedrock.Util
open Baremetal

let site_folder = "./_site"

let soft_creation folder =
  let open Result.Infix in
  if not (Dir.exists folder)
  then Dir.make folder >> Ok (true, folder)
  else Ok (false, folder)
;;

let soft_deletion folder =
  let open Result.Infix in
  if Dir.exists folder
  then Dir.delete folder >> Ok (true, folder)
  else Ok (false, folder)
;;

let init () = soft_creation site_folder
let clean () = soft_deletion site_folder

let trace message = function
  | Error err ->
    Prompter.prompt_error err
  | Ok (x, filename) ->
    (if x
    then
      Ansi.
        [ fg green
        ; text $ Format.sprintf "[%s] has beed %s" filename message
        ]
    else
      Ansi.
        [ fg yellow
        ; text $ Format.sprintf "[%s] Nothing to do" filename ])
    |> Ansi.to_string |> print_endline
;;

let trace_creation = trace "created"
let trace_deletion = trace "deleted"
let clean_generated () = trace_deletion (clean ())
let generate () = trace_creation (init ())

let logs () =
  let () = generate () in
  ()
;;
