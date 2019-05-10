open Bedrock
open Util
open Baremetal

let run cmd =
  let status, _result = Shell.run_to_string cmd in
  status |> Shell.capture (fun () -> Ok ())
;;

let stage files =
  let args = List.map Shell.string files in
  let cmd = Shell.(command "git" $ subcommand "add" :: args) in
  run cmd
;;

let commit ?desc message =
  let open Option.Infix in
  let d =
    desc
    >|= (fun x -> Shell.[ flag ~value:(string $ "\n" ^ x) "m" ])
    |> Option.get_or (const [])
  in
  let cmd =
    Shell.(
      command "git"
      $ subcommand "commit" :: flag ~value:(string message) "m" :: d)
  in
  run cmd
;;
