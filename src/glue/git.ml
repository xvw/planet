open Bedrock
open Util
open Baremetal

let git = Shell.command "git"
let ok x = Ok x

let run f cmd =
  let status, result = Shell.run_to_string cmd in
  status |> Shell.capture (fun () -> f result)
;;

let stage files =
  let args = List.map Shell.string files in
  let cmd = Shell.(git $ subcommand "add" :: args) in
  Result.Infix.(run ok cmd >> Ok ())
;;

let commit ?desc message =
  let open Option.Infix in
  let d =
    desc
    >|= (fun x -> Shell.[ flag ~value:(string $ "\n" ^ x) "m" ])
    |> Option.get_or (const [])
  in
  let cmd = Shell.(git $ subcommand "commit" :: flag ~value:(string message) "m" :: d) in
  Result.Infix.(run ok cmd >> Ok ())
;;
