open Bedrock
open Baremetal

let prompt_errors ?(intro = true) errors =
  let () =
    if intro
    then
      Ansi.[foreground red; bold; text "Errors are occured:"]
      |> Ansi.to_string ~scoped:true
      |> print_endline
  in
  let () =
    List.iter
      (fun error -> Format.printf "  - %s@." (Error.to_string error))
      errors
  in
  ()
;;

let prompt_error ?(intro = true) error = prompt_errors ~intro [error]
