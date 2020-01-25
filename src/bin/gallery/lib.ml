open Baremetal

let create potential_kind =
  match Shapes.Gallery.kind_from_string potential_kind with
  | Ok _kind -> ()
  | Error errs -> Prompter.prompt_errors errs
;;
