open Baremetal
open Bedrock
open Bedrock.Util

let database = Glue.Database.twtxt

let rec prompt_feeds () =
  let open Result.Infix in
  Dir.children (Glue.Database.path database)
  >|= (fun feeds -> None :: List.map (fun x -> Some x) feeds)
  >>= fun feeds ->
  try_until Prompter.repeat_result (fun () ->
      Prompter.choose
        ~answer_style:Ansi.[ fg yellow ]
        ~title:"In which feeds?"
        (function Some x -> `Fixed x | None -> `Unfixed)
        (Option.get_or (fun () -> "New feed"))
        (Array.of_list feeds)
        "Select a feeds")
  >>= function
  | `Fixed x ->
    Ok x
  | `Unfixed ->
    try_until Prompter.repeat_option (fun () ->
        Prompter.string_opt
          ~answer_style:Ansi.[ fg yellow ]
          ~title:"feeds?"
          ~f:
            (Option.bind (fun x ->
                 let s = String.trim x in
                 if String.length s = 0 then None else Some s))
          "Write feed title")
    |> (function Some x -> Ok x | None -> prompt_feeds ())
;;

let collect_file () =
  let open Result.Infix in
  (if Array.length Sys.argv > 1
  then Ok Sys.argv.(1)
  else prompt_feeds ())
  >|= fun feed ->
  if String.has_extension feed "txt" then feed else feed ^ ".txt"
;;

let () =
  let open Result.Infix in
  collect_file () >|= print_endline
  |> function Ok _ -> () | Error e -> Prompter.prompt_error e
;;
