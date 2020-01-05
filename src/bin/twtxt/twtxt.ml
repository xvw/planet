open Baremetal
open Bedrock
open Bedrock.Error
open Bedrock.Util

let database = Glue.Database.twtxt
let path = Glue.Database.path database

let rec prompt_feeds () =
  let open Result.Infix in
  Dir.children path
  >|= (fun feeds -> None :: List.map (fun x -> Some x) feeds)
  >>= fun feeds ->
  try_until Prompter.repeat_result (fun () ->
      Prompter.choose
        ~answer_style:Ansi.[ fg yellow ]
        ~title:"In which feeds?"
        (function
          | Some x -> `Fixed x
          | None -> `Unfixed)
        (Option.get_or (fun () -> "New feed (or feed query)"))
        (Array.of_list feeds)
        "Select a feeds")
  >>= function
  | `Fixed x -> Ok x
  | `Unfixed ->
    try_until Prompter.repeat_option (fun () ->
        Prompter.string_opt
          ~answer_style:Ansi.[ fg yellow ]
          ~title:"Query?"
          ~f:
            (Option.bind (fun x ->
                 let s = String.trim x in
                 if String.length s = 0 then None else Some s))
          "Write feed query")
    |> (function
    | Some x -> Ok x
    | None -> prompt_feeds ())
;;

let collect_file () =
  let open Result.Infix in
  (if Array.length Sys.argv > 1 then Ok Sys.argv.(1) else prompt_feeds ())
  >|= String.split_on_char '&'
  |> Validation.from_result
  |> Validation.bind (fun feeds ->
         List.map
           (fun feed ->
             let s = String.trim feed in
             if String.length s = 0
             then Error [ Of "invalid feed name" ]
             else Ok (if String.has_extension s "txt" then s else s ^ ".txt"))
           feeds
         |> Validation.Applicative.sequence)
;;

let rec set_message feeds =
  let fds = String.concat ", " feeds in
  try_until Prompter.repeat_option (fun () ->
      Prompter.string_opt
        ~answer_style:Ansi.[ fg yellow ]
        ~title:("Write message in: " ^ fds)
        ~f:
          (Option.bind (fun x ->
               let s = String.trim x in
               if String.length s = 0 then None else Some s))
        "Twtxt")
  |> function
  | Some x -> Ok (feeds, x)
  | None -> set_message feeds
;;

let dump_message (feeds, message) =
  let open Result.Infix in
  Glue.Util.moment_with_sec ()
  >|= fun (date, offset) ->
  let tweet = Shapes.Twtxt.make date offset message in
  let qexp = Paperwork.Qexp.node [ Shapes.Twtxt.to_qexp tweet ] in
  let str = Paperwork.Qexp.to_string qexp |> String.trim in
  List.iter
    (fun feed ->
      let file = Filename.concat path feed in
      let () = print_string ("Process for " ^ file ^ "\t") in
      File.touch file
      >>= (fun () -> File.append file ("\n" ^ str))
      |> function
      | Ok _ ->
        let s = Ansi.(to_string [ fg green; bold; !"done" ]) in
        print_endline s
      | Error e ->
        let s = Ansi.(to_string [ fg red; bold; !"failed" ]) in
        let () = print_endline s in
        Prompter.prompt_error e)
    feeds
;;

let () =
  let open Validation.Infix in
  collect_file ()
  >>= set_message
  >|= dump_message
  |> function
  | Ok _ -> ()
  | Error e -> Prompter.prompt_errors e
;;
