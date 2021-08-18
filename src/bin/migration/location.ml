open Bedrock
open Bedrock.Util
open Baremetal
open Paperwork
open Error

let log_folder = Glue.Database.(path logs)
let file = Filename.concat log_folder "whereami.qube"

let fold (opt, acc) (dp, _, city) =
  let s x =
    Format.asprintf "%a" Timetable.Day.ppr x
    |> String.split_on_char '-'
    |> String.concat "/"
  in
  let d1 = Stdlib.Option.fold ~none:"" ~some:s opt in
  Some dp, (d1, s dp, city) :: acc
;;

let pcmp f a b =
  let r = f a b in
  if r = 0 then -1 else r
;;

let get_data ?(reverse = true) () =
  let cmp (a, _, _) (b, _, _) = Timetable.Day.cmp a b in
  let sorter = if reverse then flip $ pcmp cmp else pcmp cmp in
  let open Result.Infix in
  file
  |> File.to_stream (fun _ -> Qexp.from_stream)
  >>= Qexp.extract_root
  >|= List.map (function
          | Qexp.Node
              [ Qexp.Keyword daypoint
              ; Qexp.String (_, country)
              ; Qexp.String (_, city)
              ] ->
            daypoint
            |> Timetable.Day.from_string
            >|= fun dp -> dp, country, city
          | x -> Error (Invalid_field (Qexp.to_string x)))
  >>= Result.Applicative.sequence
  >|= List.sort sorter
  >|= List.fold_left fold (None, [])
;;

let () =
  let index =
    (try int_of_string_opt Sys.argv.(1) with
    | _ -> Some 0)
    |> Stdlib.Option.value ~default:0
  in
  match get_data () with
  | Error _ -> ()
  | Ok (_, x) ->
    List.iteri
      (fun i (d1, d, city) ->
        if i >= index
        then (
          let () = Format.printf "%04d - %s -> %s : %s\n" i d d1 city in
          let cmd =
            Shell.(
              command
              $ Format.asprintf "echo \"%s\" | xclip -selection clipboard" d)
              []
          in
          let _ = Shell.run cmd in
          let _ = read_line () in
          ()))
      x
;;
