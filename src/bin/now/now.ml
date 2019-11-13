let current = Glue.Util.moment ()

let print_simple () =
  match current with
  | Error _ ->
    ()
  | Ok t ->
    Format.printf "%a" Paperwork.Timetable.Moment.pp t
;;

let () = print_simple ()
