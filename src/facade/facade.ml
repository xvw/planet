open Js_of_ocaml
module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events

let start f =
  let open Bedrock.Validation in
  Lwt.Infix.(
    Lwt.return (Console.print "Await for planet")
    >>= Lwt_js_events.onload
    >>= (fun _ev -> f ())
    >|= fun () -> Console.print "Planet launched")
;;

let () =
  Js.export
    "planet"
    (object%js (self)
       val internal =
         object%js
           val loadTasks = Js.array [||]

           method isoWeek date = Calendar.iso_week date

           method yearsAgo years date = Calendar.years_ago years date
         end

       method suspend f =
         let _ = self##.internal##.loadTasks##push f in
         ()

       method start =
         let f =
           self##.internal##.loadTasks
           |> Js.to_array
           |> Array.fold_left
                (fun callback task () ->
                  let open Lwt.Infix in
                  callback ()
                  >|= fun () -> Js.Unsafe.fun_call task [||])
                (fun () -> Lwt.return_unit)
         in
         start f

       val project = Widget.Project.api

       val location = Widget.Location.api

       val story = Widget.Story.api

       val common = Widget.Common.api

       val diary = Widget.Diary.api

       val roe = Roe.api
    end)
;;
