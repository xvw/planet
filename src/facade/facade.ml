open Js_of_ocaml
module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events

let start _generation_id_node f =
  let open Bedrock.Validation in
  (* generation_id_node |> Js.Opt.to_option
   * |> from_option (Of "Unable to find generation-id")
   * >>= (fun x ->
   *       Attr.Data.(x.%{"uuid"})
   *       |> from_option (Of "Unable to find data-uuid"))
   * |> (function
   *      | Ok uuid ->
   *        let () = Console.print ("Planet is started with " ^ uuid) in
   *        Lwt.return_unit
   *      | Error errs ->
   *        Lwt.return (Console.render_error errs))
   * |> fun promise -> *)
  Lwt.Infix.(
    Lwt.return (Console.log "Await for planet")
    >>= Lwt_js_events.onload
    >>= (fun _ev -> f ())
    >|= fun () -> Console.log "Planet launched")
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

       method start nodes =
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
         start nodes f

       val project = Widget.Project.api

       val location = Widget.Location.api

       val story = Widget.Story.api

       val common = Widget.Common.api

       val diary = Widget.Diary.api

       val roe = Roe.api
    end)
;;
