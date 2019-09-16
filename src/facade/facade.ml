open Js_of_ocaml

(* let hydrate uuid =
 *   let open Lwt.Infix in
 *   "Start processing data" |> Lwt.return >|= Console.print
 *   >|= Storage.Session.clear
 *   >|= (fun () -> Storage.Session.set "planet-uuid" uuid)
 *   >>= Binding.Log.hydrate
 *   >|= fun () -> Console.print "Processing data done"
 * ;; *)

let start generation_id_node f =
  let open Bedrock.Validation in
  generation_id_node |> Js.Opt.to_option
  |> from_option (Of "Unable to find generation-id")
  >>= (fun x ->
        Attr.Data.(x.%{"uuid"})
        |> from_option (Of "Unable to find data-uuid"))
  |> (function
       | Ok uuid ->
         let () = Console.print ("Planet is started with " ^ uuid) in
         (match Storage.Session.get "planet-uuid" with
         | None ->
           (* hydrate uuid *)
           Lwt.return_unit
         | Some pred_uuid when uuid <> pred_uuid ->
           (* hydrate uuid *)
           Lwt.return_unit
         | _ ->
           Lwt.return_unit)
       | Error errs ->
         Lwt.return (Console.render_error errs))
  |> fun promise ->
  Dom_html.window##.onload
  := Dom.handler (fun _ ->
         let open Lwt.Infix in
         let _ =
           Lwt.finalize
             (fun () -> promise)
             (fun () -> Lwt.return (f ()))
         in
         Js._true)
;;

let () =
  Js.export
    "planet"
    (object%js (self)
       val internal =
         object%js
           val loadTasks = Js.array [||]
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
                  let () = callback () in
                  Js.Unsafe.fun_call task [||])
                (fun () -> ())
         in
         start nodes f

       val project = Widget.Project.api

       val location = Widget.Location.api

       val story = Widget.Story.api

       val common = Widget.Common.api

       val roe = Roe.api
    end)
;;
